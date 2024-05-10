use std::cell::RefCell;
use crate::lox::{ast, TokenType};
use crate::lox::ast::LiteralValue;
use crate::lox::ast::expression::{Accept, Assign, AstVisitor, Binary, Call, Expr, Get, Grouping, Literal, Logical, Set, This, Unary, Variable};
use crate::lox::ast::statement::{Accept as StmtAccept, Block, Class, Expression, Function, If, Print, Return, Stmt, StmtVisitor, Var, While};
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;
use std::ptr::addr_of;
use std::rc::Rc;
use std::time::{Instant};
use rustc_hash::{FxHashMap};
use crate::lox::RunString;

type HashMap<K, V> = FxHashMap<K, V>;


pub trait Callable {
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<EvalValue>) -> Result<EvalValue, RuntimeErrorReport>;
    fn arity(&self) -> usize;
    fn to_literal(&self) -> LiteralValue;
    fn to_instance(&self) -> Option<LoxInstanceRef>;
    fn bind(&self, instance: &LoxInstanceRef) -> LValueType;
}

type LValueType = Rc<Box<dyn Callable>>;

#[derive(Clone)]
pub enum EvalValue {
    RValue(LiteralValue),
    LValue(LValueType)
}

impl EvalValue {
    pub fn get_literal(&self) -> LiteralValue {
        match self {
            EvalValue::RValue(x) => {x.clone()}
            EvalValue::LValue(x) => {x.to_literal()}
        }
    }

    pub fn get_callable(&self, line: usize) -> Result<LValueType, Unwinder> {
        match self {
            EvalValue::RValue(_) => {Err(Unwinder::RuntimeError(
                RuntimeErrorReport::new("Can only call functions and classes.", line)))
            }
            EvalValue::LValue(l) => {Ok(l.clone())}
        }
    }
}

impl From<LiteralValue> for EvalValue{
    fn from(value: LiteralValue) -> Self {
        EvalValue::RValue(value)
    }
}

type RunValue = Result<EvalValue, Unwinder>;

// The AST uses pinned allocations so I can generate a hash based on the pointer value,
// And don't need to store a GUID or something else on each AST node
pub type ExprId = usize;

// Environment to store variables at some level of scope
pub struct Environment {
    enclosing: Option<EnvironmentRef>,
    values: HashMap<String, EvalValue>
}

struct BuiltinFunctionTime;

impl Callable for BuiltinFunctionTime {
    fn call(&self, interpreter: &mut Interpreter, _arguments: Vec<EvalValue>) -> Result<EvalValue, RuntimeErrorReport> {
        Ok(EvalValue::RValue(LiteralValue::Number(
            Instant::now().duration_since(interpreter.boot_time).as_micros()as f64 / 1_000_000.0
        )))
    }
    fn arity(&self) -> usize {
        0
    }

    fn to_literal(&self) -> LiteralValue {
        LiteralValue::String(Rc::new("<native fn>".to_string()))
    }

    fn to_instance(&self) -> Option<LoxInstanceRef> {
        None
    }

    fn bind(&self, instance: &LoxInstanceRef) -> LValueType {
        todo!()
    }
}

struct LoxInstance {
    class: LoxClassRef,
    fields: HashMap<RunString, EvalValue>
}

type LoxInstanceRef = Rc<RefCell<LoxInstance>>;

impl LoxInstance {
    pub fn get(instance: &LoxInstanceRef, name: &str) -> Result<EvalValue, Unwinder> {
        let name = Rc::new(name.to_string());
        if let Some(value) =  instance.borrow().fields.get(&name) {
            Ok(value.clone())
        } else {
            if let Some(method) = &instance.borrow().class.find_method(&name) {
                let bound_method = method.bind(instance);
                return Ok(EvalValue::LValue(bound_method));
            }
            Err(Unwinder::error(&format!("Undefined property '{}'.", name), 0))
        }
    }

    pub fn set(&mut self, name: &str, value: EvalValue) {
        let name = Rc::new(name.to_string());
        self.fields.insert(name, value);
    }

    pub fn new(class: &LoxClassRef) -> LoxInstanceRef {
        Rc::new(
            RefCell::new(
                LoxInstance {
                    class: class.clone(),
                    fields: HashMap::default()
                }
            )
        )
    }
}

impl Callable for LoxInstanceRef {
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<EvalValue>) -> Result<EvalValue, RuntimeErrorReport> {
        todo!()
    }

    fn arity(&self) -> usize {
        todo!()
    }

    fn to_literal(&self) -> LiteralValue {
        let name = self.borrow().class.name.clone();
        LiteralValue::String(Rc::new(format!("{} instance", name).to_string()))
    }

    fn to_instance(&self) -> Option<LoxInstanceRef> {
        Some(self.clone())
    }

    fn bind(&self, instance: &LoxInstanceRef) -> LValueType {
        todo!()
    }
}

struct LoxClass {
    name: RunString,
    methods: HashMap<RunString, LValueType>
}

impl LoxClass {
    pub fn new_ref(name: RunString, methods: HashMap<RunString, LValueType>) -> LoxClassRef {
        Rc::new(Box::new(
            LoxClass {
                name,
                methods
            }
        ))
    }

    pub fn find_method(&self, name: &RunString) -> Option<LValueType> {
        if let Some(method) = self.methods.get(name) {
            Some(method.clone())
        } else {
            None
        }
    }
}

type LoxClassRef = Rc<Box<LoxClass>>;

impl Callable for LoxClassRef {
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<EvalValue>) -> Result<EvalValue, RuntimeErrorReport> {
        let instance = LoxInstance::new(self);
        if let Some(initializer) = self.find_method(&Rc::new("init".to_string())) {
            initializer.bind(&instance).call(interpreter, arguments)?;
        }
        Ok(
            EvalValue::LValue(
                Rc::new(
                    Box::new(
                        instance
                    )
                )
            )
        )
    }

    fn arity(&self) -> usize {
        if let Some(initializer) = self.find_method(&Rc::new("init".to_string())) {
            return initializer.arity();
        }
        return 0;
    }

    fn to_literal(&self) -> LiteralValue {
        LiteralValue::String(Rc::new(self.name.to_string()))
    }

    fn to_instance(&self) -> Option<LoxInstanceRef> {
        None
    }

    fn bind(&self, instance: &LoxInstanceRef) -> LValueType {
        todo!()
    }
}

struct LoxFunction {
    name: RunString,
    params: Vec<RunString>,
    body: ast::statement::FuncBody,
    closure: EnvironmentRef,
    is_initializer: bool
}

impl LoxFunction {
    fn new(function: &Function, closure: EnvironmentRef, is_initializer: bool) -> Self {
        let name = if let TokenType::Identifier(n) = &function.name.token_type {
            n.clone()
        } else { panic!("Parser fucked up") };

        let params: Vec<RunString> =
        function.params.iter().map(
            |x| {
                if let TokenType::Identifier(i) = &x.token_type {
                    i.clone()
                }
                else {
                    panic!("Parser fucked up with functions");
                }
            }
        ).collect();

        LoxFunction {
            name,
            params,
            body: function.body.clone(), // Get new reference count
            closure,
            is_initializer
        }
    }
}
impl Callable for LoxFunction {
    fn bind(&self, instance: &LoxInstanceRef) -> LValueType {
        let mut environment = Environment::new(Some(self.closure.clone()));
        environment.borrow_mut().define("this",
                                        EvalValue::LValue(
                                            Rc::new(Box::new(instance.clone()))
                                        ));
        let new_func = LoxFunction {
            name: self.name.clone(),
            params: self.params.clone(),
            body: self.body.clone(), // Get new reference count
            closure: environment,
            is_initializer: self.is_initializer
        };
        Rc::new(Box::new(new_func))
    }

    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<EvalValue>) -> Result<EvalValue, RuntimeErrorReport> {
        let environment = Environment::new(Some(self.closure.clone()));

        for (arg_name, arg_value) in self.params.iter().zip(arguments) {
            environment.borrow_mut().define(arg_name, arg_value);
        }

        //
        let binding = self.body.clone();
        let a = binding.deref();
        let run_result = if let Stmt::Block(x) = a.deref() {
            interpreter.execute_block(&x.statements, environment)
        } else {
            panic!("Somehow not executing a block.  This is a bug")
        };

        if let Err(unwinder) =  run_result {
            match unwinder {
                Unwinder::RuntimeError(err) => {
                    return Err(err);
                }
                Unwinder::ReturnValue(val) => {
                    if self.is_initializer {
                        return Ok(self.closure.borrow().get_at(0, &"this".to_string()).unwrap());
                    }
                    return Ok(val);
                }
            }
        }
        if self.is_initializer {
            Ok(self.closure.borrow().get_at(0, &"this".to_string()).unwrap())
        } else {
            Ok(EvalValue::RValue(LiteralValue::Nil))
        }
    }

    fn arity(&self) -> usize {
        self.params.len()
    }

    fn to_literal(&self) -> LiteralValue {
        LiteralValue::String(Rc::new(format!("<fn {}>", self.name)))
    }

    fn to_instance(&self) -> Option<LoxInstanceRef> {
        None
    }
}

pub type EnvironmentRef = Rc<RefCell<Environment>>;
impl Environment {
    pub fn new(enclosing: Option<EnvironmentRef>) -> EnvironmentRef {
        Rc::new(
            RefCell::new(
                Environment {
                    enclosing,
                    values: HashMap::default()
                }
            )
        )
    }

    pub fn define(&mut self, name: &str, value: EvalValue) {
        self.values.insert(String::from(name), value);
    }

    pub fn assign(
        &mut self,
        name: &String,
        value: &EvalValue) -> Result<(), String> {

        // Check current scope
        if self.values.contains_key(name) {
            self.values.insert(name.clone(), value.clone());
            return Ok(());
        }

        // Search enclosing scopes
        let mut current_env = self.enclosing.clone();
        while let Some(env) = current_env {
            let has_key = env.borrow().values.contains_key(name);
            if has_key {
                env.borrow_mut().values.insert(name.clone(), value.clone());
                return Ok(());
            }
            current_env = env.borrow().enclosing.clone();
        }
        Err(format!("Undefined variable \'{}\'.", &name))
    }

    pub fn get(
        &self,
        name: &String) -> Result<EvalValue, String> {

        // Check current scope
        if let Some(val) = self.values.get(name) {
            return Ok(val.clone());
        }

        // Search enclosing scopes

        let mut current_env = self.enclosing.clone();
        while let Some(env) = current_env {
            if let Some(val) = env.borrow().values.get(name) {
                return Ok(val.clone());
            }
            current_env = env.borrow().enclosing.clone();
        }

        Err(format!("Undefined variable \'{}\'.", &name))
    }

    fn ancestor(&self, distance:usize) ->Option<EnvironmentRef> {
        assert_ne!(distance, 0);
        let mut current_env = self.enclosing.clone();

        for _ in 1..distance{
            if let Some(env) = current_env {
                current_env = env.borrow().enclosing.clone();
            } else {
                panic!("Parser fucked up");
            }
        }

        current_env
    }
    fn get_at(
        &self,
        distance: usize, name: &String) -> Result<EvalValue, String> {

        if distance == 0 {
            // Check current scope
            if let Some(val) = self.values.get(name) {
                return Ok(val.clone());
            }
        }
        else {
            // Search enclosing scopes
            if let Some(val) = self.ancestor(distance) {
                if let Some(t) = val.borrow().values.get(name) {
                    return Ok(t.clone());
                }
                else {
                    panic!("Variable not found")
                }
            }
        }

        Err(format!("Variable  {} is not found", &name).to_string())
    }

    fn assign_at(&mut self, distance: usize, name: &str, value: &EvalValue) {
        if distance == 0 {
            self.values.insert(name.to_string(), value.clone());
        }
        else {
            let a = self.ancestor(distance)
                .expect("The resolver determined this variable exists, it is a bug if not found");
            a.borrow_mut().values.insert(name.to_string(), value.clone());
        }
    }
}

pub struct Interpreter {
    environment: EnvironmentRef,
    globals: EnvironmentRef,
    boot_time: Instant,
    locals: HashMap<ExprId, usize>
}

impl Interpreter {
    pub fn new() -> Self {
        let locals: HashMap<ExprId, usize> = HashMap::default();
        let global_env = Environment::new(None);
        {
            let mut ge = global_env.borrow_mut();
            ge.define("clock", EvalValue::LValue(
                Rc::new(Box::new(BuiltinFunctionTime))
            ));
        }
        Interpreter{environment: global_env.clone(), globals: global_env, boot_time: Instant::now(), locals}
    }

    pub fn resolve(&mut self, expr: ExprId, depth: usize) {
        self.locals.insert(expr, depth);
    }

    fn lookup_variable(&self, name: &String, expr_id: ExprId) -> Result<EvalValue, String> {
        if let Some(distance) = self.locals.get(&expr_id) {
            self.environment.borrow().get_at(*distance, name)
        } else {
            self.globals.borrow().get(name)
        }
    }
}

fn is_truthy(value: &LiteralValue) -> bool {
    match value {
        LiteralValue::Nil => false,
        LiteralValue::Boolean(b) => *b,
        _ => true
    }
}

fn is_equal(x: &LiteralValue, y: &LiteralValue) -> bool
{
    match (x, y) {
        (LiteralValue::Nil, LiteralValue::Nil) => { true}
        (LiteralValue::Nil, _) => { false }
        (_, LiteralValue::Nil) => { false }
        (LiteralValue::Boolean(a), LiteralValue::Boolean(b)) => {a == b}
        (LiteralValue::Number(a), LiteralValue::Number(b)) => {a == b}
        (LiteralValue::String(a), LiteralValue::String(b)) => {a == b}
        (_, _) => {false}
    }
}

fn check_number_operands(line: usize, left: &LiteralValue, right: &LiteralValue) -> Result<(f64, f64), Unwinder> {
    match (left, right) {
        (LiteralValue::Number(l), LiteralValue::Number(r)) => {Ok((*l, *r))}
        _ => Err(Unwinder::error("Operands must be numbers.", line))
    }
}

#[derive(Debug)]
pub struct RuntimeErrorReport {
    pub msg: String,
    pub line: usize
}

impl Display for RuntimeErrorReport {
    fn fmt(&self, _: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl std::error::Error for RuntimeErrorReport {

}

impl RuntimeErrorReport {
    fn new(msg: &str, line: usize) -> Self {
        RuntimeErrorReport {
            msg: msg.to_string(),
            line
        }
    }
}

pub enum Unwinder {
    RuntimeError(RuntimeErrorReport),
    ReturnValue(EvalValue)
}

impl From<RuntimeErrorReport> for Unwinder {
    fn from(value: RuntimeErrorReport) -> Self {
        Unwinder::RuntimeError(
            value
        )
    }
}

impl Unwinder {
    pub fn error(message: &str, line: usize) -> Self {
        Unwinder::RuntimeError(
            RuntimeErrorReport::new(message, line)
        )
    }
}

impl Interpreter {
    fn execute_block(self: &mut Interpreter,
                     statements: &ast::statement::StmtList,
                     environment: EnvironmentRef) -> Result<(), Unwinder> {
        let previous = self.environment.clone();
        self.environment = environment;

        for statement in statements.iter() {
            let res = statement.accept(self);
            if let Err(unwind) = res {
                self.environment = previous;
                match unwind {
                    Unwinder::RuntimeError(e) => {
                        return Err(Unwinder::RuntimeError(e));
                    }
                    Unwinder::ReturnValue(u) => {
                        return Err(Unwinder::ReturnValue(u));
                    }
                }
            }
        }
        self.environment = previous;
        Ok(())
    }
}

impl StmtVisitor<Result<(), Unwinder>> for Interpreter
{
    fn visit_block(&mut self, block: &Block) -> Result<(), Unwinder> {
        let new_env = Environment::new(Some(self.environment.clone()));
        self.execute_block(&block.statements,
                           new_env)?;
        Ok(())
    }

    fn visit_class(&mut self, class: &Class) -> Result<(), Unwinder> {
        let name = if let TokenType::Identifier(name) = &class.name.token_type {
            name
        } else {
            panic!("Parser messed up function token");
        };

        let mut env = self.environment.borrow_mut();
        env.define(name, EvalValue::RValue(LiteralValue::Nil));

        let mut methods: HashMap<RunString, LValueType> = HashMap::default();
        for method in &class.methods {
            let is_initializer = method.name.lexeme == "init";
            let function = LoxFunction::new(
                method,
                self.environment.clone(),
                is_initializer);
            methods.insert(
                Rc::new(method.name.lexeme.clone()),
                Rc::new(Box::new(function))
            );
        }

        let class = LoxClass::new_ref(name.clone(), methods);
        env.assign(name, &EvalValue::LValue(
            Rc::new(Box::new(class))
        )).expect("Failed to assign");
        Ok(())
    }

    fn visit_expression(&mut self, expression: &Expression) -> Result<(), Unwinder> {
        expression.expression.accept(self)?;
        Ok(())
    }

    fn visit_function(&mut self, function: &Function) -> Result<(), Unwinder> {
        let name = if let TokenType::Identifier(name) = &function.name.token_type {
            name
        } else {
            panic!("Parser messed up function token");
        };

        let new_func = EvalValue::LValue(
            Rc::new(
                Box::new(LoxFunction::new(function, self.environment.clone(), false))
            )
        );

        self.environment.borrow_mut().define(
            name,
            new_func
        );
        Ok(())
    }

    fn visit_if(&mut self, ifstmt: &If) -> Result<(), Unwinder> {
        if is_truthy(&ifstmt.condition.accept(self)?.get_literal()) {
            ifstmt.then_branch.accept(self)?;
        } else if let Stmt::Empty = ifstmt.else_branch {}
        else {
            ifstmt.else_branch.accept(self)?;
        }

        Ok(())
    }

    fn visit_print(&mut self, print: &Print) -> Result<(), Unwinder> {
        let value = print.expression.accept(self)?;
        let value = value.get_literal();

        match value {
            LiteralValue::String(x) => {
                println!("{}", x);
            }
            LiteralValue::Number(x) => {
                println!("{}", x);
            }
            LiteralValue::Boolean(x) => {
                println!("{}", x);
            }
            LiteralValue::Nil => {
                println!("nil");
            }
        }
        Ok(())
    }

    fn visit_return(&mut self, ret: &Return) -> Result<(), Unwinder> {
        let value = ret.value.accept(self)?;
        // Indicate we should unwind to call size and pass return value, not an error
        Err(Unwinder::ReturnValue(value))
    }

    fn visit_var(&mut self, stmt: &Var) -> Result<(), Unwinder> {
        let value = match stmt.initializer {
            ast::expression::Expr::Empty => {
                EvalValue::RValue(LiteralValue::Nil)
            }
            _ => {
                stmt.initializer.accept(self)?
            }
        };
        self.environment.borrow_mut().define(&stmt.name.lexeme, value);
        Ok(())
    }

    fn visit_while(&mut self, whilestmt: &While) -> Result<(), Unwinder> {
        while is_truthy(&whilestmt.condition.accept(self)?.get_literal()) {
            whilestmt.body.accept(self)?;
        }
        Ok(())
    }
}

impl AstVisitor<RunValue> for Interpreter {
    fn visit_assign(&mut self, expr: &Assign) -> RunValue {
        let value = expr.value.accept(self)?;
        let name = if let Expr::Variable(name) = &expr.name {
            &name.name
        } else {
            panic!("Parser fucked up");
        };

        let exprid = addr_of!(*expr) as ExprId;
        if let Some(distance) = self.locals.get(&exprid) {
            self.environment.borrow_mut().assign_at(*distance, &name.lexeme, &value);
        } else if let Err(err) = self.globals.borrow_mut().assign(&name.lexeme, &value) {
            return Err(Unwinder::error(&err, name.line));
        }

        Ok(value)
    }

    fn visit_binary(&mut self, binary: &Binary) -> RunValue {
        let left = binary.left.accept(self)?;
        let left = left.get_literal();
        let right = binary.right.accept(self)?;
        let right = right.get_literal();

        let line = binary.operator.line;

        let value: LiteralValue = match binary.operator.token_type {
            TokenType::BangEqual => {LiteralValue::Boolean(!is_equal(&left, &right))}
            TokenType::EqualEqual => {LiteralValue::Boolean(is_equal(&left, &right))}
            TokenType::Minus => {
                let (l, r) = check_number_operands(line, &left, &right)?;
                LiteralValue::Number(l - r)
            }
            TokenType::Plus => {
                match (&left, &right) {
                    (LiteralValue::String(l), LiteralValue::String(r)) => {
                        let mut new_str = l.to_string();
                        new_str.push_str(r);
                        LiteralValue::String(Rc::new(new_str))
                    }
                    (LiteralValue::Number(l), LiteralValue::Number(r)) => {LiteralValue::Number(l+r)}
                    _ => {return Err(Unwinder::error("Operands must be two numbers or two strings.", binary.operator.line));}
                    }
                }
            TokenType::Slash => {
                let (l, r) = check_number_operands(line, &left, &right)?;
                LiteralValue::Number(l/r)
            }
            TokenType::Star => {
                let (l, r) = check_number_operands(line, &left, &right)?;
                LiteralValue::Number(l*r)
            }
            TokenType::Greater => {
                let (l, r) = check_number_operands(line, &left, &right)?;
                LiteralValue::Boolean(l > r)
            }
            TokenType::GreaterEqual => {
                let (l, r) = check_number_operands(line, &left, &right)?;
                LiteralValue::Boolean(l >= r)
            }
            TokenType::Less => {
                let (l, r) = check_number_operands(line, &left, &right)?;
                LiteralValue::Boolean(l < r)
            }
            TokenType::LessEqual => {
                let (l, r) = check_number_operands(line, &left, &right)?;
                LiteralValue::Boolean(l <= r)
            }

            _ => {
                panic!("Invalid token type, should have been caught in parser")
            }
        };
        Ok(EvalValue::from(value))
    }

    fn visit_call(&mut self, expr: &Call) -> RunValue {
        let callee = expr.callee.accept(self)?;
        let callee = callee.get_callable(expr.paren.line)?;

        if callee.to_instance().is_some() {
            let err = format!("Can only call functions and classes.");
            return Err(Unwinder::error(&err, expr.paren.line));
        }

        if expr.arguments.len() != callee.arity(){
            let err = format!("Expected {} arguments but got {}.", callee.arity(), expr.arguments.len());
            return Err(Unwinder::error(&err, expr.paren.line));
        }

        let mut arguments: Vec<EvalValue> = Vec::new();
        for argument in expr.arguments.iter() {
            arguments.push(argument.accept(self)?);
        }

        Ok(callee.call(self, arguments)?)
    }

    fn visit_get(&mut self, expr: &Get) -> RunValue {
        let object = expr.object.accept(self)?;
        if let Ok(object) = object.get_callable(expr.name.line) {
            if let Some(instance) = object.to_instance() {
                return Ok(LoxInstance::get(&instance, &expr.name.lexeme)?);
            }
        }
        Err(Unwinder::error("Only instances have properties.", expr.name.line))
    }

    fn visit_grouping(&mut self, visitor: &Grouping) -> RunValue {
        visitor.expression.accept(self)
    }

    fn visit_literal(&mut self, visitor: &Literal) -> RunValue {
        Ok(visitor.value.clone().into())
    }

    fn visit_logical(&mut self, logical: &Logical) -> RunValue {
        let left = logical.left.accept(self)?;
        let left = left.get_literal();

        match logical.operator.token_type {
            TokenType::And => {
                let t = is_truthy(&left);
                if !t {
                    return Ok(left.into());
                }
            }
            TokenType::Or => {
                let t = is_truthy(&left);
                if t {
                    return Ok(left.into());
                }
            }
            _ => {
                panic!("Logic error")
            }
        }

        logical.right.accept(self)
    }

    fn visit_set(&mut self, expr: &Set) -> RunValue {
        let object = expr.object.accept(self)?;
        if let Ok(object) = object.get_callable(expr.name.line) {
            if let Some(instance) = object.to_instance() {
                let value = expr.value.accept(self)?;
                instance.borrow_mut().set(&expr.name.lexeme, value.clone());
                return Ok(value)
            }
        }
        Err(Unwinder::error("Only instances have fields.", expr.name.line))
    }

    fn visit_this(&mut self, this: &This) -> RunValue {
        let e: ExprId = addr_of!(*this) as ExprId;
        match self.lookup_variable(&"this".to_string(), e) {
            Ok(v) => {Ok(v)}
            Err(err) => {Err(Unwinder::error(&err, this.keyword.line))}
        }
    }

    fn visit_unary(&mut self, unary: &Unary) -> RunValue {
        let right = unary.right.accept(self)?;

        let value = match unary.operator.token_type {
            TokenType::Minus => {
                if let LiteralValue::Number(x) = right.get_literal() {
                    LiteralValue::Number(-x).into()
                }
                else {
                    return Err(Unwinder::error("Operand must be a number.",
                                               unary.operator.line))
                }
            }

            TokenType::Bang => {
                EvalValue::from(
                    LiteralValue::Boolean(
                        !is_truthy(&right.get_literal())
                    )
                )
            }
            _ => return Err(Unwinder::error("Unknown unary operator", unary.operator.line))
        };

        Ok(value)
    }

    fn visit_variable(&mut self, varname: &Variable) -> RunValue {
        let e: ExprId = addr_of!(*varname) as ExprId;
        match self.lookup_variable(&varname.name.lexeme, e) {
            Ok(v) => {Ok(v)}
            Err(err) => {Err(Unwinder::error(&err, varname.name.line))}
        }
    }
}