use std::cell::RefCell;
use crate::lox::{ast, TokenType};
use crate::lox::ast::LiteralValue;
use crate::lox::ast::expression::{Accept, Assign, AstVisitor, Binary, Call, Expr, Grouping, Literal, Logical, Unary, Variable};
use crate::lox::ast::statement::{Accept as StmtAccept, Block, Expression, Function, If, Print, Return, Stmt, StmtVisitor, Var, While};
use std::collections::HashMap;
use std::ops::Deref;
use std::ptr::addr_of;
use std::rc::Rc;
use std::time::{Instant};


pub trait Callable {
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<EvalValue>) -> Result<EvalValue, String>;
    fn arity(&self) -> usize;
    fn to_literal(&self) -> LiteralValue;
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

    pub fn get_callable(&self) -> Result<LValueType, Unwinder> {
        match self {
            EvalValue::RValue(_) => {Err(Unwinder::RuntimeError("Cannot convert to callable".to_string()))}
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
    fn call(&self, interpreter: &mut Interpreter, _arguments: Vec<EvalValue>) -> Result<EvalValue, String> {
        Ok(EvalValue::RValue(LiteralValue::Number(
            Instant::now().duration_since(interpreter.boot_time).as_micros() as f64
        )))
    }
    fn arity(&self) -> usize {
        0
    }

    fn to_literal(&self) -> LiteralValue {
        LiteralValue::String("<fn builtin time>".to_string())
    }
}

struct LoxFunction {
    name: String,
    params: Vec<String>,
    body: ast::statement::FuncBody,
    closure: EnvironmentRef
}

impl LoxFunction {
    fn new(function: &Function, closure: EnvironmentRef) -> Self {
        let name = if let TokenType::Identifier(n) = &function.name.token_type {
            n.clone()
        } else { panic!("Parser fucked up") };

        let params: Vec<String> =
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
            closure
        }
    }
}
impl Callable for LoxFunction {
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<EvalValue>) -> Result<EvalValue, String> {
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
            Err(
                Unwinder::RuntimeError("Somehow not executing a block.  This is a bug".to_string())
            )
        };

        if let Err(unwinder) =  run_result {
            match unwinder {
                Unwinder::RuntimeError(e) => {
                    return Err(e);
                }
                Unwinder::ReturnValue(val) => {
                    return Ok(val);
                }
            }
        }
        Ok(EvalValue::RValue(LiteralValue::Nil))
    }

    fn arity(&self) -> usize {
        self.params.len()
    }

    fn to_literal(&self) -> LiteralValue {
        LiteralValue::String(format!("<fn {} >", self.name))
    }
}

pub type EnvironmentRef = Rc<RefCell<Environment>>;
impl Environment {
    pub fn new(enclosing: Option<EnvironmentRef>) -> EnvironmentRef {
        Rc::new(
            RefCell::new(
                Environment {
                    enclosing,
                    values: HashMap::new()
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
        value: &EvalValue) -> Result<(),String> {

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

        Err(format!("L-value {} is not defined but was assigned to", &name).to_string())
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

        Err(format!("Variable  {} is not found", &name).to_string())
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
            }
        }

        Err(format!("Variable  {} is not found", &name).to_string())
    }

    fn assign_at(&mut self, distance: usize, name: &str, value: &EvalValue) {
        if distance == 0 {
            self.values.insert(name.to_string(), value.clone());
        }
        else {
            let a = self.ancestor(distance).expect("Parser fucked up");
            a.borrow_mut().values.insert(name.to_string(), value.clone());
        }
    }
}

impl From<String> for Unwinder {
    fn from(value: String) -> Self {
        Unwinder::RuntimeError(value)
    }
}

pub struct Interpreter {
    environment: EnvironmentRef,
    globals: EnvironmentRef,
    boot_time: Instant,
    locals: HashMap<ExprId, usize>
}

impl<'a> Interpreter {
    pub fn new() -> Self {
        let locals: HashMap<ExprId, usize> = HashMap::new();
        let global_env = Environment::new(None);
        {
            let mut ge = global_env.borrow_mut();
            ge.define("time", EvalValue::LValue(
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
        (LiteralValue::Boolean(a), LiteralValue::Boolean(b)) => {a == b}
        (LiteralValue::Number(a), LiteralValue::Number(b)) => {a == b}
        (LiteralValue::String(a), LiteralValue::String(b)) => {a == b}
        (_, _) => {false}
    }
}

pub enum Unwinder {
    RuntimeError(String),
    ReturnValue(EvalValue)
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
                Box::new(LoxFunction::new(function, self.environment.clone()))
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
        } else {
            if let Stmt::Empty = ifstmt.else_branch {}
            else {
                ifstmt.else_branch.accept(self)?;
            }
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
                println!("Nil");
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
        self.environment.borrow_mut().define(&stmt.name, value);
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

        let exprid = addr_of!(expr) as ExprId;
        if let Some(distance) = self.locals.get(&exprid) {
            self.environment.borrow_mut().assign_at(*distance, name, &value);
        } else {
            self.globals.borrow_mut().assign(name, &value)?;
        }

        return Ok(value);
    }

    fn visit_binary(&mut self, visitor: &Binary) -> RunValue {
        let left = visitor.left.accept(self)?;
        let left = left.get_literal();
        let right = visitor.right.accept(self)?;
        let right = right.get_literal();

        match (&left, &right) {
            // If both operands are strings
            (LiteralValue::String(l), LiteralValue::String(r)) => {
                match visitor.operator.token_type {
                    TokenType::Plus => {
                        let mut new_str = l.to_string();
                        new_str.push_str(r);
                        Ok(LiteralValue::String(new_str).into())
                    }

                    TokenType::BangEqual => {
                        Ok(LiteralValue::Boolean(!is_equal(&left, &right)).into())
                    }

                    TokenType::EqualEqual => {
                        Ok(LiteralValue::Boolean(is_equal(&left, &right)).into())
                    }
                    _ => {
                        Err("Invalid binary operation between strings".to_string().into())
                    }
                }

            }

            // If Both operands are numbers
            (LiteralValue::Number(l), LiteralValue::Number(r)) => {
                match visitor.operator.token_type {
                    TokenType::Plus => {Ok(LiteralValue::Number(l+r).into())}
                    TokenType::Minus => {Ok(LiteralValue::Number(l-r).into())}
                    TokenType::Star => {Ok(LiteralValue::Number(l*r).into())}
                    TokenType::Slash => {Ok(LiteralValue::Number(l/r).into())}
                    TokenType::BangEqual => {Ok(LiteralValue::Boolean(l != r).into())}
                    TokenType::EqualEqual => {Ok(LiteralValue::Boolean(l == r).into())}
                    TokenType::Greater => {Ok(LiteralValue::Boolean(l > r).into())}
                    TokenType::GreaterEqual => {Ok(LiteralValue::Boolean(l >= r).into())}
                    TokenType::Less => {Ok(LiteralValue::Boolean(l < r).into())}
                    TokenType::LessEqual => {Ok(LiteralValue::Boolean(l <= r).into())}
                    _ => Err("Unknown operand between two Numbers".to_string().into())
                }
            }

            _ => {
                Err("Operands must be two numbers or two strings".to_string().into())
            }
        }
    }

    fn visit_call(&mut self, expr: &Call) -> RunValue {
        let callee = expr.callee.accept(self)?;
        let callee = callee.get_callable()?;

        if expr.arguments.len() != callee.arity(){
            let err = format!("Line {}, expected {} but got {} arguments",
                              expr.paren.line, callee.arity(), expr.arguments.len());
            return Err(err.into());
        }

        let mut arguments: Vec<EvalValue> = Vec::new();
        for argument in expr.arguments.iter() {
            arguments.push(argument.accept(self)?);
        }

        Ok(callee.call(self, arguments)?)
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
                if !is_truthy(&left) {
                    return Ok(left.clone().into());
                }
            }
            TokenType::Or => {
                if is_truthy(&left) {
                    return Ok(left.clone().into());
                }
            }
            _ => {
                panic!("Logic error")
            }
        }

        logical.right.accept(self)
    }

    fn visit_unary(&mut self, visitor: &Unary) -> RunValue {
        let right = visitor.right.accept(self)?;

        let value = match visitor.operator.token_type {
            TokenType::Minus => {
                if let LiteralValue::Number(x) = right.get_literal() {
                    Ok(LiteralValue::Number(-x).into())
                }
                else {
                    Err("Unary minus must operate on a number".to_string())
                }
            }

            TokenType::Bang => {
                Ok(
                    EvalValue::from(
                        LiteralValue::Boolean(
                            is_truthy(&right.get_literal())
                        )
                    )
                )
            }
            _ => Err("Unknown unary operator".to_string())
        };

        Ok(value?)
    }

    fn visit_variable(&mut self, varname: &Variable) -> RunValue {
        let e: ExprId = addr_of!(varname) as ExprId;
        let v = self.lookup_variable(&varname.name, e)?;
        Ok(v)
    }
}