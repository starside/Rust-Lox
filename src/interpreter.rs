use crate::lox::{ast, TokenType};
use crate::lox::ast::LiteralValue;
use crate::lox::ast::expression::{Accept, Assign, AstVisitor, Binary, Call, Grouping, Literal, Logical, Unary, Variable};
use crate::lox::ast::statement::{Accept as StmtAccept, Block, Expression, Function, If, Print, Return, Stmt, StmtVisitor, Var, While};
use std::collections::HashMap;
use std::rc::Rc;
use std::time::{Instant};


pub trait Callable {
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<EvalValue>) -> EvalValue;
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
    pub fn get_literal(&self) -> Result<LiteralValue, Unwinder> {
        match self {
            EvalValue::RValue(x) => {Ok(x.clone())}
            EvalValue::LValue(x) => {Ok(x.to_literal())}
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

// Environment to store variables at some level of scope
struct Environment {
    enclosing: Option<usize>,
    values: HashMap<String, EvalValue>
}

struct BuiltinFunctionTime;

impl Callable for BuiltinFunctionTime {
    fn call(&self, interpreter: &mut Interpreter, _arguments: Vec<EvalValue>) -> EvalValue {
        EvalValue::RValue(LiteralValue::Number(
            Instant::now().duration_since(interpreter.boot_time).as_micros() as f64
        ))
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
    body: ast::statement::FuncBody
}

impl LoxFunction {
    fn new(function: &Function) -> Self {
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
            body: function.body.clone() // Get new reference count
        }
    }
}
impl Callable for LoxFunction {
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<EvalValue>) -> EvalValue {
        let enclosing = interpreter.environment_stack.global_frame();
        let my_frame = interpreter.environment_stack.push_frame(enclosing);

        for (arg_name, arg_value) in self.params.iter().zip(arguments) {
            interpreter.environment_stack.define(my_frame, arg_name, &arg_value);
        }
        let run_result = self.body.accept(interpreter);
        interpreter.environment_stack.pop_frame();
        if let Err(unwinder) =  run_result {
            match unwinder {
                Unwinder::RuntimeError(_) => {panic!("Runtime error, code shouldn't panic here but does out of laziness")}
                Unwinder::ReturnValue(val) => {
                    return val;
                }
            }
        }
        EvalValue::RValue(LiteralValue::Nil)
    }

    fn arity(&self) -> usize {
        self.params.len()
    }

    fn to_literal(&self) -> LiteralValue {
        LiteralValue::String(format!("<fn {} >", self.name))
    }
}

impl Environment {
    pub fn new(enclosing: Option<usize>) -> Self {
        Environment {enclosing, values: HashMap::new()}
    }

    pub fn define(&mut self, name: &str, value: &EvalValue) {
        self.values.insert(String::from(name), value.clone());
    }
}

struct EnvironmentStack {
    environment_stack: Vec<Environment>
}

impl From<String> for Unwinder {
    fn from(value: String) -> Self {
        Unwinder::RuntimeError(value)
    }
}

impl EnvironmentStack {
    pub fn new() -> Self {
        EnvironmentStack{environment_stack: vec![Environment::new(None)]}
    }

    pub fn define(&mut self, scope_id: Option<usize>, name: &str, value: &EvalValue) {
        let idx = scope_id.unwrap();
        self.environment_stack[idx].define(name, value);
    }

    pub fn push_frame(&mut self, enclosing: Option<usize>) -> Option<usize> {
        self.environment_stack.push(Environment::new(enclosing));
        self.current_frame()
    }
    pub fn pop_frame(&mut self) {
        self.environment_stack.pop();
    }

    pub fn current_frame(&self) -> Option<usize> {
        if self.environment_stack.is_empty() {
            None
        } else{
            Some(self.environment_stack.len() - 1)
        }
    }

    pub fn global_frame(&self) -> Option<usize> {
        Some(0)
    }

    pub fn assign(
        &mut self,
        scope_id: Option<usize>,
        name: &String,
        value: &EvalValue) -> Result<(),String> {

        if let Some(scope_id) = scope_id {
            let name = name.clone();

            // First check current scope
            if self.environment_stack[scope_id].values.contains_key(&name) {
                self.environment_stack[scope_id].values.insert(name, value.clone());
                return Ok(());
            }

            // Follow enclosing scopes up the stack
            let mut enclosing_idx = self.environment_stack[scope_id].enclosing;
            while let Some(e) = enclosing_idx {
                if self.environment_stack[e].values.contains_key(&name) {
                    self.environment_stack[e].values.insert(name, value.clone());
                    return Ok(());
                } else {
                    enclosing_idx = self.environment_stack[e].enclosing;
                }
            }
        }

        Err(format!("L-value {} is not defined but was assigned to", &name).to_string())
    }

    pub fn get(&mut self, scope_id: Option<usize>, name: &str) -> RunValue{

        if let Some(scope_id) = scope_id {
            // First check current scope
            if  let Some(v) = self.environment_stack[scope_id].values.get(name) {
                return Ok(v.clone());
            }

            // Follow enclosing scopes up the stack
            let mut enclosing_idx = self.environment_stack[scope_id].enclosing;
            while let Some(e) = enclosing_idx {
                if let Some(v) = self.environment_stack[e].values.get(name) {
                    return Ok(v.clone());
                } else {
                    enclosing_idx = self.environment_stack[e].enclosing;
                }
            }
        }
        Err(Unwinder::RuntimeError(format!("Undefined variable'{}'", name).to_string()))
    }
}

pub struct Interpreter {
    environment_stack: EnvironmentStack,
    boot_time: Instant
}

impl<'a> Interpreter {
    pub fn new() -> Self {
        let mut es = EnvironmentStack::new();
        es.define(es.current_frame(),"time", &EvalValue::LValue(
            Rc::new(Box::new(BuiltinFunctionTime))
        ));
        Interpreter{environment_stack: es, boot_time: Instant::now()}
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
impl StmtVisitor<Result<(), Unwinder>> for Interpreter
{
    fn visit_block(&mut self, block: &Block) -> Result<(), Unwinder> {
        self.environment_stack.push_frame(self.environment_stack.current_frame());
        for statement in block.statements.iter() {
            let res = statement.accept(self);
            if let Err(Unwinder::ReturnValue(rv)) = res {
                self.environment_stack.pop_frame();
                return Err(Unwinder::ReturnValue(rv));
            }
        }
        self.environment_stack.pop_frame();
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
        self.environment_stack.define(
            self.environment_stack.current_frame(),
            name,
            &EvalValue::LValue(
                Rc::new(
                    Box::new(LoxFunction::new(function))
                )
            )
        );
        Ok(())
    }

    fn visit_if(&mut self, ifstmt: &If) -> Result<(), Unwinder> {
        if is_truthy(&ifstmt.condition.accept(self)?.get_literal()?) {
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
        let value = value.get_literal()?;

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
                LiteralValue::Nil
            }
            _ => {
                stmt.initializer.accept(self)?.get_literal()?.clone()
            }
        };
        self.environment_stack.define(self.environment_stack.current_frame(), &stmt.name, &EvalValue::RValue(value));
        Ok(())
    }

    fn visit_while(&mut self, whilestmt: &While) -> Result<(), Unwinder> {
        while is_truthy(&whilestmt.condition.accept(self)?.get_literal()?) {
            whilestmt.body.accept(self)?;
        }
        Ok(())
    }
}

impl AstVisitor<RunValue> for Interpreter {
    fn visit_assign(&mut self, expr: &Assign) -> RunValue {
        let value = expr.value.accept(self)?;
        self.environment_stack.assign(self.environment_stack.current_frame(), &expr.name, &value)?;
        return Ok(value);
    }

    fn visit_binary(&mut self, visitor: &Binary) -> RunValue {
        let left = visitor.left.accept(self)?;
        let left = left.get_literal()?;
        let right = visitor.right.accept(self)?;
        let right = right.get_literal()?;

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

        Ok(callee.call(self, arguments))
    }

    fn visit_grouping(&mut self, visitor: &Grouping) -> RunValue {
        visitor.expression.accept(self)
    }

    fn visit_literal(&mut self, visitor: &Literal) -> RunValue {
        Ok(visitor.value.clone().into())
    }

    fn visit_logical(&mut self, logical: &Logical) -> RunValue {
        let left = logical.left.accept(self)?;
        let left = left.get_literal()?;

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
                if let LiteralValue::Number(x) = right.get_literal()? {
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
                            is_truthy(&right.get_literal()?)
                        )
                    )
                )
            }
            _ => Err("Unknown unary operator".to_string())
        };

        Ok(value?)
    }

    fn visit_variable(&mut self, varname: &Variable) -> RunValue {
        let res = self.environment_stack.get(self.environment_stack.current_frame(), &varname.name);
        res
    }
}