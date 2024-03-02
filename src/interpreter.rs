use crate::lox::{ast, TokenType};
use crate::lox::ast::LiteralValue;
use crate::lox::ast::expression::{Accept, Assign, AstVisitor, Binary, Call, Grouping, Literal, Logical, Unary, Variable};
use crate::lox::ast::statement::{Accept as StmtAccept, Block, Expression, Function, If, Print, StmtVisitor, Var, While};
use std::collections::HashMap;
use std::rc::Rc;
use std::time::{Instant};


trait Callable {
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<EvalValue>) -> EvalValue;
    fn arity(&self) -> usize;
}

type LValueType = Rc<Box<dyn Callable>>;

#[derive(Clone)]
enum EvalValue {
    RValue(LiteralValue),
    LValue(LValueType)
}

impl EvalValue {
    pub fn get_literal(&self) -> Result<&LiteralValue, String> {
        match self {
            EvalValue::RValue(x) => {Ok(x)}
            EvalValue::LValue(_) => {Err("Cannot convert to callable".to_string())}
        }
    }

    pub fn get_callable(&self) -> Result<LValueType, String> {
        match self {
            EvalValue::RValue(_) => {Err("Cannot convert to callable".to_string())}
            EvalValue::LValue(l) => {Ok(l.clone())}
        }
    }
}

impl From<LiteralValue> for EvalValue{
    fn from(value: LiteralValue) -> Self {
        EvalValue::RValue(value)
    }
}

type RunValue = Result<EvalValue, String>;

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

impl EnvironmentStack {
    pub fn new() -> Self {
        EnvironmentStack{environment_stack: vec![Environment::new(None)]}
    }

    pub fn define(&mut self, scope_id: Option<usize>, name: &str, value: &EvalValue) {
        let idx = scope_id.unwrap();
        self.environment_stack[idx].define(name, value);
    }

    pub fn push_frame(&mut self, enclosing: Option<usize>) {
        self.environment_stack.push(Environment::new(enclosing));
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
        Err(format!("Undefined variable'{}'", name).to_string())
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

impl StmtVisitor<Result<(), String>> for Interpreter
{
    fn visit_block(&mut self, block: &Block) -> Result<(), String> {
        self.environment_stack.push_frame(self.environment_stack.current_frame());
        for statement in block.statements.iter() {
            statement.accept(self)?
        }
        self.environment_stack.pop_frame();
        Ok(())
    }

    fn visit_expression(&mut self, expression: &Expression) -> Result<(), String> {
        expression.expression.accept(self)?;
        Ok(())
    }

    fn visit_function(&mut self, visitor: &Function) -> Result<(), String> {
        todo!()
    }

    fn visit_if(&mut self, ifstmt: &If) -> Result<(), String> {
        if is_truthy(ifstmt.condition.accept(self)?.get_literal()?) {
            ifstmt.then_branch.accept(self)?;
        } else {
            ifstmt.else_branch.accept(self)?;
        }
        Ok(())
    }

    fn visit_print(&mut self, print: &Print) -> Result<(), String> {
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

    fn visit_var(&mut self, stmt: &Var) -> Result<(), String> {
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

    fn visit_while(&mut self, whilestmt: &While) -> Result<(), String> {
        while is_truthy(whilestmt.condition.accept(self)?.get_literal()?) {
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
                        Err("Invalid binary operation between strings".to_string())
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
                    _ => Err("Unknown operand between two Numbers".to_string())
                }
            }

            _ => {
                Err("Operands must be two numbers or two strings".to_string())
            }
        }
    }

    fn visit_call(&mut self, expr: &Call) -> RunValue {
        let callee = expr.callee.accept(self)?;
        let callee = callee.get_callable()?;

        if expr.arguments.len() != callee.arity(){
            let err = format!("Line {}, expected {} but got {} arguments",
                              expr.paren.line, callee.arity(), expr.arguments.len());
            return Err(err);
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
                Ok(LiteralValue::Boolean(!is_truthy(right.get_literal()?)).into())
            }
            _ => Err("Unknown unary operator".to_string())
        };

        value
    }

    fn visit_variable(&mut self, varname: &Variable) -> RunValue {
        let res = self.environment_stack.get(self.environment_stack.current_frame(), &varname.name);
        res
    }
}