use std::ops::Deref;
use std::ptr::addr_of;
use crate::interpreter::{ExprId, Interpreter};
use crate::lox::ast::expression::{Accept as ExprAccept, Assign, AstVisitor, Binary, Call, Expr, Grouping, Literal, Logical, Unary, Variable};
use crate::lox::ast::statement::{Accept, Block, Expression, Function, If, Print, Return, Stmt, StmtList, StmtVisitor, Var, While};
use crate::lox::{TokenType};
use rustc_hash::{FxHashMap};

type HashMap<K, V> = FxHashMap<K, V>;

#[derive(Copy, Clone, PartialEq)]
enum FunctionType {
    None,
    Function
}

pub struct Resolver<'i> {
    interpreter: &'i mut Interpreter,
    scopes: Vec<HashMap<String, bool>>,
    current_function: FunctionType
}

pub struct ResolverError {
    pub line: usize,
    pub message: String
}

impl ResolverError {

    fn new(line: usize, message: &str) -> Self {
        ResolverError {
            line,
            message: message.to_string()
        }
    }
    fn error(line: usize, message: &str) -> Result<(), Self>{
        Err(
            ResolverError::new(line, message)
        )
    }
}

impl<'i> Resolver<'i> {
    pub fn new(interpreter: &'i mut Interpreter) -> Self {
        Resolver{interpreter, scopes: Vec::new(), current_function: FunctionType::None}
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::default());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn resolve_statement(&mut self, statement: &Stmt) -> Result<(), ResolverError> {
        statement.accept(self)
    }

    fn resolve_expression(&mut self, expr: &Expr) -> Result<(), ResolverError> {
        expr.accept(self)
    }

    pub fn resolve_statement_list(&mut self, statements: &StmtList) -> Result<(), ResolverError> {
        for statement in statements.iter() {
            self.resolve_statement(statement)?;
        }
        Ok(())
    }

    fn resolve_function(&mut self, function: &Function, function_type: FunctionType) -> Result<(), ResolverError> {
        let enclosing_function= self.current_function;
        self.current_function = function_type;
        self.begin_scope();
        for param in function.params.iter() {
            if let TokenType::Identifier(pn) = &param.token_type {
                self.declare(pn).map_err(|x| {
                    let m = format!("Error at \'{}\': {}", param.lexeme, &x);
                    ResolverError::new(param.line, &m)
                })?;
                self.define(pn);
            } else {
                panic!("Parser fucked up");
            }
        }
        if let Stmt::Block(x) = &function.body.deref().deref(){
            self.resolve_statement_list(&x.statements)?;
        } else {
            panic!("Bug in parser or resolver")
        }
        self.end_scope();
        self.current_function = enclosing_function;
        Ok(())
    }

    fn resolve_local(&mut self, exprid: ExprId, name: &str) {
        let idx = 0..self.scopes.len();
        let num_scopes = self.scopes.len();
        for (i, scope) in idx.zip(self.scopes.iter()).rev() {
            if scope.contains_key(name) {
                self.interpreter.resolve(exprid, num_scopes - 1 - i);
                return;
            }
        }
    }

    fn declare(&mut self, name: &str) -> Result<(), String> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.insert(String::from(name), false).is_some() {
                return Err("Already a variable with this name in this scope.".to_string());
            }
        }
        Ok(())
    }

    fn define(&mut self, name: &str) {
        if let Some(scope) = self.scopes.last_mut() {
            if let Some(value) = scope.get_mut(name) {
                *value = true;
            }
            else {
                //scope.insert(name.to_string(), true);
                panic!("Resolver cannot define an undeclared variable");
            }
        }
    }
}

impl AstVisitor<Result<(), ResolverError>> for Resolver<'_>{
    fn visit_assign(&mut self, assign: &Assign) -> Result<(), ResolverError> {
        self.resolve_expression(&assign.value)?;
        let name = if let Expr::Variable(name) = &assign.name {
            &name.name
        } else {
            panic!("Parser fucked up");
        };
        self.resolve_local(addr_of!(*assign) as ExprId, &name.lexeme);
        Ok(())
    }

    fn visit_binary(&mut self, binary: &Binary) -> Result<(), ResolverError> {
        self.resolve_expression(&binary.left)?;
        self.resolve_expression(&binary.right)?;
        Ok(())
    }

    fn visit_call(&mut self, call: &Call) -> Result<(), ResolverError> {
        self.resolve_expression(&call.callee)?;
        for arg in call.arguments.iter() {
            self.resolve_expression(arg)?;
        }
        Ok(())
    }

    fn visit_grouping(&mut self, paren: &Grouping) -> Result<(), ResolverError> {
        self.resolve_expression(&paren.expression)?;
        Ok(())
    }

    fn visit_literal(&mut self, _: &Literal) -> Result<(), ResolverError> {
        Ok(())
    }

    fn visit_logical(&mut self, logical: &Logical) -> Result<(), ResolverError> {
        self.resolve_expression(&logical.left)?;
        self.resolve_expression(&logical.right)?;
        Ok(())
    }

    fn visit_unary(&mut self, unary: &Unary) -> Result<(), ResolverError> {
        self.resolve_expression(&unary.right)?;
        Ok(())
    }

    fn visit_variable(&mut self, var: &Variable) -> Result<(), ResolverError> {
        let name = &var.name;
        if let Some(scope) = self.scopes.last_mut() {
            if let Some(value) = scope.get(&name.lexeme) {
                if !(*value) {
                    return ResolverError::error(var.name.line,
                    &format!("Error at \'{}\': {}", var.name.lexeme,
                            "Can't read local variable in its own initializer."));
                }
            }
        }
        self.resolve_local(addr_of!(*var) as ExprId, &name.lexeme);
        Ok(())
    }
}
impl StmtVisitor<Result<(), ResolverError>> for Resolver<'_> {
    fn visit_block(&mut self, block: &Block) -> Result<(), ResolverError> {
        self.begin_scope();
        self.resolve_statement_list(&block.statements)?;
        self.end_scope();
        Ok(())
    }

    fn visit_expression(&mut self, expr: &Expression) -> Result<(), ResolverError> {
        self.resolve_expression(&expr.expression)?;
        Ok(())
    }

    fn visit_function(&mut self, stmt: &Function) -> Result<(), ResolverError> {
        let name = if let TokenType::Identifier(x) = &stmt.name.token_type {
            x
        } else {
            panic!("Parser fucked up")
        };
        self.declare(name).map_err(|x| {ResolverError::new(stmt.name.line, &x)})?;
        self.define(name);
        self.resolve_function(stmt, FunctionType::Function)?;
        Ok(())
    }

    fn visit_if(&mut self, ifstmt: &If) -> Result<(), ResolverError> {
        self.resolve_expression(&ifstmt.condition)?;
        self.resolve_statement(&ifstmt.then_branch)?;
        if let Stmt::Empty = ifstmt.else_branch {}
        else {
            self.resolve_statement(&ifstmt.else_branch)?;
        }
        Ok(())
    }

    fn visit_print(&mut self, stmt: &Print) -> Result<(), ResolverError> {
        self.resolve_expression(&stmt.expression)?;
        Ok(())
    }

    fn visit_return(&mut self, stmt: &Return) -> Result<(), ResolverError> {
        //stmt.keyword.line
        if self.current_function == FunctionType::None {
            return ResolverError::error(stmt.keyword.line,
                 &format!("Error at \'{}\': Can't return from top-level code.", stmt.keyword.lexeme))
        }
        if let Expr::Empty = stmt.value {}
        else {
            self.resolve_expression(&stmt.value)?;
        }
        Ok(())
    }

    fn visit_var(&mut self, var: &Var) -> Result<(), ResolverError> {
        let name = &var.name.lexeme;
        self.declare(name).map_err(|x| {
            let m = format!("Error at \'{}\': {}", name,  &x);
            ResolverError::new(var.name.line, &m)
        })?;
        if let Expr::Empty = var.initializer {}
        else {
            self.resolve_expression(&var.initializer)?;
        }
        self.define(name);
        Ok(())
    }

    fn visit_while(&mut self, stmt: &While) -> Result<(), ResolverError> {
        self.resolve_expression(&stmt.condition)?;
        self.resolve_statement(&stmt.body)?;
        Ok(())
    }
}
