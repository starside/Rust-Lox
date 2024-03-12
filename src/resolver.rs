use std::collections::HashMap;
use std::ptr::addr_of;
use crate::interpreter::{ExprId, Interpreter};
use crate::lox::ast::expression::{Accept as ExprAccept, Assign, AstVisitor, Binary, Call, Expr, Grouping, Literal, Logical, Unary, Variable};
use crate::lox::ast::statement::{Accept, Block, Expression, Function, If, Print, Return, Stmt, StmtList, StmtVisitor, Var, While};
use crate::lox::{TokenType};

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

impl<'i> Resolver<'i> {
    pub fn new(interpreter: &'i mut Interpreter) -> Self {
        Resolver{interpreter, scopes: Vec::new(), current_function: FunctionType::None}
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn resolve_statement(&mut self, statement: &Stmt) -> Result<(), String> {
        statement.accept(self)
    }

    fn resolve_expression(&mut self, expr: &Expr) -> Result<(), String> {
        expr.accept(self)
    }

    pub fn resolve_statement_list(&mut self, statements: &StmtList) -> Result<(), String> {
        for statement in statements.iter() {
            self.resolve_statement(statement)?;
        }
        Ok(())
    }

    fn resolve_function(&mut self, function: &Function, function_type: FunctionType) -> Result<(), String> {
        let enclosing_function= self.current_function;
        self.current_function = function_type;
        self.begin_scope();
        for param in function.params.iter() {
            if let TokenType::Identifier(pn) = &param.token_type {
                self.declare(pn)?;
                self.define(pn);
            } else {
                panic!("Parser fucked up");
            }
        }
        self.resolve_statement(&function.body)?;
        self.end_scope();
        self.current_function = enclosing_function;
        Ok(())
    }

    fn resolve_local(&mut self, exprid: ExprId, name: &str) {
        let idx = (0..self.scopes.len()).rev();
        for (i, scope) in idx.zip(self.scopes.iter()) {
            if scope.contains_key(name) {
                self.interpreter.resolve(exprid, i);
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

impl AstVisitor<Result<(), String>> for Resolver<'_>{
    fn visit_assign(&mut self, assign: &Assign) -> Result<(), String> {
        self.resolve_expression(&assign.value)?;
        let name = if let Expr::Variable(name) = &assign.name {
            &name.name
        } else {
            panic!("Parser fucked up");
        };
        self.resolve_local(addr_of!(*assign) as ExprId, name);
        Ok(())
    }

    fn visit_binary(&mut self, binary: &Binary) -> Result<(), String> {
        self.resolve_expression(&binary.left)?;
        self.resolve_expression(&binary.right)?;
        Ok(())
    }

    fn visit_call(&mut self, call: &Call) -> Result<(), String> {
        self.resolve_expression(&call.callee)?;
        for arg in call.arguments.iter() {
            self.resolve_expression(arg)?;
        }
        Ok(())
    }

    fn visit_grouping(&mut self, paren: &Grouping) -> Result<(), String> {
        self.resolve_expression(&paren.expression)?;
        Ok(())
    }

    fn visit_literal(&mut self, _: &Literal) -> Result<(), String> {
        Ok(())
    }

    fn visit_logical(&mut self, logical: &Logical) -> Result<(), String> {
        self.resolve_expression(&logical.left)?;
        self.resolve_expression(&logical.right)?;
        Ok(())
    }

    fn visit_unary(&mut self, unary: &Unary) -> Result<(), String> {
        self.resolve_expression(&unary.right)?;
        Ok(())
    }

    fn visit_variable(&mut self, var: &Variable) -> Result<(), String> {
        let name = &var.name;
        if let Some(scope) = self.scopes.last_mut() {
            if let Some(value) = scope.get(name) {
                if *value == false {
                    // TODO:  Lost line information
                    return Err("Can't read local variable in its own initializer.".to_string());
                }
            }
        }
        self.resolve_local(addr_of!(*var) as ExprId, name);
        Ok(())
    }
}
impl StmtVisitor<Result<(), String>> for Resolver<'_> {
    fn visit_block(&mut self, block: &Block) -> Result<(), String> {
        self.begin_scope();
        self.resolve_statement_list(&block.statements)?;
        self.end_scope();
        Ok(())
    }

    fn visit_expression(&mut self, expr: &Expression) -> Result<(), String> {
        self.resolve_expression(&expr.expression)?;
        Ok(())
    }

    fn visit_function(&mut self, stmt: &Function) -> Result<(), String> {
        let name = if let TokenType::Identifier(x) = &stmt.name.token_type {
            x
        } else {
            panic!("Parser fucked up")
        };
        self.declare(name)?;
        self.define(name);
        self.resolve_function(stmt, FunctionType::Function)?;
        Ok(())
    }

    fn visit_if(&mut self, ifstmt: &If) -> Result<(), String> {
        self.resolve_expression(&ifstmt.condition)?;
        self.resolve_statement(&ifstmt.then_branch)?;
        if let Stmt::Empty = ifstmt.else_branch {}
        else {
            self.resolve_statement(&ifstmt.else_branch)?;
        }
        Ok(())
    }

    fn visit_print(&mut self, stmt: &Print) -> Result<(), String> {
        self.resolve_expression(&stmt.expression)?;
        Ok(())
    }

    fn visit_return(&mut self, stmt: &Return) -> Result<(), String> {
        if self.current_function == FunctionType::None {
            return Err("Can't return from top-level code.".to_string());
        }
        if let Expr::Empty = stmt.value {}
        else {
            self.resolve_expression(&stmt.value)?;
        }
        Ok(())
    }

    fn visit_var(&mut self, var: &Var) -> Result<(), String> {
        let name = var.name.as_str();
        self.declare(name)?;
        if let Expr::Empty = var.initializer {}
        else {
            self.resolve_expression(&var.initializer)?;
        }
        self.define(name);
        Ok(())
    }

    fn visit_while(&mut self, stmt: &While) -> Result<(), String> {
        self.resolve_expression(&stmt.condition)?;
        self.resolve_statement(&stmt.body)?;
        Ok(())
    }
}
