use std::collections::HashMap;
use std::panic::panic_any;
use std::thread::scope;
use crate::interpreter::Interpreter;
use crate::lox;
use crate::lox::ast::expression::{Accept as ExprAccept, Assign, AstVisitor, Binary, Call, Expr, Grouping, Literal, Logical, Unary, Variable};
use crate::lox::ast::statement::{Accept, Block, Expression, Function, If, Print, Return, Stmt, StmtList, StmtVisitor, Var, While};
use crate::lox::{Token, TokenType};

pub struct Resolver<'i> {
    interpreter: &'i mut Interpreter,
    scopes: Vec<HashMap<String, bool>>
}

impl<'i> Resolver<'i> {
    pub fn new(interpreter: &'i mut Interpreter) -> Self {
        Resolver{interpreter, scopes: Vec::new()}
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

    fn resolve_statement_list(&mut self, statements: &StmtList) -> Result<(), String> {
        for statement in statements.iter() {
            self.resolve_statement(statement)?;
        }
        Ok(())
    }

    fn resolve_function(&mut self, function: &Function) -> Result<(), String> {
        self.begin_scope();
        for param in function.params.iter() {
            if let TokenType::Identifier(pn) = &param.token_type {
                self.declare(pn);
                self.define(pn);
            } else {
                panic!("Parser fucked up");
            }
        }
        self.resolve_statement(&function.body)?;
        self.end_scope();
        Ok(())
    }

    fn resolve_local(&mut self, expr: &Expr, name: &str) {
        let idx = (0..self.scopes.len()).rev();
        for (i, scope) in idx.zip(self.scopes.iter()) {
            if scope.contains_key(name) {
                //self.interpreter.resolve(expr, scopes.len() - i - 1);
                return;
            }
        }
    }

    fn declare(&mut self, name: &str) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(String::from(name), false);
        }
    }

    fn define(&mut self, name: &str) {
        if let Some(scope) = self.scopes.last_mut() {
            if let Some(value) = scope.get_mut(name) {
                *value = true;
            }
            else {
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
        self.resolve_local(&assign.value, name);
        Ok(())
    }

    fn visit_binary(&mut self, visitor: &Binary) -> Result<(), String> {
        todo!()
    }

    fn visit_call(&mut self, visitor: &Call) -> Result<(), String> {
        todo!()
    }

    fn visit_grouping(&mut self, visitor: &Grouping) -> Result<(), String> {
        todo!()
    }

    fn visit_literal(&mut self, visitor: &Literal) -> Result<(), String> {
        todo!()
    }

    fn visit_logical(&mut self, visitor: &Logical) -> Result<(), String> {
        todo!()
    }

    fn visit_unary(&mut self, visitor: &Unary) -> Result<(), String> {
        todo!()
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
            else {
                panic!("Bug:  Resolver did not find variable")
            }
        }
        //self.resolve_local(var, name);
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

    fn visit_expression(&mut self, visitor: &Expression) -> Result<(), String> {
        todo!()
    }

    fn visit_function(&mut self, stmt: &Function) -> Result<(), String> {
        let name = if let TokenType::Identifier(x) = &stmt.name.token_type {
            x
        } else {
            panic!("Parser fucked up")
        };
        self.declare(name);
        self.define(name);
        self.resolve_function(stmt)?;
        Ok(())
    }

    fn visit_if(&mut self, visitor: &If) -> Result<(), String> {
        todo!()
    }

    fn visit_print(&mut self, visitor: &Print) -> Result<(), String> {
        todo!()
    }

    fn visit_return(&mut self, visitor: &Return) -> Result<(), String> {
        todo!()
    }

    fn visit_var(&mut self, var: &Var) -> Result<(), String> {
        let name = var.name.as_str();
        self.declare(name);
        if let Expr::Empty = var.initializer {}
        else {
            self.resolve_expression(&var.initializer)?;
        }
        self.define(name);
        Ok(())
    }

    fn visit_while(&mut self, visitor: &While) -> Result<(), String> {
        todo!()
    }
}