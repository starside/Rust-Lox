use std::ops::Deref;
use std::ptr::addr_of;
use std::rc::Rc;
use crate::interpreter::{ExprId, Interpreter};
use crate::lox::ast::expression::{Accept as ExprAccept, Assign, AstVisitor, Binary, Call, Expr, Get, Grouping, Literal, Logical, Set, Super, This, Unary, Variable};
use crate::lox::ast::statement::{Accept, Block, Class, Expression, Function, If, Print, Return, Stmt, StmtList, StmtVisitor, Var, While};
use crate::lox::{RunString, TokenType};
use rustc_hash::{FxHashMap};
use crate::lox::ast::LiteralValue;

type HashMap<K, V> = FxHashMap<K, V>;

#[derive(Copy, Clone, PartialEq)]
enum FunctionType {
    None,
    Function,
    Initializer,
    Method
}

#[derive(Copy, Clone)]
enum ClassType {
    None,
    Class,
    Subclass
}

pub struct Resolver<'i> {
    interpreter: &'i mut Interpreter,
    scopes: Vec<HashMap<RunString, bool>>,
    current_function: FunctionType,
    current_class: ClassType
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
        Resolver{interpreter, scopes: Vec::new(), current_function: FunctionType::None, current_class: ClassType::None}
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::default());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn resolve_statement(&mut self, statement: &Stmt) -> Result<(), ResolverError> {
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

    fn resolve_local(&mut self, exprid: ExprId, name: &RunString) {
        let idx = 0..self.scopes.len();
        let num_scopes = self.scopes.len();
        for (i, scope) in idx.zip(self.scopes.iter()).rev() {
            if scope.contains_key(name) {
                self.interpreter.resolve(exprid, num_scopes - 1 - i);
                return;
            }
        }
    }

    fn declare(&mut self, name: &RunString) -> Result<(), String> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.insert(name.clone(), false).is_some() {
                return Err("Already a variable with this name in this scope.".to_string());
            }
        }
        Ok(())
    }

    fn define(&mut self, name: &RunString) {
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

    fn visit_get(&mut self, expr: &Get) -> Result<(), ResolverError> {
        self.resolve_expression(&expr.object)?;
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

    fn visit_set(&mut self, expr: &Set) -> Result<(), ResolverError> {
        self.resolve_expression(&expr.value)?;
        self.resolve_expression(&expr.object)?;
        Ok(())
    }

    fn visit_super(&mut self, expr: &Super) -> Result<(), ResolverError> {
        match self.current_class {
            ClassType::None => {
                return Err(
                    ResolverError::new(expr.keyword.line,
                    &format!("Error at \'{}\': Can't use 'super' outside of a class.",
                             expr.keyword.lexeme))
                );
            }
            ClassType::Class => {
                return Err(
                    ResolverError::new(expr.keyword.line,
                        &format!("Error at \'{}\': Can't use 'super' in a class with no superclass.",
                         expr.keyword.lexeme))
                );
            }
            ClassType::Subclass => {}
        }
        self.resolve_local(addr_of!(*expr) as ExprId, &Rc::new(expr.keyword.lexeme.clone()));
        Ok(())
    }

    fn visit_this(&mut self, this: &This) -> Result<(), ResolverError> {
        match self.current_class {
            ClassType::Class | ClassType::Subclass => {}
            ClassType::None => {
                return Err(
                    ResolverError::new(this.keyword.line,
                   &format!("Error at \'{}\': Can't use 'this' outside of a class.", this.keyword.lexeme))
                );
            }
        }
        self.resolve_local(addr_of!(*this) as ExprId, &Rc::new(this.keyword.lexeme.clone()));
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

    fn visit_class(&mut self, class: &Class) -> Result<(), ResolverError> {
        let mut has_superclass = false;
        let enclosing_class = self.current_class;
        self.current_class = ClassType::Class;

        let name = if let TokenType::Identifier(x) = &class.name.token_type {
            x
        } else {
            panic!("Parser fucked up")
        };
        self.declare(name).map_err(|x| {ResolverError::new(class.name.line, &x)})?;
        self.define(name);

        if let Some(superclass) = &class.superclass {
            match superclass {
                Expr::Variable(s) => {
                    if s.name.lexeme.as_str() == class.name.lexeme.as_str() {
                        return ResolverError::error(
                            s.name.line,
                            &format!(
                                "Error at \'{}\': A class can't inherit from itself.",
                                s.name.lexeme));
                    }
                },
                _ => {panic!("Parser fucked up, this should only be Variable");}
            }
            self.current_class = ClassType::Subclass;
            has_superclass = true;
            self.resolve_expression(superclass)?;
        }

        if has_superclass {
            self.begin_scope();
            self.scopes.last_mut().unwrap().insert(Rc::new("super".to_string()), true);
        }

        self.begin_scope();
        self.scopes.last_mut().unwrap().insert(Rc::new("this".to_string()), true);

        for method in &class.methods {
            let declaration = if method.name.lexeme == "init" {
                FunctionType::Initializer
            } else {
                FunctionType::Method
            };
            self.resolve_function(method, declaration)?;
        }

        if has_superclass {
            self.end_scope();
        }

        self.end_scope();
        self.current_class = enclosing_class;
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
        if let Expr::Literal(x) = &stmt.value {
            if x.value != LiteralValue::Nil {
                if self.current_function == FunctionType::Initializer {
                    return ResolverError::error(
                        stmt.keyword.line,
                        &&format!("Error at \'{}\': Can't return a value from an initializer.", stmt.keyword.lexeme));
                }
            }
        }

        self.resolve_expression(&stmt.value)?;
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
