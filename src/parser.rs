use std::pin::Pin;
use std::rc::Rc;
use crate::lox::ast::{LiteralValue, VarName};
use crate::lox::{ast, Token, TokenKind, TokenType};
use crate::lox::ast::expression::{Binary, Call, Expr, Literal, Unary};
use crate::lox::ast::statement::{Print, Expression, Stmt, Var, FuncList};

pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    current: usize
}

pub type ParserError = (Token, String);
type ParserResult = Result<Expr, ParserError>;
type StatementResult = Result<Stmt, ParserError>;

impl<'a> Parser<'a> {

    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Parser {tokens, current: 0}
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn is_at_end(&self) -> bool {
        let token = self.peek();
        if TokenType::Eof == token.token_type {
            return true;
        }
        false
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous().clone()
    }

    fn check(&self, match_types: &[TokenKind]) -> bool
    {
        if self.is_at_end() {
            return false;
        }

        match_types.contains(&TokenKind::from(&self.peek().token_type))
    }

    fn consume(&mut self, match_type: TokenKind, error_message: String) ->  Result<Token, ParserError>
    {
        if self.check(&[match_type]) {
            return Ok(self.advance());
        }

        Err(self.error(self.peek(), error_message))
    }

    fn error(&self, token: &'a Token, message: String) -> ParserError
    {

        (
            token.clone(),
            format!("Error at \'{}\': {}", token.lexeme, message)
        )
    }

    fn match_token(&mut self, match_types: &[TokenKind]) -> bool
    {
        if self.check(match_types) {
            self.advance();
            return true;
        }
        false
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, Vec<ParserError>> {
        let mut statements: Vec<Stmt> = Vec::new();
        let mut errors: Vec<ParserError> = Vec::new();

        while !self.is_at_end() {
            let result = self.declaration();
            match result {
                Ok(x) => {statements.push(x)}
                Err(x) => {errors.push(x)}
            };
        }
        if errors.is_empty() {
            Ok(statements)
        } else {
            Err(errors)
        }
    }

    fn comparison(&mut self) -> ParserResult
    {
        let mut left = self.term()?;

        while self.match_token(
            &[TokenKind::Greater, TokenKind::GreaterEqual, TokenKind::LessEqual, TokenKind::Less]) {
            let operator = self.previous().clone();
            let right = self.term()?;
            left =
                Expr::Binary(
                    Pin::new(Box::new(Binary{left, operator, right}))
                );
        }

        Ok(left)
    }

    fn assignment(&mut self) -> ParserResult {
        let expr = self.or()?;
        let is_equal = self.match_token(&[TokenKind::Equal]);
        if  is_equal {
            let equals = self.previous().clone();
            let value = self.assignment()?;
            match &expr {
                Expr::Variable(_) => {}
                _ => {
                    return Err(self.error(&equals, "Invalid assignment target.".to_string()));
                }
            };
            return Ok(Expr::Assign(
                Box::pin(ast::expression::Assign{
                    name: expr,
                    value})
            ));
        }

        Ok(expr)
    }

    fn or(&mut self) -> ParserResult {
        let mut expr = self.and()?;
        while self.match_token(&[TokenKind::Or]) {
            let operator = self.previous().clone();
            let right = self.and()?;
            expr = Expr::Logical(
                Box::pin(
                    ast::expression::Logical {
                        left: expr,
                        operator,
                        right
                    }
                )
            );
        }
        Ok(expr)
    }

    fn and(&mut self) -> ParserResult {
        let mut expr = self.equality()?;
        while self.match_token(&[TokenKind::And]) {
            let operator = self.previous().clone();
            let right = self.equality()?;
            expr = Expr::Logical(
                Box::pin(
                    ast::expression::Logical {
                        left: expr,
                        operator,
                        right
                    }
                )
            );
        }
        Ok(expr)
    }

    fn equality(&mut self) -> ParserResult {
        let mut left = self.comparison()?;

        while self.match_token(&[TokenKind::BangEqual, TokenKind::EqualEqual]) {
            let operator = self.previous().clone();
            let right = self.comparison()?;
            left =
                Expr::Binary(
                    Box::pin(Binary{left, operator, right})
                );
        }

        Ok(left)
    }

    fn expression(&mut self) -> ParserResult {
        self.assignment()
    }


    fn term(&mut self) -> ParserResult
    {
        let mut left = self.factor()?;

        while self.match_token(
            &[TokenKind::Minus, TokenKind::Plus]) {
            let operator = self.previous().clone();
            let right = self.factor()?;
            left = Expr::Binary(
                    Box::pin(Binary{left, operator, right})
                );
        }

        Ok(left)
    }

    fn factor(&mut self) -> ParserResult
    {
        let mut left = self.unary()?;

        while self.match_token(
            &[TokenKind::Slash, TokenKind::Star]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            left = Expr::Binary(
                    Box::pin(Binary{left, operator, right})
                );
        }

        Ok(left)
    }

    fn finish_call(&mut self, callee: Expr) -> ParserResult {
        let mut arguments: Vec<Expr> = Vec::new();
        if !self.check(&[TokenKind::RightParen]) {
            loop {
                if arguments.len() >= 255 {
                    return Err(self.error(self.peek(), "Can't have more than 255 arguments.".to_string()));
                }
                arguments.push(self.expression()?);
                if !self.match_token(&[TokenKind::Comma]) {
                    break;
                }
            }
        }

        let paren = self.consume(TokenKind::RightParen, "Expect ')' after arguments.".to_string())?;
        Ok(
            Expr::Call(
                Box::pin(Call{
                    callee,
                    paren,
                    arguments
                })
            )
        )
    }
    fn call(&mut self) -> ParserResult {
        let mut expr = self.primary()?;

        loop {
            if self.match_token(&[TokenKind::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn unary(&mut self) -> ParserResult
    {
        if self.match_token(
            &[TokenKind::Bang, TokenKind::Minus]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            return Ok(Expr::Unary(
                    Box::pin(Unary{operator, right})
                )
            );
        }
        self.call()
    }

    fn primary(&mut self) -> ParserResult {
        if self.match_token(&[TokenKind::False]) {
            return Ok(Expr::Literal(Box::pin(ast::expression::Literal{value: LiteralValue::Boolean(false)})));
        }

        if self.match_token(&[TokenKind::True]) {
            return Ok(Expr::Literal(Box::pin(ast::expression::Literal{value: LiteralValue::Boolean(true)})));
        }

        if self.match_token(&[TokenKind::Nil]) {
            return Ok(Expr::Literal(Box::pin(ast::expression::Literal{value: LiteralValue::Nil})));
        }

        if self.match_token(
            &[TokenKind::Number, TokenKind::String]) {
            let previous = &self.previous().token_type;
            let value = match previous {
                TokenType::String(x) => LiteralValue::String(x.clone()),
                TokenType::Number(x) => LiteralValue::Number(*x),
                _ => panic!("Error")
            };
            return Ok(Expr::Literal(Box::pin(ast::expression::Literal{value})));
        }

        if self.match_token(
            &[TokenKind::Identifier]) {
            let pt = self.previous();
            let previous = &pt.token_type;
            let line = pt.line;
            if let TokenType::Identifier(x) = previous {
                return Ok(Expr::Variable(Box::pin(ast::expression::Variable{name: VarName::new(x, line)})));
            };
        }

        if self.match_token(&[TokenKind::LeftParen]) {
            let expr = self.expression()?;
            self.consume(TokenKind::RightParen, "Expected )".to_string())?;
            return Ok(Expr::Grouping(Box::pin(ast::expression::Grouping{expression: expr})));
        }

        Err(self.error(self.peek(), "Expect expression.".to_string()))
    }

    //
    // Statement tree
    //

    fn while_statement(&mut self) -> StatementResult {
        self.consume(TokenKind::LeftParen, "Expect '(' after 'while'".to_string())?;
        let condition = self.expression()?;
        self.consume(TokenKind::RightParen, "Expect ')' after 'whiile' condition.".to_string())?;
        let body = self.statement()?;
        Ok(
            Stmt::While(
                Box::pin(
                    ast::statement::While {
                        condition,
                        body
                    }
                )
            )
        )
    }

    fn for_statement(&mut self) -> StatementResult {
        self.consume(TokenKind::LeftParen, "Expect '(' after 'for'.".to_string())?;

        // Read initializer
        let initializer = if self.match_token(&[TokenKind::Semicolon]) {
            None
        } else if self.match_token(&[TokenKind::Var]) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        // Read Condition.
        let condition = if !self.check(&[TokenKind::Semicolon]) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(TokenKind::Semicolon, "Expect ';' after loop condition.".to_string())?;

        // Read increment
        let increment = if !self.check(&[TokenKind::RightParen]) {
            Some(
                Stmt::Expression(
                    Box::pin(
                        Expression {
                            expression: self.expression()?
                        }
                    )
                )
            )
        } else {
            None
        };
        self.consume(TokenKind::RightParen, "Expect ')' after 'for' clauses.".to_string())?;

        let for_body = self.statement()?;

        /*
            { // Block A
              <initializer>
              while (<condition>)
              { // Block B
                <for_body>
                <increment>
              }
            }
         */

        let condition = if let Some(x) = condition {
            x
        } else {
            Expr::Literal(Box::pin(ast::expression::Literal{value: LiteralValue::Boolean(true)}))
        };

        // Build while body.  First for_body, followed by optional increment
        let mut while_body: Vec<Stmt> = Vec::new();
        while_body.push(for_body);
        if let Some(x) = increment {
            while_body.push(x);
        }

        let while_body = Stmt::Block(
            Box::pin(
                ast::statement::Block {
                    statements: while_body
                }
            )
        );

        let while_loop = Stmt::While(
            Box::pin(
                ast::statement::While {
                    condition,
                    body: while_body
                }
            )
        );

        // Construct block A
        let mut statements: Vec<Stmt> = Vec::new();
        if let Some(x) = initializer {
            statements.push(x);
        }
        statements.push(while_loop);

        Ok(
            Stmt::Block(
                Box::pin(
                    ast::statement::Block {
                        statements
                    }
                )
            )
        )
    }

    fn if_statement(&mut self) -> StatementResult {
        self.consume(TokenKind::LeftParen, "Expect '(' after 'if'.".to_string())?;
        let condition = self.expression()?;
        self.consume(TokenKind::RightParen, "Expect ')' after 'if' condition.".to_string())?;

        let then_branch = self.statement()?;
        let else_branch =
            if self.match_token(&[TokenKind::Else]) {
                self.statement()?
            } else {
                Stmt::Empty
            };

        Ok(
            Stmt::If(
                Box::pin(
                    ast::statement::If {
                        condition,
                        then_branch,
                        else_branch,
                    }
                )
            )
        )
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }

            match self.peek().token_type {
                TokenType::Class | TokenType::Fun | TokenType::For |TokenType::If |
                TokenType::Print | TokenType::Return | TokenType::Var | TokenType::While
                    => {
                    return;
                }
                _ => {}
            }

            self.advance();
        }
    }

    fn fun_declaration(&mut self) -> StatementResult {
        let name = self.consume(TokenKind::Identifier,
                                "Expect function name".to_string())?;
        self.consume(TokenKind::LeftParen,
                                "Expect '(' after function name".to_string())?;
        let mut params: Vec<Token> = Vec::new();
        if !self.check(&[TokenKind::RightParen]) {
            loop {
                if params.len() >= 255 {
                    return Err(self.error(self.peek(), "Can't have more than 255 parameters.".to_string()));
                }
                params.push(self.consume(TokenKind::Identifier, "Expect an identifier".to_string())?);
                if !self.match_token(&[TokenKind::Comma]) {
                    break;
                }
            }
        }

        self.consume(TokenKind::RightParen,
                                 "Expect ')' after parameters.".to_string())?;


        self.consume(TokenKind::LeftBrace, "Expect '{' before function body.".to_string())?;
        let body = self.block()?;
        Ok(
            Stmt::Function(
                Box::pin(
                    ast::statement::Function {
                        name,
                        params,
                        body: Rc::new(Box::pin(body)) // Damn, I need to figure out lifetimes
                    }
                )
            )
        )
    }

    fn declaration(&mut self) -> StatementResult {
        let result = if self.match_token(&[TokenKind::Var]) {
            self.var_declaration()
        }
        else if self.match_token(&[TokenKind::Class]){
            self.class_declaration()
        }
        else if self.match_token(&[TokenKind::Fun]) {
            self.fun_declaration()
        }
        else{
            self.statement()
        };

        if result.is_err() {
            self.synchronize();
        }

        result
    }

    fn class_declaration(&mut self) -> StatementResult {
        let name = self.consume(TokenKind::Identifier, "Expect class name".to_string())?;
        self.consume(TokenKind::LeftBrace, "Expect '{' before class body.".to_string())?;
        let mut methods: FuncList = Vec::new();
        while !self.check(&[TokenKind::RightBrace]) && !self.is_at_end() {
            let funres = self.fun_declaration()?;
            match funres {
                Stmt::Function(f) => {
                    methods.push(f);
                }
                _ => {panic!("Impossible, not a function")}
            }
        }

        self.consume(TokenKind::RightBrace, "Expect '}' after class body.".to_string())?;
        Ok(
            Stmt::Class(
                Box::pin(
                    ast::statement::Class {
                        name,
                        methods
                    }
                )
            )
        )
    }

    fn var_declaration(&mut self) -> StatementResult {
        let name = self.consume(TokenKind::Identifier,
                                "Expect variable name.".to_string())?;

        let initializer = if self.match_token(&[TokenKind::Equal]) {
            self.expression()?
        }
        else {
            Expr::Empty
        };
        self.consume(TokenKind::Semicolon, "Expect ';' after variable declaration.".to_string())?;
        let new_id = if let TokenType::Identifier(x) = &name.token_type
        {
            Box::pin(Var {name: VarName::new(x, name.line), initializer})
        } else {
            panic!("Logic error!")
        };
        Ok(
            Stmt::Var(
                new_id
            )
        )
    }

    fn statement(&mut self) -> StatementResult {
        if self.match_token(&[TokenKind::If]) {
            return self.if_statement();
        }
        if self.match_token(&[TokenKind::Print]) {
            return self.print_statement();
        }
        if self.match_token(&[TokenKind::LeftBrace]) {
            return self.block();
        }
        if self.match_token(&[TokenKind::While]) {
            return self.while_statement();
        }
        if self.match_token(&[TokenKind::For]) {
            return self.for_statement();
        }
        if self.match_token(&[TokenKind::Return]) {
            return self.return_statement();
        }
        self.expression_statement()
    }

    fn return_statement(&mut self) -> StatementResult {
        let keyword = self.previous().clone();

        let value = if !self.check(&[TokenKind::Semicolon]) {
            self.expression()?
        } else {
            Expr::Literal(Box::pin(Literal{value: LiteralValue::Nil}))
        };

        self.consume(TokenKind::Semicolon, "Expect ';' after return value".to_string())?;

        Ok(
            Stmt::Return(
                Box::pin(ast::statement::Return{
                    keyword,
                    value
                })
            )
        )
    }

    fn block(&mut self) -> StatementResult {
        let mut statements: Vec<Stmt> = Vec::new();
        while !self.check(&[TokenKind::RightBrace]) && !self.is_at_end() {
            statements.push(self.declaration()?)
        }
        self.consume(TokenKind::RightBrace, "Expect '}' after block.".to_string())?;
        Ok(
            Stmt::Block(Box::pin(ast::statement::Block { statements }))
        )
    }

    fn print_statement(&mut self) -> StatementResult {
        let value = self.expression()?;
        self.consume(TokenKind::Semicolon, "Expected ; after value".to_string())?;
        let new_print = Print{expression: value};
        Ok(
            Stmt::Print(Box::pin(new_print))
        )
    }

    fn expression_statement(&mut self) -> StatementResult {
        let value = self.expression()?;
        self.consume(TokenKind::Semicolon, "Expect \';\' after expression.".to_string())?;
        Ok(
            Stmt::Expression(Box::pin(
                Expression{expression: value}
            )
            )
        )
    }
}