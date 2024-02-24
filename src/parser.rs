use clap::error::ErrorKind::TooFewValues;
use crate::lox::{ast, Token, TokenTextValueMetadata};
use crate::lox::ast::expression::{Binary, Expr, LiteralValue, Unary};
use crate::lox::ast::statement::{Print, Expression, Stmt, Var};

pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    current: usize
}

type ParserError = (Token, String);
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
        if let Token::Eof = self.peek() {
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

    fn check<F>(&mut self, match_type: F) -> bool
        where F: Fn(&Token) -> bool {
        if self.is_at_end() {
            return false;
        }

        match_type(self.peek())
    }
    fn consume<F>(&mut self, match_type: F, error_message: String) ->  Result<Token, ParserError>
        where F: Fn(&Token) -> bool {

        if self.check(match_type) {
            return Ok(self.advance());
        }

        Err(self.error(self.peek(), error_message))
    }

    fn error(&self, token: &'a Token, message: String) -> ParserError
    {
        (token.clone(), message)
    }

    fn match_token<F>(&mut self, match_type: F) -> bool
        where F: Fn(&Token) -> bool {
        if self.check(match_type) {
            self.advance();
            return true;
        }
        false
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut statements: Vec<Stmt> = Vec::new();
        while !self.is_at_end() {
            statements.push(self.declaration()?)
        }
        Ok(statements)
    }

    fn comparison(&mut self) -> ParserResult
    {
        let mut left = self.term()?;

        while self.match_token(|x| {
            Token::is_greater(x) || Token::is_greaterequal(x) || Token::is_less(x) || Token::is_lessequal(x)
        }) {
            let operator = self.previous().clone();
            let right = self.term()?;
            left =
                Expr::Binary(
                    Box::new(Binary{left: left, operator, right: right})
                );
        }

        Ok(left)
    }

    fn assignment(&mut self) -> ParserResult {
        let expr = self.or()?;
        let is_equal = self.match_token(|x| Token::is_equal(x));
        if  is_equal {
            let equals = self.previous().clone();
            let value = self.assignment()?;
            let r = match &expr {
                Expr::Variable(x) => {
                    Ok(Expr::Assign(
                        Box::new(ast::expression::Assign{name: Token::Identifier(x.name.clone()), value })
                    ))
                }
                _ => {
                    Err(self.error(&equals, "Invalid target assignment".to_string()))
                }
            };
            return r;
        }

        Ok(expr)
    }

    fn or(&mut self) -> ParserResult {
        let mut expr = self.and()?;
        while self.match_token(Token::is_or) {
            let operator = self.previous().clone();
            let right = self.and()?;
            expr = Expr::Logical(
                Box::new(
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
        while self.match_token(Token::is_and) {
            let operator = self.previous().clone();
            let right = self.equality()?;
            expr = Expr::Logical(
                Box::new(
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

        while self.match_token(|x| {
            Token::is_bangequal(x) || Token::is_equalequal(x)
        }) {
            let operator = self.previous().clone();
            let right = self.comparison()?;
            left =
                Expr::Binary(
                    Box::new(Binary{left: left, operator, right: right})
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

        while self.match_token(|x| {
            Token::is_minus(x) || Token::is_plus(x)
        }) {
            let operator = self.previous().clone();
            let right = self.factor()?;
            left = Expr::Binary(
                    Box::new(Binary{left: left, operator, right: right})
                );
        }

        Ok(left)
    }

    fn factor(&mut self) -> ParserResult
    {
        let mut left = self.unary()?;

        while self.match_token(|x| {
            Token::is_slash(x) || Token::is_star(x)
        }) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            left = Expr::Binary(
                    Box::new(Binary{left: left, operator, right: right})
                );
        }

        Ok(left)
    }

    fn unary(&mut self) -> ParserResult
    {
        if self.match_token(|x| {
            Token::is_bang(x) || Token::is_minus(x)
        }) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            return Ok(Expr::Unary(
                    Box::new(Unary{operator, right: right})
                )
            );
        }

        self.primary()
    }

    fn primary(&mut self) -> ParserResult {
        if self.match_token(Token::is_false) {
            return Ok(Expr::Literal(Box::new(ast::expression::Literal{value: LiteralValue::Boolean(false)})));
        }

        if self.match_token(Token::is_true) {
            return Ok(Expr::Literal(Box::new(ast::expression::Literal{value: LiteralValue::Boolean(true)})));
        }

        if self.match_token(Token::is_nil) {
            return Ok(Expr::Literal(Box::new(ast::expression::Literal{value: LiteralValue::Nil})));
        }

        if self.match_token(|x|{
            Token::is_number(x) || Token::is_string(x)
        }) {
            let previous = self.previous();
            let value = match previous {
                Token::String(x) => LiteralValue::String(x.lexeme.clone()),
                Token::Number(x) => LiteralValue::Number(x.value),
                _ => panic!("Error")
            };
            return Ok(Expr::Literal(Box::new(ast::expression::Literal{value})));
        }

        if self.match_token(|x|{ Token::is_identifier(x)}) {
            let previous = self.previous();
            if let Token::Identifier(x) = previous {
                return Ok(Expr::Variable(Box::new(ast::expression::Variable{name: x.clone()})));
            };

        }

        if self.match_token(Token::is_leftparen) {
            let expr = self.expression()?;
            self.consume(Token::is_rightparen, "Expected )".to_string())?;
            return Ok(Expr::Grouping(Box::new(ast::expression::Grouping{expression: expr})));
        }

        Err(self.error(self.peek(), "Expected expression".to_string()))
    }

    //
    // Statement tree
    //

    fn while_statement(&mut self) -> StatementResult {
        self.consume(Token::is_leftparen, "Expect '(' after 'while'".to_string())?;
        let condition = self.expression()?;
        self.consume(Token::is_rightparen, "Expect ')' after 'whiile' condition.".to_string())?;
        let body = self.statement()?;
        Ok(
            Stmt::While(
                Box::new(
                    ast::statement::While {
                        condition,
                        body
                    }
                )
            )
        )
    }

    fn if_statement(&mut self) -> StatementResult {
        self.consume(Token::is_leftparen, "Expect '(' after 'if'.".to_string())?;
        let condition = self.expression()?;
        self.consume(Token::is_rightparen, "Expect ')' after 'if' condition.".to_string())?;

        let then_branch = self.statement()?;
        let else_branch =
            if self.match_token(Token::is_else) {
                self.statement()?
            } else {
                Stmt::Empty
            };

        Ok(
            Stmt::If(
                Box::new(
                    ast::statement::If {
                        condition,
                        then_branch,
                        else_branch,
                    }
                )
            )
        )
    }


    fn declaration(&mut self) -> StatementResult {
        if self.match_token( |x| {
            Token::is_var(x)
        }) {
            return self.var_declaration();
        }
        self.statement()
        // If there was an error synchronize
    }

    fn var_declaration(&mut self) -> StatementResult {
        let name = self.consume(Token::is_identifier,
                                "Expect variable name".to_string())?;

        let initializer = if self.match_token(|x| {Token::is_equal(x)}) {
            self.expression()?
        }
        else {
            Expr::Empty
        };
        self.consume(Token::is_semicolon, "Expect ';' after variable declaration.".to_string())?;
        let new_id = if let Token::Identifier(x) = name
        {
            Box::new(Var {name: x, initializer})
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
        if self.match_token(Token::is_if) {
            return self.if_statement();
        }
        if self.match_token(Token::is_print) {
            return self.print_statement();
        }
        if self.match_token(Token::is_leftbrace) {
            return self.block();
        }
        if self.match_token(Token::is_while) {
            return self.while_statement();
        }
        self.expression_statement()
    }

    fn block(&mut self) -> StatementResult {
        let mut statements: Vec<Stmt> = Vec::new();
        while !self.check(Token::is_rightbrace) && !self.is_at_end() {
            statements.push(self.declaration()?)
        }
        self.consume(Token::is_rightbrace, "Expect '}' after block.".to_string())?;
        Ok(
            Stmt::Block(Box::new(ast::statement::Block { statements }))
        )
    }

    fn print_statement(&mut self) -> StatementResult {
        let value = self.expression()?;
        self.consume(Token::is_semicolon, "Expected ; after value".to_string())?;
        let new_print = Print{expression: value};
        Ok(
            Stmt::Print(Box::new(new_print))
        )
    }

    fn expression_statement(&mut self) -> StatementResult {
        let value = self.expression()?;
        self.consume(Token::is_semicolon, "Expected ; after expression".to_string())?;
        Ok(
            Stmt::Expression(Box::new(
                Expression{expression: value}
            )
            )
        )
    }
}