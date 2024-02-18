use crate::lox::{ast, Token};
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
        self.equality()
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
            Token::is_number(x) || Token::is_string(x) || Token::is_identifier(x)
        }) {
            let previous = self.previous();
            let value = match previous {
                Token::String(x) => LiteralValue::String(x.lexeme.clone()),
                Token::Number(x) => LiteralValue::Number(x.value),
                Token::Identifier(x) => LiteralValue::String(x.lexeme.clone()),
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
        Ok(
            Stmt::Var(
                Box::new(Var {name, initializer})
            )
        )
    }

    fn statement(&mut self) -> StatementResult {
        if self.match_token( |x| {
            Token::is_print(x)
        }) {
            return self.print_statement();
        }
        self.expression_statement()
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