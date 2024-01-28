use crate::lox::{ast, Token};
use crate::lox::ast::{Binary, Expr, LiteralValue, Unary};

pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    current: usize
}

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

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn check<F>(&mut self, match_type: F) -> bool
        where F: Fn(&Token) -> bool {
        if self.is_at_end() {
            return false;
        }

        match_type(self.peek())
    }
    fn consume<F>(&mut self, match_type: F) -> &Token
        where F: Fn(&Token) -> bool {

        if self.check(match_type) {
            return self.advance();
        }

        panic!("Parse error!")
    }

    fn match_token<F>(&mut self, match_type: F) -> bool
        where F: Fn(&Token) -> bool {
        if self.check(match_type) {
            self.advance();
            return true;
        }
        false
    }

    pub fn parse(&mut self) -> Box<Expr> {
        self.expression()
    }

    fn comparison(&mut self) -> Box<Expr>
    {
        let mut left = self.term();

        while self.match_token(|x| {
            Token::is_greater(x) || Token::is_greaterequal(x) || Token::is_less(x) || Token::is_lessequal(x)
        }) {
            let operator = self.previous().clone();
            let right = self.term();
            left = Box::new(
                Expr::Binary(
                    Box::new(Binary{left, operator, right})
                )
            );
        }

        left
    }

    fn equality(&mut self) -> Box<Expr> {
        let mut left = self.comparison();

        while self.match_token(|x| {
            Token::is_bangequal(x) || Token::is_equalequal(x)
        }) {
            let operator = self.previous().clone();
            let right = self.comparison();
            left = Box::new(
                Expr::Binary(
                    Box::new(Binary{left, operator, right})
                )
            );
        }

        left
    }

    fn expression(&mut self) -> Box<Expr> {
        self.equality()
    }


    fn term(&mut self) -> Box<Expr>
    {
        let mut left = self.factor();

        while self.match_token(|x| {
            Token::is_minus(x) || Token::is_plus(x)
        }) {
            let operator = self.previous().clone();
            let right = self.factor();
            left = Box::new(
                Expr::Binary(
                    Box::new(Binary{left, operator, right})
                )
            );
        }

        left
    }

    fn factor(&mut self) -> Box<Expr>
    {
        let mut left = self.unary();

        while self.match_token(|x| {
            Token::is_slash(x) || Token::is_star(x)
        }) {
            let operator = self.previous().clone();
            let right = self.unary();
            left = Box::new(
                Expr::Binary(
                    Box::new(Binary{left, operator, right})
                )
            );
        }

        left
    }

    fn unary(&mut self) -> Box<Expr>
    {
        if self.match_token(|x| {
            Token::is_bang(x) || Token::is_minus(x)
        }) {
            let operator = self.previous().clone();
            let right = self.unary();
            return Box::new(
                Expr::Unary(
                    Box::new(Unary{operator, right})
                )
            );
        }

        self.primary()
    }

    fn primary(&mut self) -> Box<Expr> {
        if self.match_token(Token::is_false) {
            return Box::new(Expr::Literal(Box::new(ast::Literal{value: LiteralValue::Number(0.0)})));
        }

        if self.match_token(Token::is_true) {
            return Box::new(Expr::Literal(Box::new(ast::Literal{value: LiteralValue::Number(1.0)})));
        }

        if self.match_token(Token::is_nil) {
            return Box::new(Expr::Literal(Box::new(ast::Literal{value: LiteralValue::Nil})));
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
            return Box::new(Expr::Literal(Box::new(ast::Literal{value})));
        }

        if self.match_token(Token::is_leftparen) {
            let expr = self.expression();
            self.consume(Token::is_rightparen);
            return Box::new(Expr::Grouping(Box::new(ast::Grouping{expression: expr})));
        }

        Box::new(Expr::Empty)
    }
}