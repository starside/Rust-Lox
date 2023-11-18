use crate::lox;
use crate::lox::{Token, TokenMetadata};
use crate::lox::Token::{Comma, Dot, LeftBrace, LeftParen, Minus, Plus, RightBrace, RightParen, Semicolon, Star};

pub struct ScannerError {
    pub line: usize,
    pub message: String
}

pub struct Scanner<'a> {
    source: &'a str,
    tokens: Vec<Token<'a>>,

    start: usize,
    current: usize,
    line: usize,

    errors: Vec<ScannerError>
}

impl<'s> Scanner<'s> {
    pub fn new(source: & str) -> Scanner{
        Scanner {
            source,
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
            errors: Vec::new()
        }
    }

    pub fn scan_tokens(&mut self) -> Result<&Vec<Token>, &Vec<ScannerError>>{
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }
        self.add_token(Token::Eof);

        if self.errors.is_empty() {
            Ok(&self.tokens)
        }
        else {
            Err(&self.errors)
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        let literal = &self.source.as_bytes()[self.start..self.current];
        match c {
            '(' => self.add_token(LeftParen(TokenMetadata::new(self.line))),
            ')' => self.add_token(RightParen(TokenMetadata::new(self.line))),
            '{' => self.add_token(LeftBrace(TokenMetadata::new(self.line))),
            '}' => self.add_token(RightBrace(TokenMetadata::new(self.line))),
            ',' => self.add_token(Comma(TokenMetadata::new(self.line))),
            '.' => self.add_token(Dot(TokenMetadata::new(self.line))),
            '-' => self.add_token(Minus(TokenMetadata::new(self.line))),
            '+' => self.add_token(Plus(TokenMetadata::new(self.line))),
            ';' => self.add_token(Semicolon(TokenMetadata::new(self.line))),
            '*' => self.add_token(Star(TokenMetadata::new(self.line))),
            _ => self.error(format!("Unexpected character {}", c))
        };
    }

    fn add_token(&mut self, token: Token<'s>) {
        self.tokens.push(token);
    }

    fn advance(&mut self) -> char {
        let next_char = self.source.as_bytes()[self.current] as char;
        self.current += 1;
        next_char
    }

    fn error(&mut self, message: String) {
        self.errors.push(ScannerError{line: self.line, message});
    }

}