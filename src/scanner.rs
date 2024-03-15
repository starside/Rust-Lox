use crate::lox::{Token, TokenType};
use std::collections::HashMap;

pub struct ScannerError {
    pub line: usize,
    pub message: String
}

pub struct Scanner<'a> {
    source: &'a str,
    tokens: Vec<Token>,

    start: usize,
    current: usize,
    line: usize,

    errors: Vec<ScannerError>,
    identifier_table: HashMap<&'static str, TokenType>
}

impl<'s> Scanner<'s> {
    pub fn new(source: & str) -> Scanner{
        Scanner {
            source,
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
            errors: Vec::new(),
            identifier_table: Scanner::build_identifier_hash()
        }
    }

    fn build_identifier_hash() -> HashMap<&'static str, TokenType>{
        let mut identifiers:HashMap<&str, TokenType> = HashMap::new();

        identifiers.insert("and", TokenType::And);
        identifiers.insert("class", TokenType::Class);
        identifiers.insert("else", TokenType::Else);
        identifiers.insert("false", TokenType::False);
        identifiers.insert("for", TokenType::For);
        identifiers.insert("fun", TokenType::Fun);
        identifiers.insert("if", TokenType::If);
        identifiers.insert("nil", TokenType::Nil);
        identifiers.insert("or", TokenType::Or);
        identifiers.insert("print", TokenType::Print);
        identifiers.insert("return", TokenType::Return);
        identifiers.insert("super", TokenType::Super);
        identifiers.insert("this", TokenType::This);
        identifiers.insert("true", TokenType::True);
        identifiers.insert("var", TokenType::Var);
        identifiers.insert("while", TokenType::While);
        identifiers
    }

    pub fn scan_tokens(&mut self) -> Result<&Vec<Token>, &Vec<ScannerError>>{
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }
        self.add_token(self.new_token(
            TokenType::Eof
        ));

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

    // Abbreviated for New Token Meta Data
    fn new_token(&self, token_type: TokenType) -> Token {
        Token {
            line: self.line,
            token_type
        }
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            // Single character tokens
            '(' => self.add_token(self.new_token(TokenType::LeftParen)),
            ')' => self.add_token(self.new_token(TokenType::RightParen)),
            '{' => self.add_token(self.new_token(TokenType::LeftBrace)),
            '}' => self.add_token(self.new_token(TokenType::RightBrace)),
            ',' => self.add_token(self.new_token(TokenType::Comma)),
            '.' => self.add_token(self.new_token(TokenType::Dot)),
            '-' => self.add_token(self.new_token(TokenType::Minus)),
            '+' => self.add_token(self.new_token(TokenType::Plus)),
            ';' => self.add_token(self.new_token(TokenType::Semicolon)),
            '*' => self.add_token(self.new_token(TokenType::Star)),

            // Whitespace
            ' ' | '\r' | '\t' => {},
            '\n' => {self.line += 1},

            // Two character tokens
            '!' => {
                let matched = self.match_char('=');
                self.add_token(if matched {self.new_token(TokenType::BangEqual)} else {self.new_token(TokenType::Bang)})
            },
            '=' => {
                let matched = self.match_char('=');
                self.add_token(if matched {self.new_token(TokenType::EqualEqual)} else {self.new_token(TokenType::Equal)})
            },
            '<' => {
                let matched = self.match_char('=');
                self.add_token(if matched {self.new_token(TokenType::LessEqual)} else {self.new_token(TokenType::Less)})
            },
            '>' => {
                let matched = self.match_char('=');
                self.add_token(if matched {self.new_token(TokenType::GreaterEqual)} else {self.new_token(TokenType::Greater)})
            },
            '/' => {
                let matched = self.match_char('/');
                if matched {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                }
                else {
                    self.add_token(self.new_token(TokenType::Slash))
                }
            },

            // String
            '"' => self.string(),

            // Number literal
            '0'..='9' => {
                self.number();
            },

            // Identifier
            'a'..='z' | 'A'..='Z' | '_' => {
                self.identifier();
            },

            // Error
            _ => self.error(format!("Unexpected character {}", c))
        };
    }

    fn identifier(&mut self) {
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {self.advance();}
        let value = std::str::from_utf8(&self.source.as_bytes()[self.start..self.current]).unwrap();

        if let Some(keyword) = self.identifier_table.get(value) {
            self.add_token(self.new_token(keyword.clone()));
        }
        else {
            self.add_token(self.new_token(
                TokenType::Identifier(value.to_string())
            ));
        }
    }

    fn number(&mut self) {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }
        let value = std::str::from_utf8(&self.source.as_bytes()[self.start..self.current]).unwrap();
        self.add_token(self.new_token(
            TokenType::Number(value.parse::<f64>().unwrap())
        ));
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.error("Unterminated string".to_string());
            return;
        }

        // The closing "
        self.advance();
        let value = std::str::from_utf8(&self.source.as_bytes()[(self.start+1)..(self.current-1)]).unwrap();
        self.add_token(self.new_token(
            TokenType::String(value.to_string())
        ));
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return 0 as char;
        }
        self.source.as_bytes()[self.current] as char
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return 0 as char;
        }
        self.source.as_bytes()[self.current + 1] as char
    }

    fn add_token(&mut self, token: Token) {
        self.tokens.push(token);
    }

    fn advance(&mut self) -> char {
        let next_char = self.source.as_bytes()[self.current] as char;
        self.current += 1;
        next_char
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.source.as_bytes()[self.current] as char != expected {
            return false;
        }

        self.current += 1;
        true
    }

    fn error(&mut self, message: String) {
        self.errors.push(ScannerError{line: self.line, message});
    }

}