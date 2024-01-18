use crate::lox;
use crate::lox::{Token, TokenMetadata, TokenNumberValueMetadata, TokenTextValueMetadata};
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
    identifier_table: HashMap<&'static str, fn(TokenMetadata) -> Token>
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

    fn build_identifier_hash() -> HashMap<&'static str, fn(TokenMetadata) -> Token>{
        let mut identifiers:HashMap<&str, fn(TokenMetadata) -> Token> = HashMap::new();

        identifiers.insert("and", |x: TokenMetadata|
            {
                Token::And(x)
            });
        identifiers.insert("class", |x: TokenMetadata|
            {
                Token::Class(x)
            });
        identifiers.insert("else", |x: TokenMetadata|
            {
                Token::Else(x)
            });
        identifiers.insert("false", |x: TokenMetadata|
            {
                Token::False(x)
            });
        identifiers.insert("for", |x: TokenMetadata|
            {
                Token::For(x)
            });
        identifiers.insert("fun", |x: TokenMetadata|
            {
                Token::Fun(x)
            });
        identifiers.insert("if", |x: TokenMetadata|
            {
                Token::If(x)
            });
        identifiers.insert("nil", |x: TokenMetadata|
            {
                Token::Nil(x)
            });
        identifiers.insert("or", |x: TokenMetadata|
            {
                Token::Or(x)
            });
        identifiers.insert("print", |x: TokenMetadata|
            {
                Token::Print(x)
            });
        identifiers.insert("return", |x: TokenMetadata|
            {
                Token::Return(x)
            });identifiers.insert("super", |x: TokenMetadata|
            {
                Token::Super(x)
            });
        identifiers.insert("this", |x: TokenMetadata|
            {
                Token::This(x)
            });identifiers.insert("true", |x: TokenMetadata|
            {
                Token::True(x)
            });identifiers.insert("var", |x: TokenMetadata|
            {
                Token::Var(x)
            });
            identifiers.insert("while", |x: TokenMetadata|
            {
                Token::While(x)
            });

        identifiers
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

    // Abbreviated for New Token Meta Data
    fn ntmd(&self) -> TokenMetadata {
        TokenMetadata::new(self.line)
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        let literal = &self.source.as_bytes()[self.start..self.current];
        match c {
            // Single character tokens
            '(' => self.add_token(Token::LeftParen(self.ntmd())),
            ')' => self.add_token(Token::RightParen(self.ntmd())),
            '{' => self.add_token(Token::LeftBrace(self.ntmd())),
            '}' => self.add_token(Token::RightBrace(self.ntmd())),
            ',' => self.add_token(Token::Comma(self.ntmd())),
            '.' => self.add_token(Token::Dot(self.ntmd())),
            '-' => self.add_token(Token::Minus(self.ntmd())),
            '+' => self.add_token(Token::Plus(self.ntmd())),
            ';' => self.add_token(Token::Semicolon(self.ntmd())),
            '*' => self.add_token(Token::Star(self.ntmd())),

            // Whitespace
            ' ' | '\r' | '\t' => {},
            '\n' => {self.line += 1},

            // Two character tokens
            '!' => {
                let matched = self.match_char('=');
                self.add_token(if matched {Token::BangEqual(self.ntmd())} else {Token::Bang(self.ntmd())})
            },
            '=' => {
                let matched = self.match_char('=');
                self.add_token(if matched {Token::EqualEqual(self.ntmd())} else {Token::Equal(self.ntmd())})
            },
            '<' => {
                let matched = self.match_char('=');
                self.add_token(if matched {Token::LessEqual(self.ntmd())} else {Token::Less(self.ntmd())})
            },
            '>' => {
                let matched = self.match_char('=');
                self.add_token(if matched {Token::GreaterEqual(self.ntmd())} else {Token::Greater(self.ntmd())})
            },
            '/' => {
                let matched = self.match_char('/');
                if matched {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                }
                else {
                    self.add_token(Token::Slash(self.ntmd()))
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
        while self.peek().is_ascii_alphanumeric() {self.advance();}
        let value = std::str::from_utf8(&self.source.as_bytes()[self.start..self.current]).unwrap();

        if let Some(keyword) = self.identifier_table.get(value) {
            self.add_token(keyword(self.ntmd()));
        }
        else {
            self.add_token(Token::Identifier(TokenTextValueMetadata{
                metadata: self.ntmd(),
                lexeme: value.to_string()
            }));
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
        self.add_token(Token::Number(TokenNumberValueMetadata{
            metadata: self.ntmd(),
            value: value.parse::<f64>().unwrap()
        }));
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
        self.add_token(Token::String(TokenTextValueMetadata {metadata: self.ntmd(), lexeme: value.to_string()}))
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