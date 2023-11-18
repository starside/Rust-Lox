use crate::lox;
use crate::lox::{Token, TokenMetadata, TokenTextValueMetadata};

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
                if(matched) {
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

            // Error
            _ => self.error(format!("Unexpected character {}", c))
        };
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
        self.add_token(Token::String(TokenTextValueMetadata {metadata: self.ntmd(), lexeme: value}))
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return 0 as char;
        }
        self.source.as_bytes()[self.current] as char
    }

    fn add_token(&mut self, token: Token<'s>) {
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