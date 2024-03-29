use crate::lox::{Token, TokenType};
use rustc_hash::{FxHashMap};
type HashMap<K, V> = FxHashMap<K, V>;
use std::rc::Rc;

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
        let mut identifiers:HashMap<&str, TokenType> = HashMap::default();

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

    pub fn scan_tokens(&mut self) -> (&Vec<Token>, &Vec<ScannerError>){
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }
        self.add_token(self.new_token(
            TokenType::Eof,
            "".to_string()
        ));
        (&self.tokens, &self.errors)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    // Abbreviated for New Token Meta Data
    fn new_token(&self, token_type: TokenType, lexeme: String) -> Token {
        Token {
            line: self.line,
            token_type,
            lexeme
        }
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        let c_str = c.to_string();
        match c {
            // Single character tokens
            '(' => self.add_token(self.new_token(TokenType::LeftParen, c_str)),
            ')' => self.add_token(self.new_token(TokenType::RightParen, c_str)),
            '{' => self.add_token(self.new_token(TokenType::LeftBrace, c_str)),
            '}' => self.add_token(self.new_token(TokenType::RightBrace, c_str)),
            ',' => self.add_token(self.new_token(TokenType::Comma, c_str)),
            '.' => self.add_token(self.new_token(TokenType::Dot, c_str)),
            '-' => self.add_token(self.new_token(TokenType::Minus, c_str)),
            '+' => self.add_token(self.new_token(TokenType::Plus, c_str)),
            ';' => self.add_token(self.new_token(TokenType::Semicolon, c_str)),
            '*' => self.add_token(self.new_token(TokenType::Star, c_str)),

            // Whitespace
            ' ' | '\r' | '\t' => {},
            '\n' => {self.line += 1},

            // Two character tokens
            '!' => {
                let matched = self.match_char('=');
                self.add_token(if matched {self.new_token(TokenType::BangEqual, "!=".to_string())}
                else {self.new_token(TokenType::Bang, "!".to_string())})
            },
            '=' => {
                let matched = self.match_char('=');
                self.add_token(if matched {self.new_token(TokenType::EqualEqual, "==".to_string())}
                else {self.new_token(TokenType::Equal, "=".to_string())})
            },
            '<' => {
                let matched = self.match_char('=');
                self.add_token(if matched {self.new_token(TokenType::LessEqual, "<=".to_string())}
                else {self.new_token(TokenType::Less, "<".to_string())})
            },
            '>' => {
                let matched = self.match_char('=');
                self.add_token(if matched {self.new_token(TokenType::GreaterEqual, ">=".to_string())}
                else {self.new_token(TokenType::Greater, ">".to_string())})
            },
            '/' => {
                let matched = self.match_char('/');
                if matched {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                }
                else {
                    self.add_token(self.new_token(TokenType::Slash, "/".to_string()))
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
            _ => self.error("Unexpected character.".to_string())
        };
    }

    fn identifier(&mut self) {
        while self.peek().is_ascii_alphanumeric() || self.peek() == '_' {self.advance();}
        let value = std::str::from_utf8((self.source[self.start..self.current]).as_ref()).unwrap();

        if let Some(keyword) = self.identifier_table.get(value) {
            self.add_token(self.new_token(keyword.clone(), value.to_string()));
        }
        else {
            self.add_token(self.new_token(
                TokenType::Identifier(Rc::new(value.to_string())),
                value.to_string()
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
        let value = std::str::from_utf8((self.source[self.start..self.current]).as_ref()).unwrap();
        self.add_token(self.new_token(
            TokenType::Number(value.parse::<f64>().unwrap()),
            value.to_string()
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
            self.error("Unterminated string.".to_string());
            return;
        }

        // The closing "
        self.advance();
        let value = std::str::from_utf8((self.source[(self.start+1)..(self.current-1)]).as_ref()).unwrap();
        self.add_token(self.new_token(
            TokenType::String(Rc::new(value.to_string())),
            value.to_string()
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