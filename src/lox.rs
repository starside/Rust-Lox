use std::str;
use std::io;
use std::error::Error;
use std::fmt;
use std::fmt::Formatter;
use std::fs::File;
use std::io::{Read, Write};

extern crate lox_derive;
extern crate lox_derive_ast;
use lox_derive::EnumStrings;
use lox_derive_ast::derive_ast;
use crate::lox::Token::And;
use crate::scanner;
use crate::scanner::ScannerError;

pub trait EnumVectorize {
    fn enum_to_vector(&self) -> Vec<String>;
}

trait EnumElement {
    fn enum_to_element(&self) -> String;
}

pub struct TokenMetadata {
    pub line: usize
}

impl TokenMetadata {
    pub fn new(line: usize) ->TokenMetadata {
        TokenMetadata{line}
    }
}

pub struct TokenTextValueMetadata {
    pub metadata: TokenMetadata,
    pub lexeme: String
}

pub struct TokenNumberValueMetadata {
    pub metadata: TokenMetadata,
    pub value: f64
}

impl EnumElement for TokenMetadata {
    fn enum_to_element(&self) -> String {
        self.line.to_string()
    }
}

impl EnumElement for TokenTextValueMetadata {
    fn enum_to_element(&self) -> String {
        self.lexeme.to_string()
    }

}

impl EnumElement for TokenNumberValueMetadata {
    fn enum_to_element(&self) -> String {
        self.value.to_string()
    }
}

#[derive(EnumStrings)]
pub enum Token {
    // Single-character tokens
    LeftParen(TokenMetadata),
    RightParen(TokenMetadata),
    LeftBrace(TokenMetadata),
    RightBrace(TokenMetadata),
    Comma(TokenMetadata),
    Dot(TokenMetadata),
    Minus(TokenMetadata),
    Plus(TokenMetadata),
    Semicolon(TokenMetadata),
    Slash(TokenMetadata),
    Star(TokenMetadata),

    // One or two character tokens
    Bang(TokenMetadata),
    BangEqual(TokenMetadata),
    Greater(TokenMetadata),
    GreaterEqual(TokenMetadata),
    Less(TokenMetadata),
    LessEqual(TokenMetadata),
    Equal(TokenMetadata),
    EqualEqual(TokenMetadata),

    // Literals
    Identifier(TokenTextValueMetadata),
    String(TokenTextValueMetadata),
    Number(TokenNumberValueMetadata),

    // Keywords
    And(TokenMetadata),
    Class(TokenMetadata),
    Else(TokenMetadata),
    False(TokenMetadata),
    Fun(TokenMetadata),
    For(TokenMetadata),
    If(TokenMetadata),
    Nil(TokenMetadata),
    Or(TokenMetadata),
    Print(TokenMetadata),
    Return(TokenMetadata),
    Super(TokenMetadata),
    This(TokenMetadata),
    True(TokenMetadata),
    Var(TokenMetadata),
    While(TokenMetadata),

    Eof,
}

pub mod ast {
    use crate::lox::Token;
    use lox_derive_ast::derive_ast;

    pub enum LiteralValue {
        String(String),
        Number(f64)
    }

    derive_ast!(
        Ast/Expr/
        Binary : Expr left, Token operator, Expr right;
        Grouping : Expr expression;
        Literal : LiteralValue value;
        Unary : Token operator, Expr right;
    );
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let v: Vec<String> = self.enum_to_vector();
        write!(f, "{}", v.join(" "))
    }
}

fn run(source: &str) -> Result<(), Box<dyn Error>>{
    println!("Source = {}", source);
    let mut scanner = scanner::Scanner::new(source);
    let tokens = scanner.scan_tokens();
    match tokens {
        Ok(tokens) => {
            for token in tokens {
                println!("{}", token);
            }
        }
        Err(errors) => {
            for error in errors {
                println!("Error on line {}: {}", error.line, error.message);
            }
        }
    }
    Ok(())
}

pub fn run_file(path: std::path::PathBuf) -> Result<(), Box<dyn Error>> {
    let mut data: Vec<u8> = Vec::new();
    let data_size = File::open(path)?.read_to_end(&mut data)?;
    let source_code = str::from_utf8(&*data)?;
    run(source_code).expect("TODO: panic message");
    return Ok(())
}

pub fn run_prompt() -> Result<(), Box<dyn Error>> {
    let mut buffer = String::new();
    loop {
        io::stdout().write("> ".as_bytes())?;
        io::stdout().flush()?;
        let bytes_read = io::stdin().read_line(&mut buffer)?;
        if bytes_read == 0 {
            break;
        }
        run(&buffer.trim())?;
        buffer.clear();
    }
    Ok(())
}