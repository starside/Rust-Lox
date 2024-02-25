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
use crate::parser::Parser;
use crate::{scanner};
use crate::interpreter::{Interpreter};
use crate::lox::ast::statement::{Accept as StatementAccept};

pub trait EnumVectorize {
    fn enum_to_vector(&self) -> Vec<String>;
}

trait EnumElement {
    fn enum_to_element(&self) -> String;
}

#[derive(Clone)]
pub struct TokenMetadata {
    pub line: usize
}

impl TokenMetadata {
    pub fn new(line: usize) ->TokenMetadata {
        TokenMetadata{line}
    }
}

#[derive(Clone)]
pub struct TokenTextValueMetadata {
    pub metadata: TokenMetadata,
    pub lexeme: String
}

#[derive(Clone)]
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

#[derive(EnumStrings, Clone)]
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
    pub mod expression{
        use crate::lox::{Token, TokenTextValueMetadata};
        use lox_derive_ast::derive_ast;

        #[derive(Clone, Debug)]
        pub enum LiteralValue {
            String(String),
            Number(f64),
            Boolean(bool),
            Nil
        }

        derive_ast!(
            Ast/Expr/
            Assign: Token name, Expr value;
            Binary : Expr left, Token operator, Expr right;
            Grouping : Expr expression;
            Literal : LiteralValue value;
            Logical :  Expr left, Token operator, Expr right;
            Unary : Token operator, Expr right;
            Variable: TokenTextValueMetadata name;
        );
    }
    pub mod statement{
        use lox_derive_ast::derive_ast;
        use crate::lox::ast::{expression};
        use crate::lox::TokenTextValueMetadata;
        type Expr = expression::Expr;

        derive_ast!(
            Stmt/Stmt/
            Block: StmtList statements;
            Expression : Expr expression;
            If: Expr condition, Stmt then_branch, Stmt else_branch;
            Print : Expr expression;
            Var : TokenTextValueMetadata name, Expr initializer;
            While : Expr condition, Stmt body;
        );

        type StmtList = Vec<Stmt>;
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let v: Vec<String> = self.enum_to_vector();
        write!(f, "{}", v.join(" "))
    }
}

fn run(source: &str) -> Result<(), Box<dyn Error>>{
    let mut scanner = scanner::Scanner::new(source);
    let tokens = scanner.scan_tokens();
    match tokens {
        Ok(tokens) => {
            let mut parser = Parser::new(tokens);
            match parser.parse() {
                Ok(statements) => {
                    let mut interpreter = Interpreter::new();
                    for s in statements {
                        if let Err(err) = s.accept(&mut interpreter) {
                            println!("Runtime Error: {}", err);
                        }
                    }
                }
                Err(err) => {
                    let (_, b) = err;
                    println!("Parse Error: {}", b);
                }
            }
        }
        Err(errors) => {
            for error in errors {
                println!("Tokenizer error on line {}: {}", error.line, error.message);
            }
        }
    }
    Ok(())
}

pub fn run_file(path: std::path::PathBuf) -> Result<(), Box<dyn Error>> {
    let mut data: Vec<u8> = Vec::new();
    File::open(path)?.read_to_end(&mut data)?;
    let source_code = str::from_utf8(&*data)?;
    run(source_code)?;
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