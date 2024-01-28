use std::str;
use std::io;
use std::error::Error;
use std::fmt;
use std::fmt::Formatter;
use std::io::{Write};

extern crate lox_derive;
extern crate lox_derive_ast;
use lox_derive::EnumStrings;
use crate::lox::ast::{Accept, LiteralValue};
use crate::parser::Parser;
use crate::{lox, scanner};

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
    use crate::lox::Token;
    use lox_derive_ast::derive_ast;

    pub enum LiteralValue {
        String(String),
        Number(f64),
        Nil
    }

    derive_ast!(
        Ast/Expr/
        Binary : ExprBox left, Token operator, ExprBox right;
        Grouping : ExprBox expression;
        Literal : LiteralValue value;
        Unary : Token operator, ExprBox right;
    );

    type ExprBox = Box<Expr>;
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let v: Vec<String> = self.enum_to_vector();
        write!(f, "{}", v.join(" "))
    }
}

struct PrettyPrinter;
impl lox::ast::AstVisitor<String> for PrettyPrinter
{
    fn visit_binary(&mut self, visitor: &lox::ast::Binary) -> String {
        format!("(binary {} {} {})", visitor.left.accept(self), visitor.operator.to_string(), visitor.right.accept(self))
    }

    fn visit_grouping(&mut self, visitor: &lox::ast::Grouping) -> String {
        format!("(grouping {})", visitor.expression.accept(self))
    }

    fn visit_literal(&mut self, visitor: &lox::ast::Literal) -> String {
        let lv = match &visitor.value {
            LiteralValue::String(x) => {x.to_string()}
            LiteralValue::Number(x) => {x.to_string()}
            LiteralValue::Nil => {"Nil".to_string()}
        };
        format!("(literal {})", lv)
    }

    fn visit_unary(&mut self, visitor: &lox::ast::Unary) -> String {
        format!("(unary {} {}  )", visitor.operator.to_string(), visitor.right.accept(self))
    }
}

fn run(source: &str) -> Result<(), Box<dyn Error>>{
    println!("Source = {}", source);
    let mut scanner = scanner::Scanner::new(source);
    let tokens = scanner.scan_tokens();
    match tokens {
        Ok(tokens) => {
            let mut parser = Parser::new(tokens);
            if let Ok(pe) = parser.parse()
            {
                let mut pp = PrettyPrinter;
                println!("AST{}", pe.accept(&mut pp));
            }
            else {
                println!("Parse Error");
            }
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
    let data: Vec<u8> = Vec::new();
    println!("Not loading file {}", path.to_str().unwrap());
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