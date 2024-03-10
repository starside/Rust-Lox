use std::str;
use std::io;
use std::error::Error;
use std::fs::File;
use std::io::{Read, Write};
use enum_kinds::EnumKind;

extern crate lox_derive_ast;
use crate::parser::Parser;
use crate::{scanner};
use crate::interpreter::{Interpreter, Unwinder};
use crate::lox::ast::statement::{Accept as StatementAccept};
use crate::resolver::Resolver;

#[derive(Clone, Debug)]
pub struct Token {
    pub line: usize,
    pub token_type: TokenType
}

#[derive(Clone, EnumKind, PartialEq, Debug)]
#[enum_kind(TokenKind)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Equal,
    EqualEqual,

    // Literals
    Identifier(String),
    String(String),
    Number(f64),

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    // End Of File
    Eof,
}

pub mod ast {
    #[derive(Clone, Debug)]
    pub enum LiteralValue {
        String(String),
        Number(f64),
        Boolean(bool),
        Nil
    }

    type VarName = String;

    pub mod expression{
        use crate::lox::{Token};
        use super::{LiteralValue, VarName};
        use lox_derive_ast::derive_ast;

        derive_ast!(
            Ast/Expr/
            Assign: Expr name, Expr value;
            Binary : Expr left, Token operator, Expr right;
            Call: Expr callee, Token paren, ExprList arguments;
            Grouping : Expr expression;
            Literal : LiteralValue value;
            Logical :  Expr left, Token operator, Expr right;
            Unary : Token operator, Expr right;
            Variable: VarName name;
        );
        type ExprList = Vec<Expr>;
    }
    pub mod statement{
        use std::rc::Rc;
        use crate::lox::ast::VarName;
        use lox_derive_ast::derive_ast;
        use crate::lox::ast::{expression};
        use crate::lox::Token;

        type Expr = expression::Expr;

        derive_ast!(
            Stmt/Stmt/
            Block: StmtList statements;
            Expression : Expr expression;
            Function: Token name, TokenList params, FuncBody body;
            If: Expr condition, Stmt then_branch, Stmt else_branch;
            Print : Expr expression;
            Return : Token keyword, Expr value;
            Var : VarName name, Expr initializer;
            While : Expr condition, Stmt body;
        );

        pub(crate) type StmtList = Vec<Stmt>;
        type TokenList = Vec<Token>;
        pub type FuncBody = Rc<Box<Stmt>>; // Can I eliminate this with lifetimes?
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
                    let resolver: Resolver = Resolver::new(&mut interpreter);
                    for s in statements {
                        if let Err(unwind) = s.accept(&mut interpreter) {
                            match unwind {
                                Unwinder::RuntimeError(err) => {println!("Runtime Error: {}", err);}
                                Unwinder::ReturnValue(_) => {
                                    println!("Interpreter returned a value");
                                }
                            }

                        }
                    }
                }
                Err(errs) => {
                    for err in errs {
                        let (t, b) = err;
                        println!("Parse Error on line {}: {}", t.line, b);
                    }
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