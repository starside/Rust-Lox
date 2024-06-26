use std::str;
use std::io;
use std::fs::File;
use std::io::{Read, Write};
use std::rc::Rc;
use std::str::Utf8Error;
use enum_kinds::EnumKind;

extern crate lox_derive_ast;
use crate::parser::{Parser, ParserError};
use crate::{scanner};
use crate::interpreter::{Interpreter, Unwinder};
use crate::lox::ast::statement::{Accept as StatementAccept};
use crate::resolver::{Resolver, ResolverError};

pub(crate) type RunString = Rc<String>;

#[derive(Clone, Debug)]
pub struct Token {
    pub line: usize,
    pub token_type: TokenType,
    pub lexeme: String
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
    Identifier(RunString),
    String(RunString),
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
    use crate::lox::RunString;

    #[derive(Clone, Debug, PartialEq)]
    pub enum LiteralValue {
        String(RunString),
        Number(f64),
        Boolean(bool),
        Nil
    }

    //type VarName = String;
    #[derive(Clone)]
    pub struct VarName {
        pub lexeme: RunString,
        pub line: usize
    }

    impl VarName {
        pub fn new(name: &RunString, line: usize) -> Self {
            VarName {
                lexeme: name.clone(),
                line
            }
        }
    }

    pub mod expression{
        use std::pin::Pin;
        use crate::lox::{Token};
        use super::{LiteralValue, VarName};
        use lox_derive_ast::derive_ast;

        derive_ast!(
            Ast/Expr/
            Assign: Expr name, Expr value;
            Binary : Expr left, Token operator, Expr right;
            Call: Expr callee, Token paren, ExprList arguments;
            Get: Expr object, Token name;
            Grouping : Expr expression;
            Literal : LiteralValue value;
            Logical :  Expr left, Token operator, Expr right;
            Set: Expr object, Token name, Expr value;
            Super: Token keyword, Token method;
            This: Token keyword;
            Unary : Token operator, Expr right;
            Variable: VarName name;
        );

        impl Default for Expr {
            fn default() -> Self {
                Expr::Empty
            }
        }
        type ExprList = Vec<Expr>;
    }
    pub mod statement{
        use std::rc::Rc;
        use crate::lox::ast::VarName;
        use lox_derive_ast::derive_ast;
        use crate::lox::ast::{expression};
        use crate::lox::Token;
        use std::pin::Pin;

        type Expr = expression::Expr;
        type OptionalExpr = Option<Expr>;

        derive_ast!(
            Stmt/Stmt/
            Block: StmtList statements;
            Class : Token name, FuncList methods, OptionalExpr superclass;
            Expression : Expr expression;
            Function: Token name, TokenList params, FuncBody body;
            If: Expr condition, Stmt then_branch, Stmt else_branch;
            Print : Expr expression;
            Return : Token keyword, Expr value;
            Var : VarName name, Expr initializer;
            While : Expr condition, Stmt body;
        );

        pub(crate) type StmtList = Vec<Stmt>;
        pub type FuncList = Vec<Pin<Box<Function>>>;
        type TokenList = Vec<Token>;
        pub type FuncBody = Rc<Pin<Box<Stmt>>>; // Can I eliminate this with lifetimes?
    }
}

fn run(source: &str) -> Result<(), RunErrorType>{
    let mut scanner = scanner::Scanner::new(source);
    let (tokens, scan_errors) = scanner.scan_tokens();
    let mut parser = Parser::new(tokens);
    let parse_results = parser.parse();

    let (statements, parse_errors) = match parse_results {
        Ok(x) => {
            (x, Vec::new())
        }
        Err(e) => {
            (Vec::new(), e)
        }
    };

    // Bail if there was a scanner or parser error
    if !scan_errors.is_empty() || !parse_errors.is_empty(){
        let scanner_report: Vec<(usize, String)> =
            scan_errors.iter().map(|x| {
                (x.line, x.message.clone())
            }).collect();
        return Err(RunErrorType::ScannerParser(scanner_report, parse_errors))
    }

    let mut interpreter = Interpreter::new();
    let mut resolver: Resolver = Resolver::new(&mut interpreter);

    // Run Resolver
    let resolve_error: Vec<ResolverError> = statements.iter().filter_map(|stmt| {
        match resolver.resolve_statement(stmt) {
            Ok(_) => {None}
            Err(e) => {
                Some(e)
            }
        }
    }).collect();

    if !resolve_error.is_empty(){
        return Err(RunErrorType::Resolver(resolve_error))
    }

    // Run interpreter
    for s in statements {
        if let Err(unwind) = s.accept(&mut interpreter) {
            match unwind {
                Unwinder::RuntimeError(err) => {
                    let e = format!("{}\n[line {}]", err.msg, err.line);
                    return Err(RunErrorType::Interpreter(e));
                }
                Unwinder::ReturnValue(_) => {
                    eprintln!("Interpreter returned a value");
                }
            }

        }
    }
    Ok(())
}

pub enum RunErrorType {
    ScannerParser(Vec<(usize, String)>, Vec<ParserError>),
    Resolver(Vec<ResolverError>),
    Interpreter(String),
    IOError(String)
}

impl From<std::io::Error> for RunErrorType {
    fn from(value: io::Error) -> Self {
        RunErrorType::IOError(value.to_string())
    }
}

impl From<Utf8Error> for RunErrorType {
    fn from(value: Utf8Error) -> Self {
        RunErrorType::IOError(value.to_string())
    }
}

pub fn run_file(path: std::path::PathBuf) -> Result<(), RunErrorType> {
    let mut data: Vec<u8> = Vec::new();
    File::open(path)?.read_to_end(&mut data)?;
    let source_code = str::from_utf8(&data)?;
    run(source_code)?;
    Ok(())
}

pub fn run_prompt() -> Result<(), RunErrorType> {
    let mut buffer = String::new();
    loop {
        io::stdout().write_all("> ".as_ref())?;
        io::stdout().flush()?;
        let bytes_read = io::stdin().read_line(&mut buffer)?;
        if bytes_read == 0 {
            break;
        }
        run(buffer.trim())?;
        buffer.clear();
    }
    Ok(())
}