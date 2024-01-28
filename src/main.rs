mod lox;
mod scanner;
mod parser;

extern crate lox_derive;

use std::error::Error;
use crate::lox::{run_prompt, run_file};
use clap;
use clap::Parser;

#[derive(clap::Parser)]
struct CommandLine {
    script: Option<std::path::PathBuf>
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = CommandLine::parse();

    /*let unary = lox::ast::Unary{
        operator: Token::Minus(TokenMetadata::new(1)) ,
        right: lox::ast::Expr::Literal(Box::new((
            lox::ast::Literal{value: lox::ast::LiteralValue::Number(123.0)}
            )))
    };*/

    //let mut pp = PrettyPrinter;
    //println!("Pretty printer {}", unary.accept(&mut pp));

    if let Some(path) = args.script {
        run_file(path)?;
    }
    else {
        run_prompt()?;
    }
    Ok(())
}