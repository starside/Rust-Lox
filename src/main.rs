mod lox;
mod scanner;

extern crate lox_derive;

use std::error::Error;
use crate::lox::{run_prompt, run_file, Token, TokenMetadata};
use clap;
use clap::Parser;
use crate::lox::Ast::{Accept, LiteralValue};

#[derive(clap::Parser)]
struct CommandLine {
    script: Option<std::path::PathBuf>
}

struct PrettyPrinter;
impl lox::Ast::AstVisitor<String> for PrettyPrinter
{
    fn visit_binary(&mut self, visitor: &lox::Ast::Binary) -> String {
        todo!()
    }

    fn visit_grouping(&mut self, visitor: &lox::Ast::Grouping) -> String {
        todo!()
    }

    fn visit_literal(&mut self, visitor: &lox::Ast::Literal) -> String {
        match &visitor.value {
            LiteralValue::String(x) => {x.to_string()}
            LiteralValue::Number(x) => {x.to_string()}
        }
    }

    fn visit_unary(&mut self, visitor: &lox::Ast::Unary) -> String {
        format!("(unary {} {}  )", visitor.operator.to_string(), visitor.right.accept(self))
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = CommandLine::parse();

    let unary = lox::Ast::Unary{
        operator: Token::Minus(TokenMetadata::new(1)) ,
        right: lox::Ast::Expr::Literal(Box::new((
            lox::Ast::Literal{value: lox::Ast::LiteralValue::Number(123.0)}
            )))
    };

    let mut pp = PrettyPrinter;
    println!("Pretty printer {}", unary.accept(&mut pp));

    if let Some(path) = args.script {
        run_file(path)?;
    }
    else {
        run_prompt()?;
    }
    Ok(())
}