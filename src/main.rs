mod lox;
mod scanner;

extern crate lox_derive;

use std::error::Error;
use crate::lox::{run_prompt, run_file, Token, TokenMetadata};
use clap;
use clap::Parser;
use crate::lox::ast::{Accept, LiteralValue};

#[derive(clap::Parser)]
struct CommandLine {
    script: Option<std::path::PathBuf>
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
        match &visitor.value {
            LiteralValue::String(x) => {x.to_string()}
            LiteralValue::Number(x) => {x.to_string()}
        }
    }

    fn visit_unary(&mut self, visitor: &lox::ast::Unary) -> String {
        format!("(unary {} {}  )", visitor.operator.to_string(), visitor.right.accept(self))
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = CommandLine::parse();

    let unary = lox::ast::Unary{
        operator: Token::Minus(TokenMetadata::new(1)) ,
        right: lox::ast::Expr::Literal(Box::new((
            lox::ast::Literal{value: lox::ast::LiteralValue::Number(123.0)}
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