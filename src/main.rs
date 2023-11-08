mod lox;

extern crate lox_derive;
use crate::lox::{EnumVectorize, Token};

fn main() {
    let a = Token::LeftParen(lox::TokenMetadata { line: 0 });
    println!("{}", a);

    let b = Token::Identifier(lox::TokenTextValueMetadata{
        metadata: lox::TokenMetadata{line: 0},
        lexeme: &"ruger"
    });

    println!("{}", b);
}