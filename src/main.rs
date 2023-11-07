mod lox;

extern crate lox_derive;
use lox_derive::{EnumStrings};
use crate::lox::{enum_vectorize, Token};

fn main() {
    let a = Token::LeftParen(lox::TokenMetadata { line: 0 });
    println!("{:?}", a.enum_to_vector());

    let b = Token::Identifier(lox::TokenTextValueMetadata{
        metadata: lox::TokenMetadata{line: 0},
        lexeme: &"ruger"
    });

    println!("{:?}", b.enum_to_vector());
}