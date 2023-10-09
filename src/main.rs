mod lox;

extern crate lox_derive;
use lox_derive::{EnumStrings};
use crate::lox::Token;

pub trait HelloWorld {
    fn hello_world();
}

pub trait hello {
    fn hi(&self);
}

struct Toppings {
    x: i32
}

struct Sides {
    y: f32
}

impl hello for Sides {
    fn hi(&self) {
        println!("Hi from sides");
    }
}

impl hello for Toppings {
    fn hi(&self) {
        println!("Hi from toppings");
    }
}

#[derive(EnumStrings)]
enum Pancakes {
    Single(Toppings),
    Doulble(Sides),
    Triples(Toppings, Sides),
    Eof
}

fn main() {
    let a = Token::LeftParen(lox::TokenMetadata { line: 0 });
    let b= Pancakes::Triples(Toppings{x:0}, Sides{y:0.0});
    b.hi();
    println!("Token {}", a.print_token_name());
}