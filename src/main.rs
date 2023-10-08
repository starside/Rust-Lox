extern crate lox_derive;
use lox_derive::HelloWorld;

pub trait HelloWorld {
    fn hello_world();
}

#[derive(HelloWorld)]
enum Pancakes {
    Single,
    Doulble
}

fn main() {
    Pancakes::hello_world();
}