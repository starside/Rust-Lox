use crate::lox::{ast, Token};
use crate::lox::ast::{Accept, AstVisitor, Binary, Grouping, Literal, LiteralValue, Unary};

pub struct Interpreter;

type RunValue = Result<ast::LiteralValue, String>;

fn is_truthy(value: &ast::LiteralValue) -> bool {
    match value {
        ast::LiteralValue::Nil => false,
        ast::LiteralValue::Boolean(b) => *b,
        _ => true
    }
}

fn is_equal(x: &LiteralValue, y: &LiteralValue) -> bool
{
    match (x, y) {
        (LiteralValue::Nil, LiteralValue::Nil) => { true}
        (LiteralValue::Boolean(a), LiteralValue::Boolean(b)) => {a == b}
        (LiteralValue::Number(a), LiteralValue::Number(b)) => {a == b}
        (LiteralValue::String(a), LiteralValue::String(b)) => {a == b}
        (_, _) => {false}
    }
}

impl AstVisitor<RunValue> for Interpreter {
    fn visit_binary(&mut self, visitor: &Binary) -> RunValue {
        let left = visitor.left.accept(self)?;
        let right = visitor.right.accept(self)?;

        match (&left, &right) {
            // If both operands are strings
            (LiteralValue::String(l), LiteralValue::String(r)) => {
                match visitor.operator {
                    Token::Plus(_) => {
                        let mut new_str = l.to_string();
                        new_str.push_str(r);
                        Ok(LiteralValue::String(new_str))
                    }

                    Token::BangEqual(_) => {
                        Ok(LiteralValue::Boolean(!is_equal(&left, &right)))
                    }

                    Token::EqualEqual(_) => {
                        Ok(LiteralValue::Boolean(is_equal(&left, &right)))
                    }
                    _ => {
                        Err("Invalid binary operation between strings".to_string())
                    }
                }

            }

            // If Both operands are numbers
            (LiteralValue::Number(l), LiteralValue::Number(r)) => {
                match visitor.operator {
                    Token::Plus(_) => {Ok(LiteralValue::Number(l+r))}
                    Token::Minus(_) => {Ok(LiteralValue::Number(l-r))}
                    Token::Star(_) => {Ok(LiteralValue::Number(l*r))}
                    Token::Slash(_) => {Ok(LiteralValue::Number(l/r))}
                    Token::BangEqual(_) => {Ok(LiteralValue::Boolean(l != r))}
                    Token::EqualEqual(_) => {Ok(LiteralValue::Boolean(l == r))}
                    Token::Greater(_) => {Ok(LiteralValue::Boolean(l > r))}
                    Token::GreaterEqual(_) => {Ok(LiteralValue::Boolean(l >= r))}
                    Token::Less(_) => {Ok(LiteralValue::Boolean(l < r))}
                    Token::LessEqual(_) => {Ok(LiteralValue::Boolean(l <= r))}
                    _ => Err("Unknown operand between two Numbers".to_string())
                }
            }

            _ => {
                Err("Operands must be two numbers or two strings".to_string())
            }
        }
    }

    fn visit_grouping(&mut self, visitor: &Grouping) -> RunValue {
        visitor.expression.accept(self)
    }

    fn visit_literal(&mut self, visitor: &Literal) -> RunValue {
        Ok(visitor.value.clone())
    }
    fn visit_unary(&mut self, visitor: &Unary) -> RunValue {
        let right = visitor.right.accept(self)?;

        let value = match visitor.operator {
            Token::Minus(_) => {
                if let ast::LiteralValue::Number(x) = right {
                    Ok(ast::LiteralValue::Number(-x))
                }
                else {
                    Err("Unary minus must operate on a number".to_string())
                }
            }

            Token::Bang(_) => {
                Ok(ast::LiteralValue::Boolean(!is_truthy(&right)))
            }
            _ => Err("Unknown unary operator".to_string())
        };

        value
    }
}