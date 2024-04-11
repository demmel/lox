use std::fmt::Display;

use justerror::Error;

use crate::ast::{Expression, InfixOperator, Literal, UnaryOperator};

#[derive(Debug)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
        }
    }
}

#[Error]
pub enum InterpretError {
    InvalidLess(Value, Value),
    InvalidLessEqual(Value, Value),
    InvalidGreater(Value, Value),
    InvalidGreaterEqual(Value, Value),
    InvalidAdd(Value, Value),
    InvalidSub(Value, Value),
    InvalidMult(Value, Value),
    InvalidDiv(Value, Value),
    InvalidNegate(Value, Value),
    InvalidNot(Value),
}

pub fn evaluate(expression: &Expression) -> Result<Value, InterpretError> {
    match expression {
        Expression::Literal(literal) => match literal {
            Literal::Number(n) => Ok(Value::Number(*n)),
            Literal::String(s) => Ok(Value::String(s.clone())),
            Literal::Boolean(b) => Ok(Value::Boolean(*b)),
            Literal::Nil => Ok(Value::Nil),
        },
        Expression::Grouping(x) => evaluate(&x),
        Expression::Binary(a, op, b) => {
            let a = evaluate(&a)?;
            let b = evaluate(&b)?;
            match op {
                InfixOperator::Equal => match (a, b) {
                    (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a == b)),
                    (Value::String(a), Value::String(b)) => Ok(Value::Boolean(a == b)),
                    (Value::Boolean(a), Value::Boolean(b)) => Ok(Value::Boolean(a == b)),
                    (Value::Nil, Value::Nil) => Ok(Value::Boolean(true)),
                    _ => Ok(Value::Boolean(false)),
                },
                InfixOperator::NotEqual => match (a, b) {
                    (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a != b)),
                    (Value::String(a), Value::String(b)) => Ok(Value::Boolean(a != b)),
                    (Value::Boolean(a), Value::Boolean(b)) => Ok(Value::Boolean(a != b)),
                    (Value::Nil, Value::Nil) => Ok(Value::Boolean(false)),
                    _ => Ok(Value::Boolean(true)),
                },
                InfixOperator::LessThan => match (a, b) {
                    (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a < b)),
                    (a, b) => Err(InterpretError::InvalidLess(a, b)),
                },
                InfixOperator::LessThanOrEqual => match (a, b) {
                    (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a <= b)),
                    (a, b) => Err(InterpretError::InvalidLessEqual(a, b)),
                },
                InfixOperator::GreaterThan => match (a, b) {
                    (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a > b)),
                    (a, b) => Err(InterpretError::InvalidGreater(a, b)),
                },
                InfixOperator::GreaterThanOrEqual => match (a, b) {
                    (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a >= b)),
                    (a, b) => Err(InterpretError::InvalidGreaterEqual(a, b)),
                },
                InfixOperator::Plus => match (a, b) {
                    (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
                    (Value::String(a), Value::String(b)) => {
                        Ok(Value::String(format!("{}{}", a, b)))
                    }
                    (a, b) => Err(InterpretError::InvalidAdd(a, b)),
                },
                InfixOperator::Minus => match (a, b) {
                    (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
                    (a, b) => Err(InterpretError::InvalidSub(a, b)),
                },
                InfixOperator::Multiply => match (a, b) {
                    (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
                    (a, b) => Err(InterpretError::InvalidMult(a, b)),
                },
                InfixOperator::Divide => match (a, b) {
                    (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a / b)),
                    (a, b) => Err(InterpretError::InvalidDiv(a, b)),
                },
            }
        }
        Expression::Unary(op, x) => {
            let x = evaluate(&x)?;
            match op {
                UnaryOperator::Negate => match x {
                    Value::Number(n) => Ok(Value::Number(-n)),
                    x => Err(InterpretError::InvalidNegate(x, Value::Nil)),
                },
                UnaryOperator::Not => match x {
                    Value::Boolean(b) => Ok(Value::Boolean(!b)),
                    x => Err(InterpretError::InvalidNot(x)),
                },
            }
        }
    }
}
