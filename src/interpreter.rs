use core::panic;
use std::{collections::HashMap, fmt::Display};

use crate::{
    ast::{Expression, InfixOperator, Literal, Statement, UnaryOperator},
    parser, tokenizer,
};

#[derive(Debug, Clone)]
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
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
        }
    }
}

struct Scope {
    variables: HashMap<String, Value>,
}

impl Scope {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    fn get(&self, name: &str) -> Option<&Value> {
        self.variables.get(name)
    }

    fn declare(&mut self, name: String, value: Value) {
        self.variables.insert(name, value);
    }

    fn assign<'a>(&'a mut self, name: &str, value: &Value) -> Option<&'a Value> {
        if let Some(v) = self.variables.get_mut(name) {
            *v = value.clone();
            Some(v)
        } else {
            None
        }
    }
}

struct Stack {
    scopes: Vec<Scope>,
}

impl Stack {
    fn new() -> Self {
        Self {
            scopes: vec![Scope::new()],
        }
    }

    fn push(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn pop(&mut self) {
        self.scopes.pop();
        if self.scopes.is_empty() {
            panic!("Popped the last scope");
        }
    }

    fn get(&self, name: &str) -> Option<&Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        None
    }

    fn declare(&mut self, name: String, value: Value) {
        self.scopes.last_mut().unwrap().declare(name, value);
    }

    fn assign(&mut self, name: &str, value: &Value) -> Option<&Value> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(v) = scope.assign(name, value) {
                return Some(v);
            }
        }
        None
    }
}

pub struct Interpreter {
    stack: Stack,
}

#[justerror::Error]
pub enum InterpretError {
    Tokenize(#[from] tokenizer::TokenizeError),
    Parse(#[from] parser::ParseErrors),
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
    UndeclaredVariable(String),
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
        }
    }

    pub fn interpret(&mut self, source: &str) -> Result<(), InterpretError> {
        let tokens = tokenizer::tokens(source)?;
        let program = parser::program(&tokens)?;

        for stmt in program.0.iter() {
            self.execute(stmt)?;
        }

        Ok(())
    }

    fn execute(&mut self, stmt: &Statement) -> Result<(), InterpretError> {
        match stmt {
            Statement::Expression(expression) => {
                self.evaluate(expression)?;
            }
            Statement::Print(expression) => {
                let value = self.evaluate(expression)?;
                println!("{}", value);
            }
            Statement::VarDeclaration(identifier, expression) => {
                let value = self.evaluate(expression)?;
                self.stack.declare(identifier.clone(), value);
            }
            Statement::Block(statements) => {
                self.stack.push();
                for stmt in statements.iter() {
                    self.execute(stmt)?;
                }
                self.stack.pop();
            }
            Statement::If(condition, then_branch, else_branch) => {
                let condition = self.evaluate(condition)?;
                if is_truthy(&condition) {
                    self.execute(then_branch)?;
                } else if let Some(else_branch) = else_branch {
                    self.execute(else_branch)?;
                }
            }
            Statement::While(condition, body) => loop {
                let condition = self.evaluate(condition)?;
                while is_truthy(&condition) {
                    self.execute(body)?;
                }
            },
        }
        Ok(())
    }

    fn evaluate(&mut self, expression: &Expression) -> Result<Value, InterpretError> {
        match expression {
            Expression::Identifier(identifier) => self
                .stack
                .get(identifier)
                .cloned()
                .ok_or_else(|| InterpretError::UndeclaredVariable(identifier.clone())),
            Expression::Literal(literal) => match literal {
                Literal::Number(n) => Ok(Value::Number(*n)),
                Literal::String(s) => Ok(Value::String(s.clone())),
                Literal::Boolean(b) => Ok(Value::Boolean(*b)),
                Literal::Nil => Ok(Value::Nil),
            },
            Expression::Grouping(x) => self.evaluate(&x),
            Expression::Binary(a, op, b) => {
                let a = self.evaluate(&a)?;
                match op {
                    InfixOperator::Or => {
                        if is_truthy(&a) {
                            return Ok(Value::Boolean(true));
                        }
                        let b = self.evaluate(&b)?;
                        if is_truthy(&b) {
                            return Ok(Value::Boolean(true));
                        } else {
                            return Ok(Value::Boolean(false));
                        }
                    }
                    InfixOperator::And => {
                        if !is_truthy(&a) {
                            return Ok(Value::Boolean(false));
                        }
                        let b = self.evaluate(&b)?;
                        if !is_truthy(&b) {
                            return Ok(Value::Boolean(false));
                        } else {
                            return Ok(Value::Boolean(true));
                        }
                    }
                    InfixOperator::Equal => match (a, self.evaluate(&b)?) {
                        (Value::Number(a), Value::Number(b)) => {
                            Ok(Value::Boolean(if a.is_nan() && b.is_nan() {
                                // Lox treats NaN as equal to itself
                                true
                            } else {
                                a == b
                            }))
                        }
                        (Value::String(a), Value::String(b)) => Ok(Value::Boolean(a == b)),
                        (Value::Boolean(a), Value::Boolean(b)) => Ok(Value::Boolean(a == b)),
                        (Value::Nil, Value::Nil) => Ok(Value::Boolean(true)),
                        _ => Ok(Value::Boolean(false)),
                    },
                    InfixOperator::NotEqual => match (a, self.evaluate(&b)?) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a != b)),
                        (Value::String(a), Value::String(b)) => Ok(Value::Boolean(a != b)),
                        (Value::Boolean(a), Value::Boolean(b)) => Ok(Value::Boolean(a != b)),
                        (Value::Nil, Value::Nil) => Ok(Value::Boolean(false)),
                        _ => Ok(Value::Boolean(true)),
                    },
                    InfixOperator::LessThan => match (a, self.evaluate(&b)?) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a < b)),
                        (a, b) => Err(InterpretError::InvalidLess(a, b)),
                    },
                    InfixOperator::LessThanOrEqual => match (a, self.evaluate(&b)?) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a <= b)),
                        (a, b) => Err(InterpretError::InvalidLessEqual(a, b)),
                    },
                    InfixOperator::GreaterThan => match (a, self.evaluate(&b)?) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a > b)),
                        (a, b) => Err(InterpretError::InvalidGreater(a, b)),
                    },
                    InfixOperator::GreaterThanOrEqual => match (a, self.evaluate(&b)?) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a >= b)),
                        (a, b) => Err(InterpretError::InvalidGreaterEqual(a, b)),
                    },
                    InfixOperator::Plus => match (a, self.evaluate(&b)?) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
                        (Value::String(a), Value::String(b)) => {
                            Ok(Value::String(format!("{}{}", a, b)))
                        }
                        (a, b) => Err(InterpretError::InvalidAdd(a, b)),
                    },
                    InfixOperator::Minus => match (a, self.evaluate(&b)?) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
                        (a, b) => Err(InterpretError::InvalidSub(a, b)),
                    },
                    InfixOperator::Multiply => match (a, self.evaluate(&b)?) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
                        (a, b) => Err(InterpretError::InvalidMult(a, b)),
                    },
                    InfixOperator::Divide => match (a, self.evaluate(&b)?) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a / b)),
                        (a, b) => Err(InterpretError::InvalidDiv(a, b)),
                    },
                }
            }
            Expression::Unary(op, x) => {
                let x = self.evaluate(&x)?;
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
            Expression::Assign(name, expr) => {
                let value = self.evaluate(&expr)?;
                self.stack
                    .assign(name, &value)
                    .cloned()
                    .ok_or_else(|| InterpretError::UndeclaredVariable(name.clone()))
            }
        }
    }
}

fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Nil => false,
        Value::Boolean(b) => *b,
        _ => true,
    }
}
