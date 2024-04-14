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
    Unit,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
            Value::Unit => write!(f, "()"),
        }
    }
}

#[derive(Debug, Clone)]
struct Scope {
    variables: HashMap<String, Value>,
    functions: HashMap<String, (Vec<String>, Statement)>,
}

impl Scope {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    fn get_variable(&self, name: &str) -> Option<&Value> {
        self.variables.get(name)
    }

    fn get_function(&self, name: &str) -> Option<&(Vec<String>, Statement)> {
        self.functions.get(name)
    }

    fn declare_variable(&mut self, name: String, value: Value) {
        self.variables.insert(name, value);
    }

    fn assign_variable<'a>(&'a mut self, name: &str, value: &Value) -> Option<&'a Value> {
        if let Some(v) = self.variables.get_mut(name) {
            *v = value.clone();
            Some(v)
        } else {
            None
        }
    }

    fn declare_function(
        &mut self,
        name: &str,
        args: &[String],
        body: &Statement,
    ) -> Result<(), ExecutionError> {
        self.functions
            .insert(name.to_string(), (args.to_vec(), body.clone()));
        Ok(())
    }
}

#[derive(Debug, Clone)]
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

    fn get_variable(&self, name: &str) -> Option<&Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get_variable(name) {
                return Some(value);
            }
        }
        None
    }

    fn get_function(&self, name: &str) -> Option<&(Vec<String>, Statement)> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get_function(name) {
                return Some(value);
            }
        }
        None
    }

    fn declare_variable(&mut self, name: String, value: Value) {
        self.scopes
            .last_mut()
            .unwrap()
            .declare_variable(name, value);
    }

    fn assign_variable(&mut self, name: &str, value: &Value) -> Option<&Value> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(v) = scope.assign_variable(name, value) {
                return Some(v);
            }
        }
        None
    }

    fn declare_function(
        &mut self,
        name: &str,
        args: &[String],
        body: &Statement,
    ) -> Result<(), ExecutionError> {
        self.scopes
            .last_mut()
            .unwrap()
            .declare_function(name, args, body)
    }
}

#[derive(Debug, Clone)]
pub struct Interpreter {
    stack: Stack,
}

#[derive(Debug, thiserror::Error)]
pub enum InterpretError {
    #[error("{0}")]
    Tokenize(#[from] tokenizer::TokenizeError),
    #[error("{0}")]
    Parse(#[from] parser::ParseErrors),
    #[error("Error executing statement: {current_statement:?} - {kind:?}\nInterptreter State\n{interpreter:#?}")]
    Execution {
        kind: ExecutionError,
        interpreter: Interpreter,
        current_statement: Option<Statement>,
    },
}

#[justerror::Error]
pub enum ExecutionError {
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
    UndeclaredFunction(String),
    InvalidFunctionCall(String, usize, usize),
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
            match self.execute(stmt) {
                Ok(_) => {}
                Err(e) => {
                    return Err(InterpretError::Execution {
                        kind: e,
                        interpreter: self.clone(),
                        current_statement: Some(stmt.clone()),
                    })
                }
            }
        }

        Ok(())
    }

    fn execute(&mut self, stmt: &Statement) -> Result<Value, ExecutionError> {
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
                self.stack.declare_variable(identifier.clone(), value);
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
            Statement::While(condition, body) => {
                while is_truthy(&self.evaluate(condition)?) {
                    self.execute(body)?;
                }
            }
            Statement::Function(name, args, body) => {
                self.stack.declare_function(name, args, body)?;
            }
        }
        Ok(Value::Unit)
    }

    fn evaluate(&mut self, expression: &Expression) -> Result<Value, ExecutionError> {
        match expression {
            Expression::Identifier(identifier) => self
                .stack
                .get_variable(identifier)
                .cloned()
                .ok_or_else(|| ExecutionError::UndeclaredVariable(identifier.clone())),
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
                        (a, b) => Err(ExecutionError::InvalidLess(a, b)),
                    },
                    InfixOperator::LessThanOrEqual => match (a, self.evaluate(&b)?) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a <= b)),
                        (a, b) => Err(ExecutionError::InvalidLessEqual(a, b)),
                    },
                    InfixOperator::GreaterThan => match (a, self.evaluate(&b)?) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a > b)),
                        (a, b) => Err(ExecutionError::InvalidGreater(a, b)),
                    },
                    InfixOperator::GreaterThanOrEqual => match (a, self.evaluate(&b)?) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a >= b)),
                        (a, b) => Err(ExecutionError::InvalidGreaterEqual(a, b)),
                    },
                    InfixOperator::Plus => match (a, self.evaluate(&b)?) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
                        (Value::String(a), Value::String(b)) => {
                            Ok(Value::String(format!("{}{}", a, b)))
                        }
                        (a, b) => Err(ExecutionError::InvalidAdd(a, b)),
                    },
                    InfixOperator::Minus => match (a, self.evaluate(&b)?) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
                        (a, b) => Err(ExecutionError::InvalidSub(a, b)),
                    },
                    InfixOperator::Multiply => match (a, self.evaluate(&b)?) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
                        (a, b) => Err(ExecutionError::InvalidMult(a, b)),
                    },
                    InfixOperator::Divide => match (a, self.evaluate(&b)?) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a / b)),
                        (a, b) => Err(ExecutionError::InvalidDiv(a, b)),
                    },
                }
            }
            Expression::Unary(op, x) => {
                let x = self.evaluate(&x)?;
                match op {
                    UnaryOperator::Negate => match x {
                        Value::Number(n) => Ok(Value::Number(-n)),
                        x => Err(ExecutionError::InvalidNegate(x, Value::Nil)),
                    },
                    UnaryOperator::Not => match x {
                        Value::Boolean(b) => Ok(Value::Boolean(!b)),
                        x => Err(ExecutionError::InvalidNot(x)),
                    },
                }
            }
            Expression::Assign(name, expr) => {
                let value = self.evaluate(&expr)?;
                self.stack
                    .assign_variable(name, &value)
                    .cloned()
                    .ok_or_else(|| ExecutionError::UndeclaredVariable(name.clone()))
            }
            Expression::FunctionCall(name, args) => {
                let (arg_names, body) = self
                    .stack
                    .get_function(name)
                    .ok_or_else(|| ExecutionError::UndeclaredFunction(name.clone()))?
                    .clone();
                if args.len() != arg_names.len() {
                    return Err(ExecutionError::InvalidFunctionCall(
                        name.clone(),
                        args.len(),
                        arg_names.len(),
                    ));
                }
                self.stack.push();
                for (arg_name, arg_value) in arg_names.iter().zip(args.iter()) {
                    let evaluated_arg = self.evaluate(arg_value)?;
                    self.stack.declare_variable(arg_name.clone(), evaluated_arg);
                }
                let result = self.execute(&body)?;
                self.stack.pop();
                Ok(result)
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
