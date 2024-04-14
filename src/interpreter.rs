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
enum Callable {
    Function(Vec<String>, Statement),
    Builtin(fn(&[Value]) -> Result<Value, ExecutionError>, usize),
}

impl Callable {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: &[Expression],
    ) -> Result<Value, ExecutionError> {
        match self {
            Callable::Function(arg_names, body) => {
                interpreter.stack.push(true);
                for (arg_name, arg_value) in arg_names.iter().zip(args.iter()) {
                    let arg_value = interpreter.evaluate(arg_value)?;
                    interpreter
                        .stack
                        .declare_variable(arg_name.clone(), arg_value);
                }
                let result = interpreter.execute(body)?;
                interpreter.stack.pop();
                interpreter.stack.set_returning(false);
                Ok(result)
            }
            Callable::Builtin(f, _) => f(&args
                .iter()
                .map(|arg| interpreter.evaluate(arg))
                .collect::<Result<Vec<_>, _>>()?),
        }
    }

    fn arity(&self) -> usize {
        match self {
            Callable::Function(args, _) => args.len(),
            Callable::Builtin(_, arity) => *arity,
        }
    }
}

#[derive(Debug, Clone)]
struct Scope {
    variables: HashMap<String, Value>,
    functions: HashMap<String, Callable>,
    is_function: bool,
}

impl Scope {
    fn new(is_function: bool) -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            is_function,
        }
    }

    fn get_variable(&self, name: &str) -> Option<&Value> {
        self.variables.get(name)
    }

    fn get_function(&self, name: &str) -> Option<&Callable> {
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

    fn declare_function(&mut self, name: &str, callable: Callable) -> Result<(), ExecutionError> {
        self.functions.insert(name.to_string(), callable);
        Ok(())
    }
}

#[derive(Debug, Clone)]
struct Stack {
    scopes: Vec<Scope>,
    is_returning: bool,
}

impl Stack {
    fn new() -> Self {
        Self {
            scopes: vec![Scope::new(false)],
            is_returning: false,
        }
    }

    fn push(&mut self, is_function: bool) {
        self.scopes.push(Scope::new(is_function));
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

    fn get_callable(&self, name: &str) -> Option<&Callable> {
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

    fn declare_function(&mut self, name: &str, callable: Callable) -> Result<(), ExecutionError> {
        self.scopes
            .last_mut()
            .unwrap()
            .declare_function(name, callable)
    }

    fn is_in_function(&self) -> bool {
        self.scopes.iter().any(|scope| scope.is_function)
    }

    fn set_returning(&mut self, is_returning: bool) {
        self.is_returning = is_returning;
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
    #[error("Error executing statement: {current_statement} - {kind:?}")]
    Execution {
        kind: ExecutionError,
        interpreter: Interpreter,
        current_statement: Statement,
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
    CannotReturnFromTopLevel,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut stack = Stack::new();

        stack
            .declare_function(
                "clock",
                Callable::Builtin(
                    |_: &[Value]| {
                        Ok(Value::Number(
                            std::time::SystemTime::now()
                                .duration_since(std::time::UNIX_EPOCH)
                                .unwrap()
                                .as_secs_f64(),
                        ))
                    },
                    0,
                ),
            )
            .expect("Builtin functions should not fail to declare");

        Self { stack }
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
                        current_statement: stmt.clone(),
                    })
                }
            }
        }

        Ok(())
    }

    fn execute(&mut self, stmt: &Statement) -> Result<Value, ExecutionError> {
        let result = match stmt {
            Statement::Expression(expression) => {
                self.evaluate(expression)?;
                Value::Unit
            }
            Statement::Print(expression) => {
                let value = self.evaluate(expression)?;
                println!("{}", value);
                Value::Unit
            }
            Statement::VarDeclaration(identifier, expression) => {
                let value = self.evaluate(expression)?;
                self.stack.declare_variable(identifier.clone(), value);
                Value::Unit
            }
            Statement::Block(statements) => {
                self.stack.push(false);
                let mut res = Value::Unit;
                for stmt in statements.iter() {
                    res = self.execute(stmt)?;
                    if self.stack.is_returning {
                        break;
                    }
                }
                self.stack.pop();
                res
            }
            Statement::If(condition, then_branch, else_branch) => {
                let condition = self.evaluate(condition)?;
                if is_truthy(&condition) {
                    self.execute(then_branch)?
                } else if let Some(else_branch) = else_branch {
                    self.execute(else_branch)?
                } else {
                    Value::Unit
                }
            }
            Statement::While(condition, body) => {
                let mut res = Value::Unit;
                while is_truthy(&self.evaluate(condition)?) {
                    res = self.execute(body)?;
                    if self.stack.is_returning {
                        break;
                    }
                }
                res
            }
            Statement::Function(name, args, body) => {
                self.stack
                    .declare_function(name, Callable::Function(args.to_vec(), (&**body).clone()))?;
                Value::Unit
            }
            Statement::Return(expression) => {
                if !self.stack.is_in_function() {
                    return Err(ExecutionError::CannotReturnFromTopLevel);
                }
                let mut result = Value::Unit;
                if let Some(expression) = expression {
                    result = self.evaluate(expression)?;
                };
                self.stack.set_returning(true);
                result
            }
        };

        Ok(result)
    }

    fn evaluate(&mut self, expression: &Expression) -> Result<Value, ExecutionError> {
        let res = match expression {
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
            Expression::Call(expr, args) => {
                let Expression::Identifier(name) = expr.as_ref() else {
                    return Err(ExecutionError::InvalidFunctionCall(
                        "not an identifier".to_string(),
                        0,
                        0,
                    ));
                };

                let callable = self
                    .stack
                    .get_callable(name)
                    .ok_or_else(|| ExecutionError::UndeclaredFunction(name.clone()))?
                    .clone();

                if args.len() != callable.arity() {
                    return Err(ExecutionError::InvalidFunctionCall(
                        name.clone(),
                        args.len(),
                        callable.arity(),
                    ));
                }

                let result = callable.call(self, args.as_slice())?;

                Ok(result)
            }
        }?;
        Ok(res)
    }
}

fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Nil => false,
        Value::Boolean(b) => *b,
        _ => true,
    }
}
