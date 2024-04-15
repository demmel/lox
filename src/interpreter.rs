mod environment;

use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::ast::{Expression, InfixOperator, Literal, Program, Statement, UnaryOperator};

use self::environment::{Declarable, Environment, Scope};

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Closure(Callable),
    Nil,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Closure(c) => write!(f, "<function {}>", c.arity()),
            Value::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Clone)]
pub struct Interpreter {
    environment: Environment,
    stdout: Rc<RefCell<dyn std::io::Write>>,
}

impl Debug for Interpreter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Interpreter")
            .field("environment", &self.environment)
            .finish()
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new(Rc::new(RefCell::new(std::io::stdout())))
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ExecutionError {
    #[error("Error executing statement: {current_statement} - {kind:?}\n{interpreter:#?}")]
    Execution {
        kind: ExecutionErrorKind,
        interpreter: Interpreter,
        current_statement: Statement,
    },
}

#[justerror::Error]
pub enum ExecutionErrorKind {
    IO(#[from] std::io::Error),
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
    FunctionRedeclaration(String),
}

impl Interpreter {
    pub fn new(stdout: Rc<RefCell<dyn std::io::Write>>) -> Self {
        let mut stack = Environment::new();

        stack
            .declare(
                "clock".to_string(),
                Declarable::Function(Callable::Builtin(
                    |_: &[Value]| {
                        Ok(Value::Number(
                            std::time::SystemTime::now()
                                .duration_since(std::time::UNIX_EPOCH)
                                .unwrap()
                                .as_secs_f64(),
                        ))
                    },
                    0,
                )),
            )
            .expect("Builtin functions should not fail to declare");

        Self {
            environment: stack,
            stdout,
        }
    }

    pub fn interpret(&mut self, program: &Program) -> Result<(), ExecutionError> {
        for stmt in program.0.iter() {
            match self.execute(stmt) {
                Ok(_) => {}
                Err(e) => {
                    return Err(ExecutionError::Execution {
                        kind: e,
                        interpreter: self.clone(),
                        current_statement: stmt.clone(),
                    })
                }
            }
        }

        Ok(())
    }

    fn execute(&mut self, stmt: &Statement) -> Result<Option<Value>, ExecutionErrorKind> {
        let result = match stmt {
            Statement::Expression(expression) => {
                self.evaluate(expression)?;
                None
            }
            Statement::Print(expression) => {
                let value = self.evaluate(expression)?;
                writeln!(self.stdout.borrow_mut(), "{}", value)?;
                None
            }
            Statement::VarDeclaration(identifier, expression) => {
                let value = self.evaluate(expression)?;
                self.environment
                    .declare(identifier.clone(), Declarable::Variable(value))?;
                None
            }
            Statement::Block(statements) => {
                self.environment.push(Rc::new(RefCell::new(Scope::new(
                    Some(self.environment.current().clone()),
                    false,
                ))));
                let mut res = None;
                for stmt in statements.iter() {
                    res = self.execute(stmt)?;
                    if res.is_some() {
                        break;
                    }
                }
                self.environment.pop();
                res
            }
            Statement::If(condition, then_branch, else_branch) => {
                let condition = self.evaluate(condition)?;
                if is_truthy(&condition) {
                    self.execute(then_branch)?
                } else if let Some(else_branch) = else_branch {
                    self.execute(else_branch)?
                } else {
                    None
                }
            }
            Statement::While(condition, body) => {
                let mut res = None;
                while is_truthy(&self.evaluate(condition)?) {
                    res = self.execute(body)?;
                    if res.is_some() {
                        break;
                    }
                }
                res
            }
            Statement::FunctionDeclaration(name, args, body) => {
                self.environment.declare(
                    name.clone(),
                    Declarable::Function(Callable::Function(
                        self.environment.current().clone(),
                        args.to_vec(),
                        (&**body).clone(),
                    )),
                )?;
                None
            }
            Statement::Return(expression) => {
                if !self.environment.is_in_function() {
                    return Err(ExecutionErrorKind::CannotReturnFromTopLevel);
                }
                let mut result = Some(Value::Nil);
                if let Some(expression) = expression {
                    result = Some(self.evaluate(expression)?);
                };
                result
            }
        };

        Ok(result)
    }

    fn evaluate(&mut self, expression: &Expression) -> Result<Value, ExecutionErrorKind> {
        let res = match expression {
            Expression::Identifier(identifier) => match self.environment.get(identifier) {
                Some(Declarable::Variable(v)) => Ok(v.clone()),
                Some(Declarable::Function(c)) => Ok(Value::Closure(c.clone())),
                _ => Err(ExecutionErrorKind::UndeclaredVariable(identifier.clone())),
            },
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
                        (a, b) => Err(ExecutionErrorKind::InvalidLess(a, b)),
                    },
                    InfixOperator::LessThanOrEqual => match (a, self.evaluate(&b)?) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a <= b)),
                        (a, b) => Err(ExecutionErrorKind::InvalidLessEqual(a, b)),
                    },
                    InfixOperator::GreaterThan => match (a, self.evaluate(&b)?) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a > b)),
                        (a, b) => Err(ExecutionErrorKind::InvalidGreater(a, b)),
                    },
                    InfixOperator::GreaterThanOrEqual => match (a, self.evaluate(&b)?) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a >= b)),
                        (a, b) => Err(ExecutionErrorKind::InvalidGreaterEqual(a, b)),
                    },
                    InfixOperator::Plus => match (a, self.evaluate(&b)?) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
                        (Value::String(a), Value::String(b)) => {
                            Ok(Value::String(format!("{}{}", a, b)))
                        }
                        (a, b) => Err(ExecutionErrorKind::InvalidAdd(a, b)),
                    },
                    InfixOperator::Minus => match (a, self.evaluate(&b)?) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
                        (a, b) => Err(ExecutionErrorKind::InvalidSub(a, b)),
                    },
                    InfixOperator::Multiply => match (a, self.evaluate(&b)?) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
                        (a, b) => Err(ExecutionErrorKind::InvalidMult(a, b)),
                    },
                    InfixOperator::Divide => match (a, self.evaluate(&b)?) {
                        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a / b)),
                        (a, b) => Err(ExecutionErrorKind::InvalidDiv(a, b)),
                    },
                }
            }
            Expression::Unary(op, x) => {
                let x = self.evaluate(&x)?;
                match op {
                    UnaryOperator::Negate => match x {
                        Value::Number(n) => Ok(Value::Number(-n)),
                        x => Err(ExecutionErrorKind::InvalidNegate(x, Value::Nil)),
                    },
                    UnaryOperator::Not => match x {
                        Value::Boolean(b) => Ok(Value::Boolean(!b)),
                        x => Err(ExecutionErrorKind::InvalidNot(x)),
                    },
                }
            }
            Expression::Assign(name, expr) => {
                let value = self.evaluate(&expr)?;
                self.environment
                    .assign_variable(name, &value)
                    .ok_or_else(|| ExecutionErrorKind::UndeclaredVariable(name.clone()))
            }
            Expression::Call(expr, args) => {
                let Expression::Identifier(name) = expr.as_ref() else {
                    return Err(ExecutionErrorKind::InvalidFunctionCall(
                        "not an identifier".to_string(),
                        0,
                        0,
                    ));
                };

                let callable = match self.environment.get(name) {
                    Some(Declarable::Function(callable)) => callable.clone(),
                    Some(Declarable::Variable(Value::Closure(callable))) => callable.clone(),
                    _ => return Err(ExecutionErrorKind::UndeclaredFunction(name.clone())),
                };

                if args.len() != callable.arity() {
                    return Err(ExecutionErrorKind::InvalidFunctionCall(
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

#[derive(Debug, Clone)]
pub enum Callable {
    Function(Rc<RefCell<Scope>>, Vec<String>, Statement),
    Builtin(fn(&[Value]) -> Result<Value, ExecutionErrorKind>, usize),
}

impl Callable {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: &[Expression],
    ) -> Result<Value, ExecutionErrorKind> {
        match self {
            Callable::Function(scope, arg_names, body) => {
                let evaluated_args = args
                    .iter()
                    .map(|arg| interpreter.evaluate(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                interpreter
                    .environment
                    .push(Rc::new(RefCell::new(Scope::new(Some(scope.clone()), true))));
                for (arg_name, evaluated_arg) in arg_names.iter().zip(evaluated_args.into_iter()) {
                    interpreter
                        .environment
                        .declare(arg_name.clone(), Declarable::Variable(evaluated_arg))?;
                }
                let result = interpreter.execute(body)?.unwrap_or(Value::Nil);
                interpreter.environment.pop();
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
            Callable::Function(_, args, _) => args.len(),
            Callable::Builtin(_, arity) => *arity,
        }
    }
}
