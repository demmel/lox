mod stack;

use crate::bytecode::{Chunk, OpCode, OpCodeFromU8Error};

use self::stack::Stack;

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Nil,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }
}

impl Eq for Value {}

impl Value {
    fn is_truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Boolean(b) => *b,
            _ => true,
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum InterpretError {
    #[error("Failed to compile")]
    Compile(String),
    #[error("Runtime failure occured")]
    Runtime(#[from] RuntimeError),
}

#[derive(Debug, thiserror::Error)]
pub enum RuntimeError {
    #[error("Failed to read instruction")]
    OpCodeFromU8(#[from] OpCodeFromU8Error),
    #[error("Cannot run VM without execution context.  Did you forget to pass a Chunk?")]
    NoExecution,
    #[error("Cannot negate non-number value")]
    NegateNonNumber,
    #[error("Invalid arithmetic values: {0:?} {1:?}")]
    InvalidArithmeticValues(Value, Value),
}

struct Execution<'a> {
    chunk: &'a Chunk,
    ip: usize,
}

impl Execution<'_> {
    fn read_byte(&mut self) -> u8 {
        let ret = self.chunk.get_bytecode(self.ip);
        self.ip += 1;
        ret
    }
}

pub struct Vm<'a> {
    execution: Option<Execution<'a>>,
    stack: Stack,
}

impl<'a> Vm<'a> {
    pub fn new() -> Self {
        Self {
            execution: None,
            stack: Stack::new(),
        }
    }

    pub fn interpret(&'a mut self, chunk: &'a Chunk) -> Result<(), InterpretError> {
        self.execution = Some(Execution { chunk, ip: 0 });
        self.run()?;
        Ok(())
    }

    fn run(&mut self) -> Result<(), RuntimeError> {
        let Some(execution) = &mut self.execution else {
            return Err(RuntimeError::NoExecution);
        };

        loop {
            #[cfg(feature = "trace")]
            {
                println!("{}", self.stack);
                execution.chunk.disassemble_instruction(execution.ip);
            }

            match OpCode::try_from(execution.read_byte())? {
                OpCode::Constant => {
                    let index = execution.read_byte();
                    let constant = execution.chunk.get_cosntant(index);
                    self.stack.push(constant);
                }
                OpCode::Nil => self.stack.push(Value::Nil),
                OpCode::True => self.stack.push(Value::Boolean(true)),
                OpCode::False => self.stack.push(Value::Boolean(false)),
                OpCode::Equal => binary_op(&mut self.stack, |a, b| Ok(Value::Boolean(a == b)))?,
                OpCode::Greater => binary_op(&mut self.stack, |a, b| {
                    if let (Value::Number(a), Value::Number(b)) = (a, b) {
                        Ok(Value::Boolean(a > b))
                    } else {
                        Err(RuntimeError::InvalidArithmeticValues(a, b))
                    }
                })?,
                OpCode::Less => binary_op(&mut self.stack, |a, b| {
                    if let (Value::Number(a), Value::Number(b)) = (a, b) {
                        Ok(Value::Boolean(a < b))
                    } else {
                        Err(RuntimeError::InvalidArithmeticValues(a, b))
                    }
                })?,
                OpCode::Add => binary_op(&mut self.stack, |a, b| {
                    if let (Value::Number(a), Value::Number(b)) = (a, b) {
                        Ok(Value::Number(a + b))
                    } else {
                        Err(RuntimeError::InvalidArithmeticValues(a, b))
                    }
                })?,
                OpCode::Subtract => binary_op(&mut self.stack, |a, b| {
                    if let (Value::Number(a), Value::Number(b)) = (a, b) {
                        Ok(Value::Number(a - b))
                    } else {
                        Err(RuntimeError::InvalidArithmeticValues(a, b))
                    }
                })?,
                OpCode::Multiply => binary_op(&mut self.stack, |a, b| {
                    if let (Value::Number(a), Value::Number(b)) = (a, b) {
                        Ok(Value::Number(a * b))
                    } else {
                        Err(RuntimeError::InvalidArithmeticValues(a, b))
                    }
                })?,
                OpCode::Divide => binary_op(&mut self.stack, |a, b| {
                    if let (Value::Number(a), Value::Number(b)) = (a, b) {
                        Ok(Value::Number(a / b))
                    } else {
                        Err(RuntimeError::InvalidArithmeticValues(a, b))
                    }
                })?,
                OpCode::Not => {
                    let value = self.stack.pop();
                    self.stack.push(Value::Boolean(!value.is_truthy()));
                }
                OpCode::Negate => match self.stack.pop() {
                    Value::Number(n) => {
                        self.stack.push(Value::Number(-n));
                    }
                    _ => return Err(RuntimeError::NegateNonNumber),
                },
                OpCode::Return => {
                    let value = self.stack.pop();
                    println!("{:?}", value);
                    return Ok(());
                }
                OpCode::Sentinel => unreachable!("Sentinel should be constructed in VM bytecode"),
            }
        }
    }
}

fn binary_op(
    stack: &mut Stack,
    op: impl Fn(Value, Value) -> Result<Value, RuntimeError>,
) -> Result<(), RuntimeError> {
    let b = stack.pop();
    let a = stack.pop();
    stack.push(op(a, b)?);
    Ok(())
}
