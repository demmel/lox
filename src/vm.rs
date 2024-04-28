mod stack;

use crate::bytecode::{self, Chunk, OpCode, OpCodeFromU8Error};

use self::stack::Stack;

pub type Value = f64;

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
                OpCode::Add => binary_op(&mut self.stack, |a, b| a + b),
                OpCode::Subtract => binary_op(&mut self.stack, |a, b| a - b),
                OpCode::Multiply => binary_op(&mut self.stack, |a, b| a * b),
                OpCode::Divide => binary_op(&mut self.stack, |a, b| a / b),
                OpCode::Negate => {
                    let value = self.stack.pop();
                    self.stack.push(-value);
                }
                OpCode::Return => {
                    let value = self.stack.pop();
                    println!("{}", value);
                    return Ok(());
                }
                OpCode::Sentinel => unreachable!("Sentinel should be constructed in VM bytecode"),
            }
        }
    }
}

fn binary_op(stack: &mut Stack, op: impl Fn(f64, f64) -> f64) {
    let b = stack.pop();
    let a = stack.pop();
    stack.push(op(a, b));
}
