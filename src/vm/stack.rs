use std::fmt::Display;

use super::Value;

const MAX_STACK_SIZE: usize = 256;

pub struct Stack {
    storage: Vec<Value>,
}

impl Stack {
    pub fn new() -> Self {
        Stack {
            storage: Vec::with_capacity(MAX_STACK_SIZE),
        }
    }

    pub fn push(&mut self, value: Value) {
        if self.storage.len() >= MAX_STACK_SIZE {
            panic!("Stack overflow");
        }
        self.storage.push(value);
    }

    pub fn pop(&mut self) -> Value {
        self.storage
            .pop()
            .expect("VM shouldn't underflow the Stack")
    }

    pub fn top(&self) -> Value {
        *self
            .storage
            .last()
            .expect("VM shouldn't underflow the stack")
    }
}

impl Display for Stack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "          ")?;
        for value in self.storage.iter() {
            write!(f, "[ {:>8.2} ]", value)?;
        }
        Ok(())
    }
}
