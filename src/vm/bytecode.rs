use std::panic;

use super::Value;

#[repr(u8)]
pub(super) enum OpCode {
    Constant,
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Return,
    Sentinel,
}

impl From<OpCode> for u8 {
    fn from(value: OpCode) -> u8 {
        value as u8
    }
}

#[derive(Debug, thiserror::Error)]
#[error(
    "Invalid byte {0} found when expecting OpCode value between 0 and {}",
    OpCode::Sentinel as u8
)]
pub struct OpCodeFromU8Error(u8);

impl TryFrom<u8> for OpCode {
    type Error = OpCodeFromU8Error;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value < OpCode::Sentinel as u8 {
            Ok(unsafe { std::mem::transmute(value) })
        } else {
            Err(OpCodeFromU8Error(value))
        }
    }
}

pub struct Chunk {
    code: Vec<u8>,
    lines: Vec<usize>,
    constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            lines: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub fn get_bytecode(&self, offset: usize) -> u8 {
        *self
            .code
            .get(offset)
            .expect("VM should only request valid offsets")
    }

    pub fn add_bytecode(&mut self, byte: impl Into<u8>, line: usize) {
        self.code.push(byte.into());
        self.lines.push(line);
    }

    pub fn get_cosntant(&self, index: u8) -> Value {
        *self
            .constants
            .get(index as usize)
            .expect("VM should only request valid constants")
    }

    pub fn add_constant(&mut self, value: Value) -> u8 {
        assert!(
            self.constants.len() <= u8::MAX as usize,
            "Too many constants in one chunk"
        );
        self.constants.push(value);
        (self.constants.len() - 1) as u8
    }

    pub fn disassemble(&self, name: &str) {
        println!("== {} ==", name);

        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassemble_instruction(offset);
        }
    }

    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        print!("{:04} ", offset);

        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            print!("   | ");
        } else {
            print!("{:4} ", self.lines[offset]);
        }

        let instruction = self.code[offset];
        match OpCode::try_from(instruction).expect("Bytecode should contain valid opcodes") {
            OpCode::Constant => {
                let constant = self.code[offset + 1];
                println!(
                    "{:<16} {:4} '{}'",
                    "OP_CONSTANT", constant, self.constants[constant as usize]
                );
                offset + 2
            }
            OpCode::Negate => simple_instruction("OP_NEGATE", offset),
            OpCode::Add => simple_instruction("OP_ADD", offset),
            OpCode::Subtract => simple_instruction("OP_SUBTRACT", offset),
            OpCode::Multiply => simple_instruction("OP_MULTIPLY", offset),
            OpCode::Divide => simple_instruction("OP_DIVIDE", offset),
            OpCode::Return => simple_instruction("OP_RETURN", offset),
            OpCode::Sentinel => panic!("Sentinel opcode should not be present in bytecode"),
        }
    }
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 1
}
