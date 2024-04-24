use std::panic;

#[repr(u8)]
enum OpCode {
    Constant,
    Return,
    Sentinel,
}

impl From<OpCode> for u8 {
    fn from(value: OpCode) -> u8 {
        value as u8
    }
}

impl TryFrom<u8> for OpCode {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value < OpCode::Sentinel as u8 {
            Ok(unsafe { std::mem::transmute(value) })
        } else {
            Err(())
        }
    }
}

type Value = f64;

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

    pub fn write(&mut self, byte: impl Into<u8>, line: usize) {
        self.code.push(byte.into());
        self.lines.push(line);
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
            OpCode::Return => simple_instruction("OP_RETURN", offset),
            OpCode::Sentinel => panic!("Sentinel opcode should not be present in bytecode"),
        }
    }
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 1
}

pub fn test_chunk() {
    let mut chunk = Chunk::new();
    let constant = chunk.add_constant(1.2);
    chunk.write(OpCode::Constant, 123);
    chunk.write(constant, 123);
    chunk.write(OpCode::Return, 123);
    chunk.disassemble("test chunk");
}
