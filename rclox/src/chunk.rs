use crate::value::Value;

pub enum OpCode {
    Return,
    Constant,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl OpCode {
    pub fn fromu8(x: u8) -> Result<Self, Box<dyn std::error::Error>> {
        match x {
            0 => Ok(OpCode::Return),
            1 => Ok(OpCode::Constant),
            2 => Ok(OpCode::Negate),
            3 => Ok(OpCode::Add),
            4 => Ok(OpCode::Subtract),
            5 => Ok(OpCode::Multiply),
            6 => Ok(OpCode::Divide),
            _ => Err(format!("Attemted to convert invalid number '{}' to OpCode", x).into()),
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            OpCode::Constant => 1,
            _ => 0,
        }
    }
}

pub struct Chunk {
    pub code: Vec<u8>,
    pub lines: Vec<usize>,
    pub constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: Vec::new(),
            lines: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub fn write_byte(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, constant: Value) -> usize {
        self.constants.push(constant);
        self.constants.len() - 1
    }
}