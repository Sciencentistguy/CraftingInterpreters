use crate::value::Value;

pub enum OpCode {
    Constant,
    Return,
}

impl OpCode {
    pub fn fromu8(x: u8) -> Result<Self, Box<dyn std::error::Error>> {
        match x {
            0 => Ok(OpCode::Constant),
            1 => Ok(OpCode::Return),
            _ => Err(format!("Attemted to convert invalid number '{}' to OpCode", x).into()),
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
