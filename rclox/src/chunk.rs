use std::convert::TryInto;
use std::num::TryFromIntError;

use crate::value::Value;
use crate::Result;

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

    #[inline]
    pub fn write_byte(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, constant: Value) -> Result<u8> {
        self.constants.push(constant);
        (self.constants.len() - 1)
            .try_into()
            .map_err(|x: TryFromIntError| x.into())
    }
}
