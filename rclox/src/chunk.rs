use std::ops::Index;
use std::ops::IndexMut;
use std::slice::SliceIndex;

use crate::opcode::OpCode;
use crate::value::Value;

pub struct Chunk {
    pub code: Vec<InstructionWithLine>,
    pub constants: Vec<Value>,
}

#[derive(Debug)]
pub struct InstructionWithLine {
    pub instruction: OpCode,
    pub line: usize,
}

impl<Idx: SliceIndex<[InstructionWithLine]>> IndexMut<Idx> for Chunk {
    fn index_mut(&mut self, index: Idx) -> &mut Self::Output {
        self.code.index_mut(index)
    }
}
impl<Idx: SliceIndex<[InstructionWithLine]>> Index<Idx> for Chunk {
    type Output = Idx::Output;

    fn index(&self, index: Idx) -> &Self::Output {
        &self.code.index(index)
    }
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),
        }
    }

    #[inline]
    pub fn write_instruction(&mut self, instruction: OpCode, line: usize) {
        self.code.push(InstructionWithLine { instruction, line })
    }

    pub fn add_constant(&mut self, constant: Value) -> usize {
        self.constants.push(constant);
        self.constants.len() - 1
    }

    pub fn get(&self, index: usize) -> Option<&InstructionWithLine> {
        self.code.get(index)
    }
}
