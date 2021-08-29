use std::ops::Index;
use std::ops::IndexMut;
use std::slice::SliceIndex;

use crate::instruction::Instruction;

#[derive(Debug)]
pub struct Chunk {
    pub code: Vec<InstructionWithLine>,
}

#[derive(Debug)]
#[allow(missing_copy_implementations)]
pub struct InstructionWithLine {
    pub instruction: Instruction,
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
        self.code.index(index)
    }
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk { code: Vec::new() }
    }

    #[inline]
    pub fn write_instruction(&mut self, instruction: Instruction, line: usize) {
        self.code.push(InstructionWithLine { instruction, line })
    }

    pub fn get(&self, index: usize) -> Option<&InstructionWithLine> {
        self.code.get(index)
    }
}

impl Default for Chunk {
    fn default() -> Self {
        Self::new()
    }
}
