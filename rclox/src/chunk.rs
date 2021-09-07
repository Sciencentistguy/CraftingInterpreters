use std::borrow::Borrow;
use std::borrow::BorrowMut;
use std::ops::Deref;
use std::ops::DerefMut;

use crate::instruction::Instruction;

/// A wrapper around a vector of instructions and their lines.
#[derive(Debug, Clone)]
pub struct Chunk {
    pub code: Vec<InstructionWithLine>,
}

/// A struct to combine an instruction and a line number
#[derive(Debug, Clone)]
#[allow(missing_copy_implementations)]
pub struct InstructionWithLine {
    pub instruction: Instruction,
    pub line: usize,
}

impl Chunk {
    /// Create a new, empty, chunk
    pub fn new() -> Chunk {
        Chunk { code: Vec::new() }
    }

    #[inline]
    /// Add an instruction to the chunk, with a specific line number
    pub fn write_instruction(&mut self, instruction: Instruction, line: usize) {
        self.code.push(InstructionWithLine { instruction, line })
    }
}

impl Deref for Chunk {
    type Target = [InstructionWithLine];

    fn deref(&self) -> &Self::Target {
        self.code.borrow()
    }
}

impl DerefMut for Chunk {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.code.borrow_mut()
    }
}

impl Default for Chunk {
    fn default() -> Self {
        Self::new()
    }
}
