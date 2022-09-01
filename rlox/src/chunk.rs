use std::ops::{Deref, DerefMut};

use crate::{debug, opcode::Opcode};

#[derive(Debug, Default)]
pub struct Chunk {
    code: Vec<Opcode>,
}

impl Chunk {
    pub fn disassemble(&self, name: &str) {
        println!("== {name} ==");

        for (idx, instruction) in self.code.iter().enumerate() {
            debug::disassemble(instruction, idx);
        }
    }
}

impl Deref for Chunk {
    type Target = Vec<Opcode>;

    fn deref(&self) -> &Self::Target {
        &self.code
    }
}

impl DerefMut for Chunk {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.code
    }
}
