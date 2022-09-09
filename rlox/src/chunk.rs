use crate::{debug::Disassembler, opcode::Opcode};

#[derive(Debug, Default)]
pub struct Chunk {
    code: Vec<Opcode>,
    lines: Vec<usize>,
}

impl Chunk {
    pub fn disassemble(&self, name: &str) {
        println!("== {name} ==");

        let mut disassembler = Disassembler::new();
        for (idx, (instruction, line)) in self.code.iter().zip(&self.lines).enumerate() {
            let next_line = self.lines.get(idx + 1).copied();
            disassembler.disassemble(instruction, idx, *line, next_line);
        }
    }

    pub fn add_instruction(&mut self, opcode: Opcode, line: usize) {
        self.code.push(opcode);
        self.lines.push(line);
    }

    pub fn code(&self) -> &[Opcode] {
        self.code.as_ref()
    }

    pub fn lines(&self) -> &[usize] {
        self.lines.as_ref()
    }

    pub fn code_mut(&mut self) -> &mut Vec<Opcode> {
        &mut self.code
    }
}
