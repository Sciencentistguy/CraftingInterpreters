use crate::opcode::Opcode;

pub struct Disassembler {
    last_line: Option<usize>,
}

impl Disassembler {
    pub fn new() -> Self {
        Self { last_line: None }
    }

    pub fn disassemble(
        &mut self,
        opcode: &Opcode,
        idx: usize,
        current_line: usize,
        next_line: Option<usize>,
    ) {
        print!("{idx:04}\t");

        if self.is_next_line(current_line) {
            self.last_line = Some(current_line);
            print!("{current_line:04}\t");
        } else {
            match next_line {
                Some(next_line) if next_line == current_line => print!("├───\t"),
                _ => print!("└───\t"),
            }
        }

        match opcode {
            Opcode::Return => {
                println!("Return");
            }
            Opcode::Constant(value) => {
                println!("Constant\t{value}");
            }
        }
    }

    fn is_next_line(&self, line: usize) -> bool {
        match self.last_line {
            Some(last_line) => line > last_line,
            None => true,
        }
    }
}
