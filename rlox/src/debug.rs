use crate::{opcode::Opcode, INTERNER};

pub struct Disassembler {
    last_line: Option<usize>,
}

impl Disassembler {
    #[allow(clippy::new_without_default)]
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

            Opcode::Negate => {
                println!("Negate");
            }
            Opcode::Add => {
                println!("Add");
            }

            Opcode::Subtract => {
                println!("Subtract");
            }

            Opcode::Multiply => {
                println!("Multiply");
            }

            Opcode::Divide => {
                println!("Divide");
            }

            Opcode::Nil => {
                println!("Nil");
            }

            Opcode::True => {
                println!("True");
            }

            Opcode::False => {
                println!("False");
            }

            Opcode::Not => {
                println!("Not");
            }

            Opcode::Equal => {
                println!("Equal");
            }

            Opcode::Greater => {
                println!("Greater");
            }

            Opcode::Less => {
                println!("Less");
            }

            Opcode::Print => {
                println!("Print");
            }

            Opcode::Pop => {
                println!("Pop");
            }

            Opcode::DefineGlobal(value) => {
                let interner = INTERNER.lock();
                let value = interner.resolve(*value).unwrap();
                println!("DefineGlobal\t{value}");
            }

            Opcode::GetGlobal(value) => {
                let interner = INTERNER.lock();
                let value = interner.resolve(*value).unwrap();
                println!("GetGlobal\t{value}");
            }

            Opcode::SetGlobal(value) => {
                let interner = INTERNER.lock();
                let value = interner.resolve(*value).unwrap();
                println!("SetGlobal\t{value}");
            }

            Opcode::GetLocal(slot) => {
                println!("GetLocal\t{slot}");
            }

            Opcode::SetLocal(slot) => {
                println!("SetLocal\t{slot}");
            }

            Opcode::JumpIfFalse(distance) => {
                println!("JumpIfFalse\t{distance}");
            }

            Opcode::Jump(distance) => {
                println!("Jump\t{distance}");
            }

            Opcode::Loop(distance) => {
                println!("Loop\t{distance}");
            }

            Opcode::Call(arity) => {
                println!("Call\t{arity}");
            }

            Opcode::Closure(function, upvalues) => {
                println!("Closure\t{function}");
                for uv in upvalues {
                    println!(
                        "  |\t\t  {} {}",
                        if uv.is_local { "local" } else { "upvalue" },
                        uv.index
                    );
                }
            }
            Opcode::GetUpvalue(slot) => {
                println!("GetUpvalue\t{slot}");
            }
            Opcode::SetUpvalue(slot) => {
                println!("SetUpvalue\t{slot}");
            }
            Opcode::CloseUpvalue => {
                println!("CloseUpvalue");
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
