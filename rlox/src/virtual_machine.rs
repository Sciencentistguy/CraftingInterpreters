use crate::{chunk::Chunk, debug::Disassembler, error::LoxError, opcode::Opcode, value::Value};

#[derive(Debug)]
pub struct VirtualMachine {
    program_counter: usize,
    stack: Vec<Value>,
}

const DEBUG_TRACE_EXECUTION: bool = true;

impl VirtualMachine {
    pub fn new() -> Self {
        Self {
            program_counter: 0,
            stack: Vec::new(),
        }
    }

    pub fn interpret(&mut self, chunk: &Chunk) -> Result<(), LoxError> {
        while self.program_counter < chunk.code().len() {
            let opcode = &chunk.code()[self.program_counter];

            if DEBUG_TRACE_EXECUTION {
                let mut debugger = Disassembler::new();
                debugger.disassemble(
                    opcode,
                    self.program_counter,
                    chunk.lines()[self.program_counter],
                    None,
                );
            }

            match opcode {
                Opcode::Return => {
                    println!("{}", self.stack.pop().expect("stack should not be empty"));
                    return Ok(());
                }
                Opcode::Constant(constant) => {
                    self.stack.push(constant.clone());
                }
                Opcode::Negate => {
                    let old = self.stack.pop().expect("stack should not be empty");
                    self.stack.push(old.negate()?);
                }
                Opcode::Add => {
                    let b = self.stack.pop().expect("stack should not be empty");
                    let a = self.stack.pop().expect("stack should not be empty");

                    self.stack.push(a.add(&b)?);
                }
                Opcode::Subtract => {
                    let b = self.stack.pop().expect("stack should not be empty");
                    let a = self.stack.pop().expect("stack should not be empty");

                    self.stack.push(a.sub(&b)?);
                }
                Opcode::Multiply => {
                    let b = self.stack.pop().expect("stack should not be empty");
                    let a = self.stack.pop().expect("stack should not be empty");

                    self.stack.push(a.mul(&b)?);
                }
                Opcode::Divide => {
                    let b = self.stack.pop().expect("stack should not be empty");
                    let a = self.stack.pop().expect("stack should not be empty");

                    self.stack.push(a.div(&b)?);
                }
            }

            self.program_counter += 1;
        }

        unreachable!("VM should terminate itself before running out of in")
    }
}
