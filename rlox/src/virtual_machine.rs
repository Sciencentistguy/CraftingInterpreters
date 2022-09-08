use std::collections::{hash_map::Entry, HashMap};

use string_interner::symbol::SymbolUsize;

use crate::{
    chunk::Chunk, compiler::Parser, debug::Disassembler, error::LoxError, opcode::Opcode,
    value::Value, INTERNER,
};

struct Uninit;
struct Init;

#[derive(Debug)]
pub struct VirtualMachine {
    program_counter: usize,
    stack: Vec<Value>,
    globals: HashMap<SymbolUsize, Value>,
    current_chunk: Chunk,
}

const DEBUG_TRACE_EXECUTION: bool = true;

impl VirtualMachine {
    pub fn new() -> Self {
        Self {
            program_counter: 0,
            stack: Vec::new(),
            globals: HashMap::new(),
            current_chunk: Chunk::default(),
        }
    }

    pub fn reset(&mut self, source: &str, line: usize) -> Result<(), LoxError> {
        let mut parser = Parser::with_line(source, line);

        parser.compile()?;

        self.program_counter = 0;
        self.stack.clear();
        self.current_chunk = parser.finalise();

        Ok(())
    }

    pub fn start(&mut self) -> Result<(), LoxError> {
        while self.program_counter < self.current_chunk.code().len() {
            let opcode = &self.current_chunk.code()[self.program_counter];

            if DEBUG_TRACE_EXECUTION {
                let mut debugger = Disassembler::new();

                print!("Executing: ");
                debugger.disassemble(
                    opcode,
                    self.program_counter,
                    self.current_chunk.lines()[self.program_counter],
                    None,
                );
            }

            match opcode {
                Opcode::Return => {
                    // Exit interpreter
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
                Opcode::Nil => {
                    self.stack.push(Value::Nil);
                }
                Opcode::True => {
                    self.stack.push(Value::Boolean(true));
                }
                Opcode::False => {
                    self.stack.push(Value::Boolean(false));
                }
                Opcode::Not => {
                    let a = self.stack.pop().expect("stack should not be empty");
                    self.stack.push(a.not());
                }
                Opcode::Equal => {
                    let b = self.stack.pop().expect("stack should not be empty");
                    let a = self.stack.pop().expect("stack should not be empty");

                    self.stack.push(Value::Boolean(a.eq(&b)));
                }
                Opcode::Greater => {
                    let b = self.stack.pop().expect("stack should not be empty");
                    let a = self.stack.pop().expect("stack should not be empty");

                    self.stack.push(Value::Boolean(a.gt(&b)));
                }
                Opcode::Less => {
                    let b = self.stack.pop().expect("stack should not be empty");
                    let a = self.stack.pop().expect("stack should not be empty");

                    self.stack.push(Value::Boolean(a.lt(&b)));
                }
                Opcode::Print => {
                    println!("{}", self.stack.pop().expect("stack should not be empty"));
                }
                Opcode::Pop => {
                    self.stack.pop().expect("stack should not be empty");
                }
                Opcode::DefineGlobal(name) => {
                    self.globals
                        .insert(*name, self.stack.pop().expect("stack should not be empty"));
                }
                Opcode::GetGlobal(name) => {
                    let val = self.globals.get(name).cloned().ok_or_else(|| {
                        let interner = INTERNER.lock();
                        LoxError::UndefinedVariable(interner.resolve(*name).unwrap().to_owned())
                    })?;
                    self.stack.push(val);
                }
                Opcode::SetGlobal(name) => match self.globals.get_mut(name) {
                    Some(x) => {
                        *x = self
                            .stack
                            .last()
                            .cloned()
                            .expect("stack should not be empty");
                    }
                    None => {
                        let interner = INTERNER.lock();
                        return Err(LoxError::UndefinedVariable(
                            interner.resolve(*name).unwrap().to_owned(),
                        ));
                    }
                },
            }

            self.program_counter += 1;
        }

        unreachable!("VM should terminate itself before running out of in")
    }
}
