use std::collections::HashMap;
use std::rc::Rc;
use std::ops::IndexMut;

use crate::chunk::Chunk;
use crate::compiler::CompilerDriver;
use crate::opcode::OpCode;
use crate::value::Value;
use crate::Result;

pub struct VM {
    chunk: Chunk,
    program_counter: usize,
    stack: Vec<Value>,
    globals_table: HashMap<Rc<String>, Value>,
}

impl VM {
    pub fn new() -> VM {
        VM {
            chunk: Chunk::new(),
            program_counter: 0,
            stack: Vec::with_capacity(256),
            globals_table: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, source: &str) -> Result<()> {
        let mut chunk = Chunk::new();
        let mut compiler_driver = CompilerDriver::new(source);
        compiler_driver.compile(&mut chunk)?;
        self.chunk = chunk;
        self.program_counter = 0;
        self.run()?;
        Ok(())
    }

    #[inline]
    fn read_byte(&mut self) -> u8 {
        self.program_counter += 1;
        self.chunk.code[self.program_counter - 1]
    }

    #[inline]
    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    #[inline]
    fn pop(&mut self) -> Value {
        self.stack
            .pop()
            .expect("Attempted to pop from an empty stack")
    }

    #[inline]
    fn peek(&self, index: usize) -> &Value {
        &self.stack[self.stack.len() - index]
    }

    fn runtime_error(&self, message: &str) -> Box<dyn std::error::Error> {
        format!(
            "<Runtime> [Line {}] Error: {}",
            self.chunk.lines[self.program_counter], message
        )
        .into()
    }

    fn run(&mut self) -> Result<()> {
        loop {
            println!("Stack:\t{:?}", self.stack);
            crate::debug::disassemble_instruction(&self.chunk, self.program_counter, false);

            let instruction = self.read_byte();
            match OpCode::fromu8(instruction) {
                Ok(x) => match x {
                    OpCode::Return => {
                        // Exit interpreter
                        return Ok(());
                    }
                    OpCode::Constant => {
                        let constant = {
                            let index = self.read_byte() as usize;
                            match self.chunk.constants[index] {
                                Value::String(ref x) => Value::String(x.clone()),
                                Value::Number(x) => Value::Number(x),
                                Value::Bool(x) => Value::Bool(x),
                                Value::Nil => Value::Nil,
                            }
                        };
                        self.push(constant);
                    }
                    OpCode::Negate => {
                        let val = match self.pop() {
                            Value::Number(x) => Value::Number(-x),
                            _ => {
                                return Err(self
                                    .runtime_error("Operand to unary negation must be a number."))
                            }
                        };
                        self.push(val);
                    }
                    OpCode::Add => {
                        let b = self.pop();
                        let a = self.pop();
                        match (&a, &b) {
                            (Value::Number(_), Value::Number(_))
                            | (Value::String(_), Value::String(_)) => self.push(a + b),
                            _ => {
                                return Err(self.runtime_error(
                                    "Operands to + must be either both numbers or both strings",
                                ))
                            }
                        }
                    }
                    OpCode::Subtract => {
                        let b = self.pop();
                        let a = self.pop();
                        if !(a.is_number() && b.is_number()) {
                            return Err(self.runtime_error("Operands to - must be number"));
                        }
                        self.push(a - b);
                    }
                    OpCode::Multiply => {
                        let b = self.pop();
                        let a = self.pop();
                        if !(a.is_number() && b.is_number()) {
                            return Err(self.runtime_error("Operands to * must be number"));
                        }
                        self.push(a * b);
                    }
                    OpCode::Divide => {
                        let b = self.pop();
                        let a = self.pop();
                        if !(a.is_number() && b.is_number()) {
                            return Err(self.runtime_error("Operands to / must be number"));
                        }
                        self.push(a / b);
                    }
                    OpCode::Nil => self.push(Value::Nil),
                    OpCode::True => self.push(Value::Bool(true)),
                    OpCode::False => self.push(Value::Bool(false)),
                    OpCode::Not => {
                        let v = self.pop();
                        self.push(Value::Bool(!v.coersce_bool()))
                    }
                    OpCode::Equal => {
                        let b = self.pop();
                        let a = self.pop();
                        self.push(Value::Bool(a == b));
                    }
                    OpCode::Greater => {
                        let b = self.pop();
                        let a = self.pop();
                        if !(a.is_number() && b.is_number()) {
                            return Err(
                                self.runtime_error("Operands to comparisons must be number")
                            );
                        }
                        self.push(Value::Bool(a > b))
                    }
                    OpCode::Less => {
                        let b = self.pop();
                        let a = self.pop();
                        if !(a.is_number() && b.is_number()) {
                            return Err(
                                self.runtime_error("Operands to comparisons must be number")
                            );
                        }
                        self.push(Value::Bool(a < b))
                    }
                    OpCode::Print => {
                        let a = self.pop();
                        println!("{}", a);
                    }
                    OpCode::Pop => {
                        self.pop();
                    }
                    OpCode::DefineGlobal => {
                        let name = {
                            let index = self.read_byte() as usize;
                            match self.chunk.constants[index] {
                                Value::String(ref x) => x.clone(),
                                _ => unreachable!(
                                    "Attempted to define a  global with a name that is not a String."
                                ),
                            }
                        };
                        let a = self.pop();
                        self.globals_table.insert(name, a);
                    }
                    OpCode::GetGlobal => {
                        let name = {
                            let index = self.read_byte() as usize;
                            match self.chunk.constants[index] {
                                Value::String(ref x) => x.clone(),
                                _ => unreachable!(
                                    "Attempted to get a global with a name that is not a String."
                                ),
                            }
                        };
                        let value = self.globals_table.get(&name).map(|v| match v {
                            Value::String(s) => Value::String(s.clone()),
                            _ => v.clone(),
                        });
                        match value {
                            Some(x) => self.push(x),
                            None => {
                                return Err(self.runtime_error(
                                    format!("Undefined variable '{}'", name).as_str(),
                                ))
                            }
                        }
                    }
                    OpCode::SetGlobal => {
                        let name = {
                            let index = self.read_byte() as usize;
                            match self.chunk.constants[index] {
                                Value::String(ref x) => x.clone(),
                                _ => unreachable!(
                                    "Attempted to set a global with a name that is not a String."
                                ),
                            }
                        };
                        if self.globals_table.get(&name).is_none() {
                            return Err(
                                self.runtime_error(format!("Undefined variable {}", name).as_str())
                            );
                        }
                        *self.globals_table.get_mut(&name).unwrap() = self.peek(0).clone();
                    }
                    OpCode::GetLocal => {
                        let slot = self.read_byte();
                        let val = self.stack[slot as usize].clone();
                        self.push(val);
                    }
                    OpCode::SetLocal => {
                        let slot = self.read_byte();
                        self.stack[slot as usize] = self.peek(0).clone();
                    }
                },
                Err(_) => {
                    return Err(
                        format!("Attempted to run invalid instruction '{}'", instruction).into(),
                    )
                }
            }
        }
    }
}
