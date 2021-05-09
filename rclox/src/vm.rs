use std::collections::HashMap;
use std::rc::Rc;

use crate::compiler::CompilerDriver;
use crate::lexer::Token;
use crate::lexer::TokenType;
use crate::opcode::OpCode;
use crate::value::Value;
use crate::{chunk::Chunk, error::RcloxError};

use eyre::Result;

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

    pub fn interpret(&mut self, source: &str) -> Result<Vec<String>> {
        let mut chunk = Chunk::new();
        let mut compiler_driver = CompilerDriver::new(source);
        compiler_driver.compile(&mut chunk)?;
        self.chunk = chunk;
        self.program_counter = 0;
        self.run()
    }

    /// Produce a vector of tokens from a source str.
    pub fn lex<'a>(&mut self, source: &'a str) -> Result<Vec<Token<'a>>> {
        let mut lexer = crate::lexer::Lexer::new(source);

        let mut out = Vec::new();
        loop {
            let tok = lexer.lex_token()?;
            out.push(tok);
            if out.last().unwrap().kind == TokenType::Eof {
                break;
            }
        }

        Ok(out)
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
        if index == 0 {
            return self.stack.last().expect("Attempted to peek an empty stack");
        }
        let idx = self.stack.len() - index;
        println!(
            "Peeking index {} of stack of size {}.",
            idx,
            self.stack.len()
        );
        &self.stack[idx]
    }

    fn runtime_error(&self, error_message: &str) -> eyre::Report {
        RcloxError::Runtime {
            message: error_message.to_string(),
            line: self.chunk[self.program_counter].line,
        }
        .into()
    }

    fn run(&mut self) -> Result<Vec<String>> {
        println!("Executing program...");
        let mut printed = Vec::new();
        loop {
            println!("Stack:\t{:?}", self.stack);
            crate::debug::disassemble_instruction(&self.chunk, self.program_counter, false);

            let instruction = &self.chunk[self.program_counter].instruction;
            // This has to be wrapping because loop instructions can wrap to usize::MAX. See the
            // comment on OpCode::Loop.
            self.program_counter = self.program_counter.wrapping_add(1);

            match instruction {
                OpCode::Return => {
                    // Exit interpreter
                    return Ok(printed);
                }
                OpCode::Constant(index) => {
                    let constant = {
                        match &self.chunk.constants[*index] {
                            Value::String(ref x) => Value::String(x.clone()),
                            x => x.clone(),
                            //Value::Number(x) => Value::Number(*x),
                            //Value::Bool(x) => Value::Bool(x),
                            //Value::Nil => Value::Nil,
                        }
                    };
                    self.push(constant);
                }
                OpCode::Negate => {
                    let val = match self.pop() {
                        Value::Number(x) => Value::Number(-x),
                        _ => {
                            return Err(
                                self.runtime_error("Operand to unary negation must be a number.")
                            )
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
                        return Err(self.runtime_error("Operands to - must be two numbers"));
                    }
                    self.push(a - b);
                }
                OpCode::Multiply => {
                    let b = self.pop();
                    let a = self.pop();
                    if !(a.is_number() && b.is_number()) {
                        return Err(self.runtime_error("Operands to * must be two numbers"));
                    }
                    self.push(a * b);
                }
                OpCode::Divide => {
                    let b = self.pop();
                    let a = self.pop();
                    if !(a.is_number() && b.is_number()) {
                        return Err(self.runtime_error("Operands to / must be two numbers"));
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
                        return Err(self.runtime_error("Operands to > must be two numbers"));
                    }
                    self.push(Value::Bool(a > b))
                }
                OpCode::Less => {
                    let b = self.pop();
                    let a = self.pop();
                    if !(a.is_number() && b.is_number()) {
                        return Err(self.runtime_error("Operands to < must be two numbers"));
                    }
                    self.push(Value::Bool(a < b))
                }
                OpCode::Print => {
                    let a = self.pop();
                    let string = format!("{}", a);
                    println!("{}", string);
                    printed.push(string);
                }
                OpCode::Pop => {
                    self.pop();
                }
                OpCode::DefineGlobal(name) => {
                    let name = &self.chunk.constants[*name];
                    let name = match name {
                        Value::String(ref x) => x.clone(),
                        _ => {
                            unreachable!(
                                "Attempted to define a  global with a name that is not a String."
                            )
                        }
                    };
                    let a = self.pop();
                    self.globals_table.insert(name, a);
                }
                OpCode::GetGlobal(name) => {
                    let name = &self.chunk.constants[*name];
                    let name = match name {
                        Value::String(ref x) => x.clone(),
                        _ => {
                            unreachable!(
                                "Attempted to define a  global with a name that is not a String."
                            )
                        }
                    };
                    let value = self.globals_table.get(&name).map(|v| match v {
                        Value::String(s) => Value::String(s.clone()),
                        _ => v.clone(),
                    });
                    match value {
                        Some(x) => self.push(x),
                        None => {
                            return Err(self
                                .runtime_error(format!("Undefined variable '{}'", name).as_str()))
                        }
                    }
                }
                OpCode::SetGlobal(name) => {
                    let name = &self.chunk.constants[*name];
                    let name = match name {
                        Value::String(ref x) => x.clone(),
                        _ => {
                            unreachable!(
                                "Attempted to define a  global with a name that is not a String."
                            )
                        }
                    };
                    if self.globals_table.get(&name).is_none() {
                        return Err(
                            self.runtime_error(format!("Undefined variable {}", name).as_str())
                        );
                    }
                    *self.globals_table.get_mut(&name).unwrap() = self.peek(0).clone();
                }
                OpCode::GetLocal(slot) => {
                    let val = self.stack[*slot].clone();
                    self.push(val);
                }
                OpCode::SetLocal(slot) => {
                    let val = self.peek(0).clone();
                    self.stack[*slot] = val;
                }
                OpCode::JumpIfFalse(offset) => {
                    if !self.peek(0).coersce_bool() {
                        self.program_counter += offset;
                    }
                }
                OpCode::Jump(offset) => {
                    self.program_counter += offset;
                }
                OpCode::Loop(offset) => {
                    // We have to allow wrapping here because to jump to index 0 the program
                    // counter has to be -1. As it is a usize, -1 is represented with usize::MAX.
                    // In the C version this is a pointer to an instruction, and is just allowed to
                    // point to an invalid location in the interim before it is incremented before
                    // the next iteration of run();
                    self.program_counter = self.program_counter.wrapping_sub(*offset);
                }
            }
        }
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}
