use std::collections::HashMap;
use std::rc::Rc;

use crate::chunk::Chunk;
use crate::compiler::compile;
use crate::error::RcloxError;
use crate::instruction::Instruction;
use crate::value::Value;

#[cfg(test)]
use crate::lexer::Token;
#[cfg(test)]
use crate::lexer::TokenType;

use crate::Result;

/// The VM state machine
#[derive(Debug)]
pub struct VM {
    chunk: Chunk,
    program_counter: usize,
    stack: Vec<Value>,
    globals_table: HashMap<Rc<String>, Value>,
}

impl VM {
    /// Construct a new, blank, VM
    pub fn new() -> VM {
        VM {
            chunk: Chunk::new(),
            program_counter: 0,
            stack: Vec::with_capacity(256),
            globals_table: HashMap::new(),
        }
    }

    /// Run a Lox program, from a string
    pub fn interpret(&mut self, source: &str) -> Result<Vec<String>> {
        let chunk = compile(source)?;
        self.chunk = chunk;
        self.program_counter = 0;
        self.run()
    }

    /// Produce a vector of tokens from a source str. Only used in testing
    #[cfg(test)]
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
    /// Push `value` to the stack
    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    #[inline]
    /// Pop from the stack and return the value
    fn pop(&mut self) -> Value {
        self.stack
            .pop()
            .expect("Attempted to pop from an empty stack")
    }

    #[inline]
    /// Peek the stack, returning a mutable reference. Panics if the stack is
    /// empty or if the peek index is too high
    fn peek_mut(&mut self, index: usize) -> &mut Value {
        if index == 0 {
            self.stack.last_mut()
        } else {
            let idx = self.stack.len() - index;
            self.stack.get_mut(idx)
        }
        .expect("Invalid peek")
    }

    #[inline]
    /// Peek the stack, returning a reference. Panics if the stack is empty or
    /// if the peek index is too high
    fn peek(&self, index: usize) -> &Value {
        if index == 0 {
            self.stack.last()
        } else {
            let idx = self.stack.len() - index;
            self.stack.get(idx)
        }
        .expect("Invalid peek")
    }

    /// Construct a RcloxError::Runtime with the correct line
    fn runtime_error(&self, error_message: &str) -> RcloxError {
        RcloxError::Runtime {
            message: error_message.to_string(),
            line: self.chunk[self.program_counter].line,
        }
    }

    /// Run the VM
    fn run(&mut self) -> Result<Vec<String>> {
        println!("Executing program...");
        let mut printed = Vec::new();
        loop {
            println!("Stack:\t{:?}", self.stack);
            crate::debug::disassemble_instruction(&self.chunk, self.program_counter, false);

            let instruction = &self.chunk[self.program_counter].instruction;
            // This has to be wrapping because loop instructions can wrap to usize::MAX. See
            // the comment on the match arm for Instruction::Loop.
            self.program_counter = self.program_counter.wrapping_add(1);

            match instruction {
                Instruction::Return => {
                    // Exit interpreter
                    return Ok(printed);
                }
                Instruction::Constant(constant) => {
                    let val = constant.clone();
                    self.push(val);
                }
                Instruction::Negate => {
                    match self.peek_mut(0) {
                        Value::Number(x) => *x = -*x,
                        _ => {
                            return Err(
                                self.runtime_error("Operand to unary negation must be a number.")
                            )
                        }
                    };
                }
                Instruction::Add => {
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
                Instruction::Subtract => {
                    let b = self.pop();
                    let a = self.pop();
                    if !(a.is_number() && b.is_number()) {
                        return Err(self.runtime_error("Operands to - must be two numbers"));
                    }
                    self.push(a - b);
                }
                Instruction::Multiply => {
                    let b = self.pop();
                    let a = self.pop();
                    if !(a.is_number() && b.is_number()) {
                        return Err(self.runtime_error("Operands to * must be two numbers"));
                    }
                    self.push(a * b);
                }
                Instruction::Divide => {
                    let b = self.pop();
                    let a = self.pop();
                    if !(a.is_number() && b.is_number()) {
                        return Err(self.runtime_error("Operands to / must be two numbers"));
                    }
                    self.push(a / b);
                }
                Instruction::Nil => self.push(Value::Nil),
                Instruction::True => self.push(Value::Bool(true)),
                Instruction::False => self.push(Value::Bool(false)),
                Instruction::Not => {
                    let v = self.pop();
                    self.push(Value::Bool(!v.coersce_bool()))
                }
                Instruction::Equal => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::Bool(a == b));
                }
                Instruction::Greater => {
                    let b = self.pop();
                    let a = self.pop();
                    if !(a.is_number() && b.is_number()) {
                        return Err(self.runtime_error("Operands to > must be two numbers"));
                    }
                    self.push(Value::Bool(a > b))
                }
                Instruction::Less => {
                    let b = self.pop();
                    let a = self.pop();
                    if !(a.is_number() && b.is_number()) {
                        return Err(self.runtime_error("Operands to < must be two numbers"));
                    }
                    self.push(Value::Bool(a < b))
                }
                Instruction::Print => {
                    let a = self.pop();
                    let string = format!("{}", a);
                    println!("{}", string);
                    printed.push(string);
                }
                Instruction::Pop => {
                    self.pop();
                }
                Instruction::DefineGlobal(name) => {
                    let name = name.clone();
                    let a = self.pop();
                    self.globals_table.insert(name, a);
                }
                Instruction::GetGlobal(name) => {
                    let name = name.clone();

                    // XXX: shut up the borrow checker. It has to clone on the hot path, but this
                    // clones on the cold path when it shouldn't have to. This *really* doesn't
                    // matter, but its annoying
                    let value = self.globals_table.get(&name).cloned();
                    match value {
                        Some(x) => self.push(x),
                        None => {
                            return Err(self
                                .runtime_error(format!("Undefined variable '{}'", name).as_str()))
                        }
                    }
                }
                Instruction::SetGlobal(name) => {
                    let name = name.clone();

                    // XXX: shut up the borrow checker. It has to clone on the hot path, but this
                    // clones on the cold path when it shouldn't have to. This *really* doesn't
                    // matter, but its annoying
                    let value = self.peek(0).clone();
                    if let Some(ptr) = self.globals_table.get_mut(&name) {
                        *ptr = value;
                    } else {
                        return Err(
                            self.runtime_error(format!("Undefined variable {}", name).as_str())
                        );
                    }
                }
                Instruction::GetLocal(slot) => {
                    let val = self.stack[*slot].clone();
                    self.push(val);
                }
                Instruction::SetLocal(slot) => {
                    let val = self.peek(0).clone();
                    self.stack[*slot] = val;
                }
                Instruction::JumpIfFalse(offset) => {
                    if !self.peek(0).coersce_bool() {
                        self.program_counter += offset;
                    }
                }
                Instruction::Jump(offset) => {
                    self.program_counter += offset;
                }
                Instruction::Loop(offset) => {
                    // In the C version this is a pointer to an instruction, and is just allowed to
                    // point to an invalid location in the interim before it is incremented before
                    // the next iteration of run();
                    //
                    // Using wrapping sub and wrapping add allows this behaviour to be replicated
                    // with indices rather than pointers
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
