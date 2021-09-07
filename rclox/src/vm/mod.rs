mod native;

use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;

use crate::chunk::Chunk;
use crate::compiler::compile;
use crate::error::RcloxError;
use crate::instruction::Instruction;
use crate::value::LoxFunction;
use crate::value::NativeFunction;
use crate::value::Value;

use crate::lexer::Token;
use crate::lexer::TokenType;

use crate::Result;

#[derive(Debug)]
struct CallFrame {
    /// The code of the callframe
    code: CallFrameCode,
    /// The index in the chunk to the instruction being executed.
    ///
    /// In the C version this is a pointer to an instruction. When jumping, the
    /// instruction pointer is set to the value before the instruction to be
    /// executed next. If this value is the first instruction (index 0), the
    /// pointer is set to an invalid location but is incremented before
    /// it is ever dereferenced.
    ///
    /// With the program counter approach, care must be taken to allow this,
    /// using wrapping_sub and wrapping_add.
    program_counter: usize,
    /// The position on the stack where this callframe's storage starts
    slots_start: usize,
}

#[derive(Debug)]
pub enum CallFrameCode {
    Function(Rc<LoxFunction>),
    Script(Chunk),
}

impl CallFrameCode {
    pub fn chunk(&self) -> &Chunk {
        match self {
            Self::Script(ref x) => x,
            Self::Function(ref x) => &x.chunk,
        }
    }

    //fn chunk_mut(&mut self) -> &mut Chunk {
    //match self {
    //Self::Script(ref mut x) => x,
    //Self::Function(ref mut x) => &mut x.chunk,
    //}
}

impl CallFrame {
    /// Construct a new, blank, CallFrame
    fn new(function: CallFrameCode) -> Self {
        Self {
            code: function,
            program_counter: 0,
            slots_start: 0,
        }
    }
}

/// The VM state machine
#[derive(Debug)]
pub struct VM {
    /// The callstack of the VM, used for function calls
    call_stack: Vec<CallFrame>,
    /// The VM stack, where temporary values and local variables are stored
    stack: Vec<Value>,
    /// The table of global variables
    globals_table: HashMap<Rc<String>, Value>,
}

impl VM {
    /// Construct a new, blank, VM
    pub fn new() -> VM {
        VM {
            stack: Vec::with_capacity(256),
            globals_table: HashMap::with_capacity(16),
            call_stack: Vec::with_capacity(64),
        }
    }

    /// Run a Lox program, from a string
    pub fn interpret(&mut self, source: &str) -> Result<Vec<String>> {
        let targets = compile(source)?;
        assert_eq!(targets.len(), 1);

        self.call_stack
            .push(CallFrame::new(targets[0].output.to_callframe_code()));
        // Debug: disassemble_chunk the functions
        println!("Functions:");
        for func in self
            .current_frame()
            .code
            .chunk()
            .iter()
            .filter_map(|x| match &x.instruction {
                Instruction::Constant(Value::Function(f)) => {
                    Some(<Rc<LoxFunction> as Borrow<LoxFunction>>::borrow(f))
                }
                _ => None,
            })
        {
            crate::debug::disassemble_chunk(&func.chunk, func.name.as_str());
        }

        self.init_native_functions();

        // Run the VM
        self.run()
    }

    /// Register native functions as global variables
    fn init_native_functions(&mut self) {
        let clock = NativeFunction::new(native::clock, "clock");
        self.define_native(clock);
        let read_to_string = NativeFunction::new(native::read_to_string, "read_to_string");
        self.define_native(read_to_string);
        let check_if_file_exists =
            NativeFunction::new(native::check_if_file_exists, "check_if_file_exists");
        self.define_native(check_if_file_exists);
        let write_string_to_file =
            NativeFunction::new(native::write_string_to_file, "write_string_to_file");
        self.define_native(write_string_to_file);
    }

    /// Get the current frame
    fn current_frame(&self) -> &CallFrame {
        self.call_stack
            .last()
            .expect("Call stack should not be empty")
    }

    /// Get the current frame mutably
    fn current_frame_mut(&mut self) -> &mut CallFrame {
        self.call_stack
            .last_mut()
            .expect("Call stack should not be empty")
    }

    /// Produce a vector of tokens from a source str. Only used in testing
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
            let idx = self.stack.len() - index - 1;
            self.stack.get(idx)
        }
        .expect("Invalid peek")
    }

    /// Construct a RcloxError::Runtime with the correct line
    fn runtime_error(&self, error_message: &str) -> RcloxError {
        RcloxError::Runtime {
            message: error_message.to_string(),
            line: self.current_frame().code.chunk()[self.current_frame().program_counter].line,
        }
    }

    /// Run the VM
    ///
    /// Returns a vector of everything that the VM printed, for testing purposes
    fn run(&mut self) -> Result<Vec<String>> {
        println!("Executing program...");

        let mut printed = Vec::new();

        loop {
            println!("Stack:\t{:?}", self.stack);
            crate::debug::disassemble_instruction(
                self.current_frame().code.chunk(),
                self.current_frame().program_counter,
                false,
            );

            // This has to be wrapping because loop instructions can wrap to usize::MAX. See
            // the comment on the match arm for Instruction::Loop.
            let old_pc = self.current_frame().program_counter;
            self.call_stack.last_mut().unwrap().program_counter = old_pc.wrapping_add(1);

            match &self.call_stack.last().unwrap().code.chunk()
                [self.call_stack.last().unwrap().program_counter - 1]
                .instruction
            {
                Instruction::Return => {
                    let result = self.pop();
                    let frame = self.call_stack.pop().unwrap();

                    // If this was the outermost scope, exit the interpreter
                    if self.call_stack.is_empty() {
                        return Ok(printed);
                    }

                    // Pop locals
                    let arg_count = match frame.code {
                        CallFrameCode::Function(f) => f.arity,
                        CallFrameCode::Script(_) => unreachable!(),
                    };

                    //let arg_count = self.stack.len().saturating_sub(frame.slots_start);
                    for _ in 0..arg_count {
                        self.pop();
                    }

                    // Pop the function itself
                    self.pop();

                    self.push(result);
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
                            );
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
                            ));
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
                    self.push(Value::Bool(!v.coerce_bool()))
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
                                .runtime_error(format!("Undefined variable '{}'", name).as_str()));
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
                    let idx = self.current_frame().slots_start + slot;
                    let val = self.stack[idx].clone();
                    //let val = self.current_frame().slots[*slot].clone();
                    //let val = self.stack[*slot].clone();
                    self.push(val);
                }
                Instruction::SetLocal(slot) => {
                    //let slot = *slot;
                    let idx = self.current_frame().slots_start + slot;
                    let val = self.peek(0).clone();
                    self.stack[idx] = val;
                    //self.current_frame_mut().slots[slot] = val;
                }
                Instruction::JumpIfFalse(offset) => {
                    if !self.peek(0).coerce_bool() {
                        let offset = *offset;
                        self.current_frame_mut().program_counter += offset;
                    }
                }
                Instruction::Jump(offset) => {
                    let offset = *offset;
                    self.current_frame_mut().program_counter += offset;
                }
                Instruction::Loop(offset) => {
                    // In the C version this is a pointer to an instruction, and is just allowed to
                    // point to an invalid location in the interim before it is incremented before
                    // the next iteration of run();
                    //
                    // Using wrapping sub and wrapping add allows this behaviour to be replicated
                    // with indices rather than pointers
                    let offset = *offset;
                    let new_pc = self.current_frame().program_counter.wrapping_sub(offset);
                    self.current_frame_mut().program_counter = new_pc;
                }
                Instruction::Call(arg_count) => {
                    let arg_count = *arg_count;
                    // XXX: Anti-pattern: clone to shut up the borrow checker
                    if !self.call_value(self.peek(arg_count).clone(), arg_count)? {
                        return Err(self.runtime_error("Incorrect number of arguments"));
                    }
                }
            }
        }
    }

    /// Call a value, erroring if it is not callable
    fn call_value(&mut self, callee: Value, arg_count: usize) -> Result<bool> {
        match callee {
            Value::Function(f) => self.call(f, arg_count),
            Value::NativeFunction(f) => {
                let result = (f.func)(&self.stack[self.stack.len() - arg_count..])?;
                for _ in 0..arg_count + 1 {
                    self.pop();
                }
                self.push(result);
                Ok(true)
            }
            _ => Err(self.runtime_error("Can only call functions and classes")),
        }
    }

    /// Call a function
    fn call(&mut self, function: Rc<LoxFunction>, arg_count: usize) -> Result<bool> {
        if arg_count != function.arity {
            //TODO: two allocations here, should runtime_error take a String bc it always
            // allocates anyway
            return Err(self.runtime_error(&format!(
                "Expected {} arguments, but got {}",
                function.arity, arg_count
            )));
        }
        let output = CallFrameCode::Function(function);
        let mut frame = CallFrame::new(output);
        frame.slots_start = self.stack.len() - arg_count;
        self.call_stack.push(frame);
        Ok(true)
    }

    /// Helper function to add a native function. Used in
    /// `[init_native_functions]`
    fn define_native(&mut self, function: NativeFunction) {
        self.globals_table.insert(
            Rc::new(function.name.to_owned()),
            Value::NativeFunction(function),
        );
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}
