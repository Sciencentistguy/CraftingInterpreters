mod native;

use std::collections::HashMap;
use std::rc::Rc;

use crate::chunk::Chunk;
use crate::compiler::compile;
use crate::debug;
use crate::error::RcloxError;
use crate::instruction::Instruction;
use crate::value::LoxClosure;
use crate::value::NativeFunction;
use crate::value::RuntimeUpvalue;
use crate::value::Value;

use crate::Result;

/// Get the current frame
macro_rules! current_frame {
    ($self:ident) => {
        $self
            .call_stack
            .last()
            .expect("Call stack should not be empty")
    };
}

/// Get the current frame mutably
macro_rules! current_frame_mut {
    ($self:ident) => {
        $self
            .call_stack
            .last_mut()
            .expect("Call stack should not be empty")
    };
}

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
    Closure(Rc<LoxClosure>),
    Script(Chunk),
}

impl CallFrameCode {
    pub fn chunk(&self) -> &Chunk {
        match self {
            Self::Script(ref x) => x,
            Self::Closure(ref x) => &x.func.chunk,
        }
    }

    //fn chunk_mut(&mut self) -> &mut Chunk {
    //match self {
    //Self::Script(ref mut x) => x,
    //Self::Function(ref mut x) => &mut x.chunk,
    //}

    pub fn as_closure(&self) -> Option<&Rc<LoxClosure>> {
        if let Self::Closure(v) = self {
            Some(v)
        } else {
            None
        }
    }
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
    /// All printed output (for testing purposes)
    pub print_log: Vec<String>,
}

impl VM {
    /// Construct a new, blank, VM
    pub fn new() -> VM {
        VM {
            stack: Vec::with_capacity(256),
            globals_table: HashMap::with_capacity(16),
            call_stack: Vec::with_capacity(64),
            print_log: Vec::new(),
        }
    }

    /// Run a Lox program, from a string
    pub fn interpret(&mut self, source: &str) -> Result<()> {
        let targets = compile(source)?;
        assert_eq!(targets.len(), 1);

        self.call_stack
            .push(CallFrame::new(targets[0].output.to_callframe_code()));

        // Debug: disassemble_chunk the functions
        println!("Functions:");
        debug::recurse_functions(targets[0].chunk());

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
    fn runtime_error(&self, error_message: String) -> RcloxError {
        RcloxError::Runtime {
            message: error_message,
            line: current_frame!(self).code.chunk()[current_frame!(self).program_counter].line,
        }
    }

    /// Run the VM
    ///
    /// Returns a vector of everything that the VM printed, for testing purposes
    fn run(&mut self) -> Result<()> {
        macro_rules! current_chunk {
            () => {
                &current_frame!(self).code.chunk()
            };
        }
        macro_rules! current_pc {
            () => {
                &current_frame!(self).program_counter
            };
        }

        println!("Executing program...");

        loop {
            println!("Stack:\t{:?}", self.stack);
            crate::debug::disassemble_instruction(
                current_frame!(self).code.chunk(),
                current_frame!(self).program_counter,
                false,
            );

            let old_pc = current_frame!(self).program_counter;
            // This has to be wrapping because loop instructions can wrap to usize::MAX. See
            // the comment on the match arm for Instruction::Loop.
            current_frame_mut!(self).program_counter = old_pc.wrapping_add(1);

            match &current_chunk!()[current_pc!() - 1].instruction {
                Instruction::Return => {
                    let result = self.pop();
                    let frame = self.call_stack.pop().unwrap();

                    // If this was the outermost scope, exit the interpreter
                    if self.call_stack.is_empty() {
                        return Ok(());
                    }

                    // Pop locals
                    let arg_count = frame
                        .code
                        .as_closure()
                        .expect("Found script callframe that was not the top level")
                        .func
                        .arity;
                    // let arg_count = match frame.code {
                    // CallFrameCode::Closure(c) => c.func.arity,
                    // CallFrameCode::Script(_) => unreachable!(),
                    // };

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
                            return Err(self.runtime_error(
                                "Operand to unary negation must be a number.".to_string(),
                            ));
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
                                "Operands to + must be either both numbers or both strings"
                                    .to_string(),
                            ));
                        }
                    }
                }
                Instruction::Subtract => {
                    let b = self.pop();
                    let a = self.pop();
                    if !(a.is_number() && b.is_number()) {
                        return Err(
                            self.runtime_error("Operands to - must be two numbers".to_string())
                        );
                    }
                    self.push(a - b);
                }
                Instruction::Multiply => {
                    let b = self.pop();
                    let a = self.pop();
                    if !(a.is_number() && b.is_number()) {
                        return Err(
                            self.runtime_error("Operands to * must be two numbers".to_string())
                        );
                    }
                    self.push(a * b);
                }
                Instruction::Divide => {
                    let b = self.pop();
                    let a = self.pop();
                    if !(a.is_number() && b.is_number()) {
                        return Err(
                            self.runtime_error("Operands to / must be two numbers".to_string())
                        );
                    }
                    self.push(a / b);
                }
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
                        return Err(
                            self.runtime_error("Operands to > must be two numbers".to_string())
                        );
                    }
                    self.push(Value::Bool(a > b))
                }
                Instruction::Less => {
                    let b = self.pop();
                    let a = self.pop();
                    if !(a.is_number() && b.is_number()) {
                        return Err(
                            self.runtime_error("Operands to < must be two numbers".to_string())
                        );
                    }
                    self.push(Value::Bool(a < b))
                }
                Instruction::Print => {
                    let a = self.pop();
                    let string = format!("{}", a);
                    println!("{}", string);
                    self.print_log.push(string);
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
                            return Err(
                                self.runtime_error(format!("Undefined variable '{}'", name))
                            );
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
                        return Err(self.runtime_error(format!("Undefined variable {}", name)));
                    }
                }
                Instruction::GetLocal(slot) => {
                    let idx = current_frame!(self).slots_start + slot;
                    let val = self.stack[idx].clone();
                    //let val = current_frame!(self).slots[*slot].clone();
                    //let val = self.stack[*slot].clone();
                    self.push(val);
                }
                Instruction::SetLocal(slot) => {
                    //let slot = *slot;
                    let idx = current_frame!(self).slots_start + slot;
                    let val = self.peek(0).clone();
                    self.stack[idx] = val;
                    //current_frame_mut!(self).slots[slot] = val;
                }
                Instruction::JumpIfFalse(offset) => {
                    if !self.peek(0).coerce_bool() {
                        let offset = *offset;
                        current_frame_mut!(self).program_counter += offset;
                    }
                }
                Instruction::Jump(offset) => {
                    let offset = *offset;
                    current_frame_mut!(self).program_counter += offset;
                }
                Instruction::Loop(offset) => {
                    // In the C version this is a pointer to an instruction, and is just allowed to
                    // point to an invalid location in the interim before it is incremented before
                    // the next iteration of run();
                    //
                    // Using wrapping sub and wrapping add allows this behaviour to be replicated
                    // with indices rather than pointers
                    let offset = *offset;
                    let new_pc = current_frame!(self).program_counter.wrapping_sub(offset);
                    current_frame_mut!(self).program_counter = new_pc;
                }
                Instruction::Call(arg_count) => {
                    let arg_count = *arg_count;
                    self.call_value(self.peek(arg_count).clone(), arg_count)?;
                }
                Instruction::Closure { closure, upvalues } => {
                    let mut closure = closure.clone();
                    closure.upvalues = upvalues
                        .iter()
                        .map(|uv| {
                            assert!(uv.is_local);
                            RuntimeUpvalue::Stack(self.stack.len() - 1 - uv.index)
                        })
                        .collect();
                    self.push(Value::Closure(Rc::new(closure)));
                }
                Instruction::GetUpvalue(slot) => {
                    let value = match current_frame!(self).code {
                        CallFrameCode::Closure(ref x) => {
                            self.dereference_upvalue(&x.upvalues[*slot]).clone()
                        }
                        CallFrameCode::Script(_) => unreachable!(),
                    };
                    self.push(value);
                }
                Instruction::SetUpvalue(_) => todo!(),
            }
        }
    }

    fn dereference_upvalue<'a>(&'a self, upvalue: &'a RuntimeUpvalue) -> &'a Value {
        match upvalue {
            RuntimeUpvalue::Stack(slot) => &self.stack[self.stack.len() - 1 - slot],
            RuntimeUpvalue::Heap(boxed) => boxed,
        }
    }

    /// Call a value, erroring if it is not callable
    fn call_value(&mut self, callee: Value, arg_count: usize) -> Result<()> {
        match callee {
            Value::Closure(c) => self.call(c, arg_count),
            Value::NativeFunction(f) => {
                let result = (f.func)(&self.stack[self.stack.len() - arg_count..])?;
                for _ in 0..arg_count + 1 {
                    self.pop();
                }
                self.push(result);
                Ok(())
            }
            x => Err(self.runtime_error(format!("Cannot call {}", x.typename()))),
        }
    }

    /// Call a function
    fn call(&mut self, closure: Rc<LoxClosure>, arg_count: usize) -> Result<()> {
        let function = &closure.func;
        if arg_count != function.arity {
            //TODO: two allocations here, should runtime_error take a String bc it always
            // allocates anyway
            return Err(self.runtime_error(format!(
                "Expected {} arguments, but got {}",
                function.arity, arg_count
            )));
        }
        let output = CallFrameCode::Closure(closure);
        let mut frame = CallFrame::new(output);
        frame.slots_start = self.stack.len() - arg_count;
        self.call_stack.push(frame);
        Ok(())
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
