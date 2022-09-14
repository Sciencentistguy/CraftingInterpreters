use std::{collections::HashMap, ops::Not, sync::Arc};

use string_interner::symbol::SymbolUsize;

use crate::{
    chunk::Chunk,
    compiler::Parser,
    debug::Disassembler,
    error::LoxError,
    opcode::Opcode,
    value::{Callable, LoxFunction, NativeFn, Value},
    INTERNER,
};

struct Uninit;
struct Init;

#[derive(Debug)]
pub struct VirtualMachine {
    stack: Vec<Value>,
    globals: HashMap<SymbolUsize, Value>,
    call_stack: Vec<CallFrame>,

    #[cfg(test)]
    pub print_log: Vec<String>,
}

#[derive(Debug)]
struct CallFrame {
    function: Arc<LoxFunction>,
    program_counter: usize,
    slots_start: usize,
}

const DEBUG_TRACE_EXECUTION: bool = true;

impl VirtualMachine {
    pub fn new() -> Self {
        let mut globals = HashMap::new();

        let clock = |_args: &[Value]| -> Result<Value, LoxError> {
            let now = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_millis() as f64;
            Ok(Value::Number(now))
        };

        let clock = NativeFn::new(clock, 0, "clock");
        let clock = Value::NativeFn(Arc::new(clock));

        globals.insert(INTERNER.lock().get_or_intern_static("clock"), clock);

        Self {
            // program_counter: 0,
            stack: Vec::new(),
            globals,
            // current_chunk(): Chunk::default(),
            // Dynamic stacks means no™️ overflows!
            call_stack: Vec::with_capacity(64),

            #[cfg(test)]
            print_log: Vec::new(),
        }
    }

    pub fn reset(&mut self, source: &str, line: usize) -> Result<(), LoxError> {
        let mut parser = Parser::with_line(source, line);

        parser.compile()?;

        let frame = CallFrame {
            function: Arc::new(parser.finalise()),
            program_counter: 0,
            slots_start: 0,
        };

        self.stack.clear();
        self.call_stack.push(frame);

        Ok(())
    }

    pub fn start(&mut self) -> Result<(), LoxError> {
        // The compiler reserves stack slot 0 for (i think?) `super` or `this`. That's not
        // supported yet, so put a `Value::Nil` there.
        self.stack.push(Value::Nil);

        while self.program_counter() < self.current_chunk().code().len() {
            // FIXME: Unncessary(?) clone
            let opcode = self.current_chunk().code()[self.program_counter()].clone();

            if DEBUG_TRACE_EXECUTION {
                let mut debugger = Disassembler::new();

                println!(
                    "Stack: [{}]",
                    self.stack
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                );
                print!("Executing: ");
                debugger.disassemble(
                    &opcode,
                    self.program_counter(),
                    self.current_chunk().lines()[self.program_counter()],
                    None,
                );
            }

            match opcode {
                Opcode::Return => {
                    let result = self.stack_pop();

                    let old_frame = self.call_stack.pop().unwrap();

                    if self.call_stack.is_empty() {
                        return Ok(());
                    }

                    self.stack.truncate(old_frame.slots_start);

                    self.stack.push(result);
                }
                Opcode::Constant(constant) => {
                    self.stack.push(constant.clone());
                }
                Opcode::Negate => {
                    let old = self.stack_pop();
                    self.stack.push(old.negate()?);
                }
                Opcode::Add => {
                    let b = self.stack_pop();
                    let a = self.stack_pop();

                    self.stack.push(a.add(&b)?);
                }
                Opcode::Subtract => {
                    let b = self.stack_pop();
                    let a = self.stack_pop();

                    self.stack.push(a.sub(&b)?);
                }
                Opcode::Multiply => {
                    let b = self.stack_pop();
                    let a = self.stack_pop();

                    self.stack.push(a.mul(&b)?);
                }
                Opcode::Divide => {
                    let b = self.stack_pop();
                    let a = self.stack_pop();

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
                    let a = self.stack_pop();
                    self.stack.push(a.not());
                }
                Opcode::Equal => {
                    let b = self.stack_pop();
                    let a = self.stack_pop();

                    self.stack.push(Value::Boolean(a.eq(&b)));
                }
                Opcode::Greater => {
                    let b = self.stack_pop();
                    let a = self.stack_pop();

                    self.stack.push(Value::Boolean(a.gt(&b)));
                }
                Opcode::Less => {
                    let b = self.stack_pop();
                    let a = self.stack_pop();

                    self.stack.push(Value::Boolean(a.lt(&b)));
                }
                Opcode::Print => {
                    #[cfg(test)]
                    {
                        let s = format!("{}", self.stack_pop());
                        self.print_log.push(s.clone());
                        println!("{s}");
                    }
                    #[cfg(not(test))]
                    {
                        println!("{}", self.stack_pop());
                    }
                }
                Opcode::Pop => {
                    self.stack_pop();
                }
                Opcode::DefineGlobal(name) => {
                    let value = self.stack_pop();
                    self.globals.insert(name, value);
                }
                Opcode::GetGlobal(name) => {
                    let val = self.globals.get(&name).cloned().ok_or_else(|| {
                        let interner = INTERNER.lock();
                        LoxError::UndefinedVariable(interner.resolve(name).unwrap().to_owned())
                    })?;
                    self.stack.push(val);
                }
                Opcode::SetGlobal(name) => {
                    // False positive.
                    // See: https://github.com/rust-lang/rust-clippy/issues/9470
                    #[allow(clippy::map_entry)]
                    if self.globals.contains_key(&name) {
                        let value = self.stack_top().clone();
                        self.globals.insert(name, value);
                    } else {
                        let interner = INTERNER.lock();
                        return Err(LoxError::UndefinedVariable(
                            interner.resolve(name).unwrap().to_owned(),
                        ));
                    }
                }
                Opcode::GetLocal(slot) => {
                    let slot = self.current_frame().slots_start + slot;
                    self.stack.push(self.stack[slot].clone());
                    // if DEBUG_TRACE_EXECUTION {
                    // println!(" ({})", self.stack_top());
                    // }
                }
                Opcode::SetLocal(slot) => {
                    let slot = self.current_frame().slots_start + slot;
                    self.stack[slot] = self.stack_top().clone();
                }
                Opcode::JumpIfFalse(distance) => {
                    if self.stack_top().to_bool().not() {
                        *self.program_counter_mut() += distance;
                    }
                }
                Opcode::Jump(distance) => {
                    *self.program_counter_mut() += distance;
                }
                Opcode::Loop(distance) => {
                    // The PC can go to "-1" (actually usize::MAX but who's counting) if we loop to
                    // the first instruction. This is fine, we just need to make sure we don't
                    // explode if that does happen, hence wrapping_sub
                    *self.program_counter_mut() = self.program_counter().wrapping_sub(distance);
                    /*
                     * if self.program_counter() > self.current_chunk().code().len() {
                     *     *self.program_counter_mut() = usize::MAX;
                     *     unreachable!("Jumped too far")
                     * }
                     */
                }
                Opcode::Call(arity) => {
                    let callee_slot = self.stack.len() - arity - 1;
                    let callee = self
                        .stack
                        .get(callee_slot)
                        .expect("Internal error: Not enough arguments on stack");

                    let callee = callee
                        .as_callable()
                        .ok_or_else(|| {
                            LoxError::RuntimeError(format!(
                                "Can only call functions and classes. Found {:?} instead.",
                                callee.kind()
                            ))
                        })?
                        .clone();

                    if arity != callee.arity() {
                        return Err(LoxError::RuntimeError(format!(
                            "Expected {} arguments but got {}.",
                            callee.arity(),
                            arity
                        )));
                    }

                    self.call(callee, arity)?;
                    continue;
                }
            }

            // The PC can go to "-1" (actually usize::MAX but who's counting) if we loop to the
            // first instruction. This is fine, we just need to make sure we don't explode if that
            // does happen, hence wrapping_add
            *self.program_counter_mut() = self.program_counter().wrapping_add(1);
        }

        unreachable!("Internal error: VM should terminate itself before running out of input")
    }

    fn program_counter(&self) -> usize {
        self.call_stack.last().unwrap().program_counter
    }

    fn program_counter_mut(&mut self) -> &mut usize {
        &mut self.call_stack.last_mut().unwrap().program_counter
    }

    fn current_chunk(&self) -> &Chunk {
        &self.call_stack.last().unwrap().function.chunk
    }

    fn current_frame(&self) -> &CallFrame {
        self.call_stack
            .last()
            .expect("Internal error: Empty callstack")
    }

    fn stack_top(&self) -> &Value {
        self.stack.last().expect("Internal error: Empty stack")
    }

    fn stack_pop(&mut self) -> Value {
        self.stack.pop().expect("Internal error: Empty stack")
    }

    fn call(&mut self, callable: Callable, arity: usize) -> Result<(), LoxError> {
        if let Some(nativefn) = callable.as_nativefn() {
            let args = &self.stack[self.stack.len() - 1 - arity..];

            let result = (nativefn.function)(args)?;

            // Pop args + callee
            self.stack.truncate(self.stack.len() - arity - 2);

            self.stack.push(result);

            // VM doesn't increment the PC after a call, so we need to do it here
            *self.program_counter_mut() = self.program_counter().wrapping_add(1);

            return Ok(());
        }

        let frame = CallFrame {
            function: callable.into_function(),
            program_counter: 0,
            slots_start: self.stack.len() - 1 - arity,
        };

        self.call_stack.push(frame);
        Ok(())
    }
}
