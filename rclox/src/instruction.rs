use std::rc::Rc;

use crate::value::Value;

#[derive(Debug, PartialEq, Clone)]
pub enum Instruction {
    /// Return from the current function
    Return,
    /// Refers to a constant stored in the constants table
    Constant(Value),
    /// Pushes `Nil` to the stack
    Nil,
    /// Pushes `true` to the stack.
    True,
    /// Pushes `False` to the stack.
    False,
    /// Negates the value at the top of the stack.
    Negate,
    /// Adds together the two values at the top of the stack, and pushes the
    /// result.
    Add,
    /// Subtracts the value at the top of the stack from the value second on the
    /// stack, and pushes the result.
    Subtract,
    /// Multiplies together the two values at the top of the stack, and pushes
    /// the result.
    Multiply,
    /// Divides the value at the top of the stack by the value second on the
    /// stack, and pushes the result.
    Divide,
    /// Negates the boolean value of the top of the stack.
    Not,
    /// Calculate equality of the two values at the top of the stack.
    Equal,
    /// Checks whether the value second on the stack is greater than the value
    /// at the top of the stack.
    Greater,
    /// Checks whether the value second on the stack is less than the value at
    /// the top of the stack.
    Less,
    /// Print the value at the top of the stack.
    Print,
    /// Throw away the value on the top of the stack, without doing anything
    /// with it.
    Pop,
    /// Define a global variable with value of the value at the top of the
    /// stack.
    DefineGlobal(Rc<String>),
    /// Fetch the value of a global variable and push it to the stack.
    GetGlobal(Rc<String>),
    /// Set the value of a global variable to the value at the top of the stack.
    SetGlobal(Rc<String>),
    /// Fetch the value of a local variable and push it to the stack.
    GetLocal(usize),
    /// Set the value of a local variable to the value at the top of the stack.
    SetLocal(usize),
    /// Jump forward `n` instructions if the value at the top of the stack is
    /// false.
    JumpIfFalse(usize),
    /// Jump forward `n` instructions unconditionally.
    Jump(usize),
    /// Jump backwards `n` instructions unconditionally
    Loop(usize),
}
