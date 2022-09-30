use string_interner::symbol::SymbolUsize;

use crate::{compiler::CompilerUpvalue, value::Value};

#[derive(Debug, Clone)]
pub enum Opcode {
    Return,
    Constant(Value),
    Negate,

    Add,
    Subtract,
    Multiply,
    Divide,

    Nil,
    True,
    False,

    Not,

    Equal,
    Greater,
    Less,

    Print,
    Pop,

    DefineGlobal(SymbolUsize),
    GetGlobal(SymbolUsize),
    SetGlobal(SymbolUsize),

    GetLocal(usize),
    SetLocal(usize),
    JumpIfFalse(usize),
    Jump(usize),
    Loop(usize),
    Call(usize),

    Closure(Value, Vec<CompilerUpvalue>),
    GetUpvalue(usize),
    SetUpvalue(usize),
    CloseUpvalue,

    Class(SymbolUsize),
}
