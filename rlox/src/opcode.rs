use string_interner::symbol::SymbolUsize;

use crate::value::Value;

#[derive(Debug)]
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
}
