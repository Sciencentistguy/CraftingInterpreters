#![allow(dead_code)]
#![allow(clippy::new_without_default)]

use once_cell::sync::Lazy;
use parking_lot::Mutex;
use string_interner::{backend::BufferBackend, symbol::SymbolUsize, StringInterner};

pub mod chunk;
pub mod compiler;
pub mod debug;
pub mod error;
pub mod lexer;
pub mod opcode;
pub mod value;
pub mod virtual_machine;

#[cfg(test)]
mod tests;

type Interner = StringInterner<BufferBackend<SymbolUsize>>;

static INTERNER: Lazy<Mutex<Interner>> = Lazy::new(|| Mutex::new(Interner::new()));
