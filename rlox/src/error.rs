use std::num::ParseFloatError;

use rustyline::error::ReadlineError;
use string_interner::symbol::SymbolUsize;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum LoxError {
    #[error("Runtime error: Type error: Expected '{expected}', found '{actual}'")]
    TypeError {
        expected: &'static str,
        actual: &'static str,
    },

    #[error("Runtime error: Attempted to divide by zero")]
    DivByZero,

    #[error("Lexer error: Encountered unexpected token '{0}'")]
    UnexpectedToken(char),

    #[error("Lexer error: Encountered unterminated string literal")]
    UnterminatedString,

    #[error("Runtime error: {0}")]
    RuntimeError(String),

    #[error("Syntax error [Line {line}]: {msg}")]
    SyntaxError { line: usize, msg: String },

    #[error("Readline error: {0}")]
    ReadlineError(#[from] ReadlineError),

    #[error("ParseFloat error: {0}")]
    ParseFloatError(#[from] ParseFloatError),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Failed to resolve string")]
    MissingString(SymbolUsize),

    #[error("Runtime error: Global variable  '{0}' not defined")]
    UndefinedVariable(String),
}
