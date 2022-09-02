use rustyline::error::ReadlineError;
use thiserror::Error;

use crate::value::ValueKind;

#[derive(Debug, Error)]
pub enum LoxError {
    #[error("Runtime error: Type error: Expected '{expected}', found '{actual}'")]
    TypeError {
        expected: ValueKind,
        actual: ValueKind,
    },

    #[error("Runtime error: Attempted to divide by zero")]
    DivByZero,

    #[error("Lexer error: Encountered unexpected token '{0}'")]
    UnexpectedToken(char),

    #[error("Lexer error: Encountered unterminated string literal")]
    UnterminatedString,

    #[error("Readline error: {0}")]
    ReadlineError(#[from] ReadlineError),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

}
