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
}
