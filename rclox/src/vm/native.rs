//! Native rust functions that are callable from Lox
//!
//! These are all of type `fn(&[Value]) -> Result<Value>`. This allows them to signal runtime
//! errors, unlike in clox. This includes being able to check for an unexpected number of
//! arguments, but this is not forced, and each function much check this itself.

use crate::Result;
use crate::value::Value;

/// Returns the amount of time since the unix epoch as an f64
pub(super) fn clock_native(_: &[Value]) -> Result<Value> {
    Ok(Value::Number(
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("The system is running before 1970...")
            .as_secs_f64(),
    ))
}
