//! Native rust functions that are callable from Lox
//!
//! These are all of type `fn(&[Value]) -> Result<Value>`. This allows them to signal runtime
//! errors, unlike in clox. This includes being able to check for an unexpected number of
//! arguments, but this is not forced, and each function much check this itself.

use std::path::Path;
use std::rc::Rc;

use crate::error::RcloxError;
use crate::value::Value;
use crate::Result;

fn native_error(message: String) -> RcloxError {
    RcloxError::Native { message }
}

/// Returns the amount of time since the unix epoch as an f64
pub(super) fn clock(_: &[Value]) -> Result<Value> {
    Ok(Value::Number(
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("The system is running before 1970...")
            .as_secs_f64(),
    ))
}

/// Reads a file to a string.
///
/// Explodes if the file does not exist
pub(super) fn read_to_string(args: &[Value]) -> Result<Value> {
    let path = args.get(0).ok_or_else(|| {
        native_error("Call to read_to_string expected 1 argument, got 0".to_owned())
    })?;

    let path: &str = match path {
        Value::String(s) => s,
        _ => {
            return Err(native_error(
                "Argument to `read_to_string` must be a string".to_owned(),
            ))
        }
    };

    let file_contents = std::fs::read_to_string(path)?;

    Ok(Value::String(Rc::new(file_contents)))
}

/// Check if a file exists
pub(super) fn check_if_file_exists(args: &[Value]) -> Result<Value> {
    let path = args.get(0).ok_or_else(|| {
        native_error("Call to `check_if_file_exists` expected 1 argument, got 0".to_owned())
    })?;

    let path = match path {
        Value::String(s) => Path::new(&**s),
        _ => {
            return Err(native_error(
                "Argument to `check_if_file_exists` must be a string".to_owned(),
            ))
        }
    };

    Ok(Value::Bool(path.exists()))
}

/// Write a string to a file, creating the file if it does not exist, and overwriting it if it does
///
/// Argument 0 is the filepath, and argument 1 is the string to write to it.
pub(super) fn write_string_to_file(args: &[Value]) -> Result<Value> {
    let path = args.get(0).ok_or_else(|| {
        native_error("Call to `write_string_to_file` expected 2 arguments, got 0".to_owned())
    })?;
    let path = match path {
        Value::String(s) => Path::new(&**s),
        _ => {
            return Err(native_error(
                "First argument to `write_string_to_file` must be a string".to_owned(),
            ))
        }
    };

    let string = args.get(1).ok_or_else(|| {
        native_error("Call to `write_string_to_file` expected 2 arguments, got 1".to_owned())
    })?;
    let string: &str = match string {
        Value::String(s) => s,
        _ => {
            return Err(native_error(
                "Second argument to `write_string_to_file` must be a string".to_owned(),
            ))
        }
    };

    std::fs::write(path, string)?;
    Ok(Value::Nil)
}
