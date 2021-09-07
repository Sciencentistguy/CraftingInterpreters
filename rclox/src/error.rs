use thiserror::Error;

/// The error type for Rclox
#[derive(Debug, Error)]
pub enum RcloxError {
    #[error("<Lexer> error: {0}")]
    Lexer(String),
    #[error("<Compiler> [Line {line}] Error at '{string}': {message}")]
    Compiler {
        string: String,
        line: usize,
        message: String,
    },
    #[error("<Runtime> [Line {line}] Error: {message}")]
    Runtime { message: String, line: usize },
    #[error("<Runtime> (in native function) Error: {message}")]
    Native { message: String },
    #[error("<Runtime> (IO) Error: {0}")]
    Io(#[from] std::io::Error),
}
