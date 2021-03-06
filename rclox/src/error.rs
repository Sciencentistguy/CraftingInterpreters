
use thiserror::Error;

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
}
