pub mod chunk;
pub mod compiler;
pub mod debug;
pub mod error;
pub mod instruction;
pub mod lexer;
pub mod value;
pub mod vm;

type Result<T> = std::result::Result<T, error::RcloxError>;
