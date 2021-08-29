pub mod chunk;
pub mod compiler;
pub mod debug;
pub mod lexer;
pub mod opcode;
pub mod value;
pub mod vm;
pub mod error;


type Result<T> = std::result::Result<T, error::RcloxError>;
