mod chunk;
mod debug;
mod value;
mod vm;

use chunk::Chunk;
use chunk::OpCode;
use value::Value;
use vm::VM;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut chunk = Chunk::new();

    let constant = chunk.add_constant(Value::Number(1.0));
    chunk.write_byte(OpCode::Constant as u8, 0);
    chunk.write_byte(constant as u8, 0);

    let constant = chunk.add_constant(Value::Number(1.0));
    chunk.write_byte(OpCode::Constant as u8, 0);
    chunk.write_byte(constant as u8, 0);

    chunk.write_byte(OpCode::Add as u8, 0);

    chunk.write_byte(OpCode::Return as u8, 1);

    let mut vm = VM::new();

    debug::disassemble_chunk(&chunk, "main");

    vm.interpret(chunk)?;

    Ok(())
}
