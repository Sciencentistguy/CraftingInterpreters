mod chunk;
mod debug;
mod value;

use chunk::Chunk;
use chunk::OpCode;
use value::Value;

fn main() {
    let mut chunk = Chunk::new();

    let constant = chunk.add_constant(Value::Number(1.5));
    chunk.write_byte(OpCode::Constant as u8, 0);
    chunk.write_byte(constant as u8, 0);

    chunk.write_byte(OpCode::Return as u8, 0);
    chunk.write_byte(OpCode::Return as u8, 0);

    debug::disassemble_chunk(&chunk, "main");
}
