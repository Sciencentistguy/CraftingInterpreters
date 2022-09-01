use chunk::Chunk;
use opcode::Opcode;
use value::Value;

mod chunk;
mod debug;
mod opcode;
mod value;

fn main() {
    let mut chunk = Chunk::default();
    chunk.add_instruction(Opcode::Constant(Value::Number(5.0)), 0);
    chunk.add_instruction(Opcode::Constant(Value::Number(6.0)), 0);
    chunk.add_instruction(Opcode::Return, 0);

    chunk.disassemble("<script>");
}
