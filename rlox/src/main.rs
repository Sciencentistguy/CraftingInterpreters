use chunk::Chunk;
use opcode::Opcode;

mod chunk;
mod debug;
mod opcode;

fn main() {
    let mut chunk = Chunk::default();
    chunk.push(Opcode::Return);

    chunk.disassemble("<script>");
}
