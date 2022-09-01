use chunk::Chunk;
use opcode::Opcode;
use value::Value;
use virtual_machine::VirtualMachine;

mod chunk;
mod debug;
mod error;
mod opcode;
mod value;
mod virtual_machine;

fn main() {
    let mut chunk = Chunk::default();
    chunk.add_instruction(Opcode::Constant(Value::Number(5.0)), 0);
    chunk.add_instruction(Opcode::Constant(Value::Number(5.0)), 0);
    chunk.add_instruction(Opcode::Add, 0);
    chunk.add_instruction(Opcode::Return, 0);

    println!("Disassembling...");
    chunk.disassemble("<script>");

    let mut vm = VirtualMachine::new();

    println!("Running...");
    if let Err(error) = vm.interpret(&chunk) {
        eprintln!("{error}")
    };
}
