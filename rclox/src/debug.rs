use crate::chunk::Chunk;
use crate::chunk::OpCode;

pub fn disassemble_chunk(chunk: &Chunk, module_name: &str) {
    println!("== {} ==", module_name);
    println!("Index\tLine\tOpcode\t");
    println!("---");
    let mut offset = 0;
    while offset < chunk.code.len() {
        offset = disassemble_instruction(&chunk, offset, true);
    }
    println!("---");
}

pub fn disassemble_instruction(chunk: &Chunk, offset: usize, grouped_mode: bool) -> usize {
    print!("{:04}\t", offset);

    let instruction = chunk.code[offset];
    let instruction = if let Ok(x) = OpCode::fromu8(instruction) {
        x
    } else {
        println!("Unknown opcode {}", instruction);
        return offset + 1;
    };

    if grouped_mode && offset > 0 && (chunk.lines[offset] == chunk.lines[offset - 1]) {
        if chunk.lines[offset]
            == *chunk
                .lines
                .get(offset + instruction.arity() + 1)
                .unwrap_or(&usize::MAX)
        {
            print!("├───\t");
        } else {
            print!("└───\t");
        }
    } else {
        print!("{:04}\t", chunk.lines[offset]);
    }

    use OpCode::*;

    match instruction {
        Return => {
            println!("Return");
            offset + 1
        }
        Constant => {
            let constant = chunk.code[offset + 1] as usize;
            println!("Constant\t{:04}\t'{}'", constant, chunk.constants[constant]);
            offset + 2
        }
        OpCode::Negate => {
            println!("Negate");
            offset + 1
        }
        OpCode::Add => {
            println!("Add");
            offset + 1
        }
        OpCode::Subtract => {
            println!("Subtract");
            offset + 1
        }
        OpCode::Multiply => {
            println!("Multiply");
            offset + 1
        }
        OpCode::Divide => {
            println!("Divide");
            offset + 1
        }
    }
}
