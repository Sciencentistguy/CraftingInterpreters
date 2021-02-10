use crate::chunk::Chunk;
use crate::chunk::OpCode;

pub fn disassemble_chunk(chunk: &Chunk, module_name: &str) {
    println!("== {} ==", module_name);
    println!("Index\tLine\tOpcode\t");
    println!("---");
    let mut offset = 0;
    while offset < chunk.code.len() {
        offset = disassemble_instruction(&chunk, offset);
    }
}

fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("{:04}\t", offset);

    if offset > 0 && (chunk.lines[offset] == chunk.lines[offset - 1]) {
        if chunk.lines[offset] == *chunk.lines.get(offset + 1).unwrap_or(&usize::MAX) {
            print!("├───\t");
        } else {
            print!("└───\t");
        }
    } else {
        print!("{:04}\t", chunk.lines[offset]);
    }

    let instruction = chunk.code[offset];
    use OpCode::*;
    match OpCode::fromu8(instruction) {
        Ok(x) => match x {
            Return => {
                println!("Return");
                offset + 1
            }
            Constant => {
                let constant = chunk.code[offset + 1] as usize;
                println!("Constant\t{:04}\t'{}'", constant, chunk.constants[constant]);
                offset + 2
            }
        },
        Err(_) => {
            println!("Unknown opcode {}", instruction);
            offset + 1
        }
    }
}
