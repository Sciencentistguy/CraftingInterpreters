use std::io::Write;

use crate::chunk::Chunk;
use crate::opcode::OpCode;

pub fn disassemble_chunk(chunk: &Chunk, module_name: &str) {
    println!("== {} ==", module_name);
    println!("Index\tLine\tOpcode\t");
    println!("---");
    for offset in 0..chunk.code.len() {
        disassemble_instruction(chunk, offset, true);
    }
    println!("---");
}

pub fn disassemble_instruction(chunk: &Chunk, offset: usize, grouped_mode: bool) -> usize {
    print!("{:04}\t", offset);

    let instruction = &chunk.code[offset];

    if grouped_mode && offset > 0 && (chunk.lines[offset] == chunk.lines[offset - 1]) {
        let next_line = chunk.lines.get(offset + 1);
        if next_line.is_some() && chunk.lines[offset] == *next_line.unwrap() {
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
        Constant(constant) => {
            //let constant = chunk.code[offset + 1] as usize;
            println!("Constant\t{:04}\t'{}'", constant, chunk.constants[*constant]);
            offset + 1
        }
        Negate => {
            println!("Negate");
            offset + 1
        }
        Add => {
            println!("Add");
            offset + 1
        }
        Subtract => {
            println!("Subtract");
            offset + 1
        }
        Multiply => {
            println!("Multiply");
            offset + 1
        }
        Divide => {
            println!("Divide");
            offset + 1
        }
        Nil => {
            println!("Nil");
            offset + 1
        }
        True => {
            println!("True");
            offset + 1
        }
        False => {
            println!("False");
            offset + 1
        }
        Not => {
            println!("Not");
            offset + 1
        }
        Equal => {
            println!("Equal");
            offset + 1
        }
        Greater => {
            println!("Greater");
            offset + 1
        }
        Less => {
            println!("Less");
            offset + 1
        }
        Print => {
            println!("Print");
            offset + 1
        }
        Pop => {
            println!("Pop");
            offset + 1
        }
        DefineGlobal(constant) => {
            //let constant = chunk.code[offset + 1] as usize;
            println!(
                "DefineGlobal\t{:04}\t'{}'",
                constant, chunk.constants[*constant]
            );
            offset + 1
        }
        GetGlobal(constant) => {
            //let constant = chunk.code[offset + 1] as usize;
            println!(
                "GetGlobal\t{:04}\t'{}'",
                constant, chunk.constants[*constant]
            );
            offset + 1
        }
        SetGlobal(constant) => {
            //let constant = chunk.code[offset + 1] as usize;
            println!(
                "SetGlobal\t{:04}\t'{}'",
                constant, chunk.constants[*constant]
            );
            offset + 1
        }
        GetLocal(slot) => {
            //let slot = chunk.code[offset + 1];
            println!("GetLocal\t{:04}", slot);
            offset + 1
        }
        SetLocal(slot) => {
            //let slot = chunk.code[offset + 1];
            println!("SetLocal\t{:04}", slot);
            offset + 1
        }
        JumpIfFalse(jump) => {
            //let jump = ((chunk.code[offset + 1] as usize) << 8) | chunk.code[offset + 2] as usize;
            println!("JumpIfFalse\t{:04} -> {:04}", offset, offset + jump);
            offset + 1
        }
        Jump(jump) => {
            //let jump = ((chunk.code[offset + 1] as usize) << 8) | chunk.code[offset + 2] as usize;
            println!("Jump\t\t{:04} -> {:04}", offset, offset + jump);
            offset + 1
        }
        Loop(jump) => {
            //let jump = ((chunk.code[offset + 1] as usize) << 8) | chunk.code[offset + 2] as usize;
            println!("Loop\t\t{:04} -> {:04}", offset, offset.wrapping_sub(*jump ).wrapping_add(1));
            offset + 1
        }
    }
}
