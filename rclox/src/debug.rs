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

pub fn disassemble_instruction(chunk: &Chunk, offset: usize, grouped_mode: bool) {
    print!("{:04}\t", offset);

    let instruction = &chunk[offset].instruction;

    if grouped_mode && offset > 0 && (chunk[offset].line == chunk[offset - 1].line) {
        let next_line = chunk.get(offset + 1).map(|x| x.line);
        if next_line.is_some() && Some(chunk[offset].line) == next_line {
            print!("├───\t");
        } else {
            print!("└───\t");
        }
    } else {
        print!("{:04}\t", chunk[offset].line);
    }

    use OpCode::*;

    match instruction {
        Return => {
            println!("Return");
        }
        Constant(constant) => {
            //let constant = chunk[offset + 1] as usize;
            println!(
                "Constant\t{:04}\t'{}'",
                constant, chunk.constants[*constant]
            );
        }
        Negate => {
            println!("Negate");
        }
        Add => {
            println!("Add");
        }
        Subtract => {
            println!("Subtract");
        }
        Multiply => {
            println!("Multiply");
        }
        Divide => {
            println!("Divide");
        }
        Nil => {
            println!("Nil");
        }
        True => {
            println!("True");
        }
        False => {
            println!("False");
        }
        Not => {
            println!("Not");
        }
        Equal => {
            println!("Equal");
        }
        Greater => {
            println!("Greater");
        }
        Less => {
            println!("Less");
        }
        Print => {
            println!("Print");
        }
        Pop => {
            println!("Pop");
        }
        DefineGlobal(constant) => {
            //let constant = chunk[offset + 1] as usize;
            println!(
                "DefineGlobal\t{:04}\t'{}'",
                constant, chunk.constants[*constant]
            );
        }
        GetGlobal(constant) => {
            //let constant = chunk[offset + 1] as usize;
            println!(
                "GetGlobal\t{:04}\t'{}'",
                constant, chunk.constants[*constant]
            );
        }
        SetGlobal(constant) => {
            //let constant = chunk[offset + 1] as usize;
            println!(
                "SetGlobal\t{:04}\t'{}'",
                constant, chunk.constants[*constant]
            );
        }
        GetLocal(slot) => {
            //let slot = chunk[offset + 1];
            println!("GetLocal\t{:04}", slot);
        }
        SetLocal(slot) => {
            //let slot = chunk[offset + 1];
            println!("SetLocal\t{:04}", slot);
        }
        JumpIfFalse(jump) => {
            //let jump = ((chunk[offset + 1] as usize) << 8) | chunk[offset + 2] as usize;
            println!("JumpIfFalse\t{:04} -> {:04}", offset, offset + jump);
        }
        Jump(jump) => {
            //let jump = ((chunk[offset + 1] as usize) << 8) | chunk[offset + 2] as usize;
            println!("Jump\t\t{:04} -> {:04}", offset, offset + jump);
        }
        Loop(jump) => {
            //let jump = ((chunk[offset + 1] as usize) << 8) | chunk[offset + 2] as usize;
            println!(
                "Loop\t\t{:04} -> {:04}",
                offset,
                offset.wrapping_sub(*jump).wrapping_add(1)
            );
        }
    }
}
