use crate::chunk::Chunk;
use crate::instruction::Instruction;

pub fn disassemble_chunk(chunk: &Chunk, module_name: &str) {
    println!("== {} ==", module_name);
    println!("Index\tLine\tInstruction");
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

    use Instruction::*;

    match instruction {
        Return => {
            println!("Return");
        }
        Constant(value) => {
            println!("Constant\t`{}`", value);
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
        DefineGlobal(name) => {
            println!("DefineGlobal\t`{}`", name);
        }
        GetGlobal(name) => {
            println!("GetGlobal\t`{}`", name);
        }
        SetGlobal(name) => {
            println!("SetGlobal\t`{}`", name);
        }
        GetLocal(slot) => {
            println!("GetLocal\t{:04}", slot);
        }
        SetLocal(slot) => {
            println!("SetLocal\t{:04}", slot);
        }
        JumpIfFalse(jump) => {
            println!("JumpIfFalse\t{:04} -> {:04}", offset, offset + jump);
        }
        Jump(jump) => {
            println!("Jump\t\t{:04} -> {:04}", offset, offset + jump);
        }
        Loop(jump) => {
            println!(
                "Loop\t\t{:04} -> {:04}",
                offset,
                offset.wrapping_sub(*jump).wrapping_add(1)
            );
        }
    }
}
