use crate::chunk::Chunk;
use crate::instruction::Instruction;

pub fn recurse_functions(chunk: &Chunk) {
    for function in chunk.iter().filter_map(|x| match &x.instruction {
        //Instruction::Closure(Value::Closure(c)) => Some(&c.func),
        Instruction::Closure {
            closure,
            upvalues: _,
        } => Some(&closure.func),
        _ => None,
    }) {
        disassemble_chunk(&function.chunk, function.name.as_str());
        recurse_functions(&function.chunk)
    }
}

/// Print the contents of a chunk
pub fn disassemble_chunk(chunk: &Chunk, module_name: &str) {
    println!("== {} ==", module_name);
    println!("Index\tLine\tInstruction");
    println!("---");
    for offset in 0..chunk.code.len() {
        disassemble_instruction(chunk, offset, true);
    }
    println!("---");
}

/// Print an instruction, including its arguments, if it has any
pub fn disassemble_instruction(chunk: &Chunk, offset: usize, grouped_mode: bool) {
    print!("{:04}\t", offset);

    let instruction = &chunk[offset].instruction;

    if grouped_mode && offset > 0 && (chunk[offset].line == chunk[offset - 1].line) {
        let next_line = chunk.get(offset + 1).map(|x| x.line);
        if next_line == Some(chunk[offset].line) {
            print!("├───\t");
        } else {
            print!("└───\t");
        }
    } else {
        print!("{:04}\t", chunk[offset].line);
    }

    match instruction {
        Instruction::Return => {
            println!("Return");
        }
        Instruction::Constant(value) => {
            println!("Constant\t`{}`", value);
        }
        Instruction::Negate => {
            println!("Negate");
        }
        Instruction::Add => {
            println!("Add");
        }
        Instruction::Subtract => {
            println!("Subtract");
        }
        Instruction::Multiply => {
            println!("Multiply");
        }
        Instruction::Divide => {
            println!("Divide");
        }
        Instruction::Not => {
            println!("Not");
        }
        Instruction::Equal => {
            println!("Equal");
        }
        Instruction::Greater => {
            println!("Greater");
        }
        Instruction::Less => {
            println!("Less");
        }
        Instruction::Print => {
            println!("Print");
        }
        Instruction::Pop => {
            println!("Pop");
        }
        Instruction::DefineGlobal(name) => {
            println!("DefineGlobal\t`{}`", name);
        }
        Instruction::GetGlobal(name) => {
            println!("GetGlobal\t`{}`", name);
        }
        Instruction::SetGlobal(name) => {
            println!("SetGlobal\t`{}`", name);
        }
        Instruction::GetLocal(slot) => {
            println!("GetLocal\t{:04}", slot);
        }
        Instruction::SetLocal(slot) => {
            println!("SetLocal\t{:04}", slot);
        }
        Instruction::JumpIfFalse(jump) => {
            // Add 1 here, because it technically jumps to the instruction *before* the next
            // one to be executed, but for debug purposes we want to display the
            // next instruction.
            println!("JumpIfFalse\t{:04} -> {:04}", offset, offset + jump + 1);
        }
        Instruction::Jump(jump) => {
            // Add 1 here, because it technically jumps to the instruction *before* the next
            // one to be executed, but for debug purposes we want to display the
            // next instruction.
            println!("Jump\t\t{:04} -> {:04}", offset, offset + jump + 1);
        }
        Instruction::Loop(jump) => {
            // Add 1 here, because it technically jumps to the instruction *before* the next
            // one to be executed, but for debug purposes we want to display the
            // next instruction.
            println!(
                "Loop\t\t{:04} -> {:04}",
                offset,
                offset.wrapping_sub(*jump).wrapping_add(1)
            );
        }
        Instruction::Call(arity) => {
            println!("Call\t\t{:04}", arity);
        }
        Instruction::Closure {
            closure,
            upvalues: _,
        } => {
            println!("Closure\t{}", closure.func.name);
            //TODO: print upvalues
        }
        Instruction::GetUpvalue(slot) => {
            println!("GetUpvalue\t{:04}", slot);
        }
        Instruction::SetUpvalue(slot) => {
            println!("SetUpvalue\t{:04}", slot);
        }
    }
}
