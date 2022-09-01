use crate::opcode::Opcode;

pub fn disassemble(instruction: &Opcode, idx: usize) {
    print!("{idx:04}\t");
    match instruction {
        Opcode::Return => {
            println!("{instruction:?}")
        }
    }
}
