use crate::value::Value;
use crate::Result;

#[derive(Debug, PartialEq, PartialOrd)]
pub enum OpCode {
    Return,
    Constant(usize),
    Nil,
    True,
    False,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Equal,
    Greater,
    Less,
    Print,
    Pop,
    DefineGlobal(usize),
    GetGlobal(usize),
    SetGlobal(usize),
    GetLocal(usize),
    SetLocal(usize),
    JumpIfFalse(usize),
    Jump(usize),
    Loop(usize),
}

//impl OpCode {
//pub fn fromu8(x: u8) -> Result<Self> {
//match x {
//0 => Ok(Self::Return),
//1 => Ok(Self::Constant),
//2 => Ok(Self::Nil),
//3 => Ok(Self::True),
//4 => Ok(Self::False),
//5 => Ok(Self::Negate),
//6 => Ok(Self::Add),
//7 => Ok(Self::Subtract),
//8 => Ok(Self::Multiply),
//9 => Ok(Self::Divide),
//10 => Ok(Self::Not),
//11 => Ok(Self::Equal),
//12 => Ok(Self::Greater),
//13 => Ok(Self::Less),
//14 => Ok(Self::Print),
//15 => Ok(Self::Pop),
//16 => Ok(Self::DefineGlobal),
//17 => Ok(Self::GetGlobal),
//18 => Ok(Self::SetGlobal),
//19 => Ok(Self::GetLocal),
//20 => Ok(Self::SetLocal),
//21 => Ok(Self::JumpIfFalse),
//22 => Ok(Self::Jump),
//23 => Ok(Self::Loop),
//_ => Err(format!("Attemted to convert invalid number '{}' to OpCode", x).into()),
//}
//}
//}
