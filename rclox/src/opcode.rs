use crate::Result;

#[derive(Debug)]
pub enum OpCode {
    Return = 0,
    Constant = 1,
    Nil = 2,
    True = 3,
    False = 4,
    Negate = 5,
    Add = 6,
    Subtract = 7,
    Multiply = 8,
    Divide = 9,
    Not = 10,
    Equal = 11,
    Greater = 12,
    Less = 13,
}

impl OpCode {
    pub fn fromu8(x: u8) -> Result<Self> {
        match x {
            0 => Ok(Self::Return),
            1 => Ok(Self::Constant),
            2 => Ok(Self::Nil),
            3 => Ok(Self::True),
            4 => Ok(Self::False),
            5 => Ok(Self::Negate),
            6 => Ok(Self::Add),
            7 => Ok(Self::Subtract),
            8 => Ok(Self::Multiply),
            9 => Ok(Self::Divide),
            10 => Ok(Self::Not),
            11 => Ok(Self::Equal),
            12 => Ok(Self::Greater),
            13 => Ok(Self::Less),
            _ => Err(format!("Attemted to convert invalid number '{}' to OpCode", x).into()),
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            OpCode::Constant => 1,
            _ => 0,
        }
    }
}

