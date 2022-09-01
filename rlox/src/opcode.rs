use crate::value::Value;

#[derive(Debug)]
pub enum Opcode {
    Return,
    Constant(Value),
}
