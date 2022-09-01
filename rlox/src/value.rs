use std::{fmt::Display, ops::Neg};

use crate::error::LoxError;

#[derive(Debug)]
pub enum ValueKind {
    Number,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Number(f64),
}

impl Value {
    pub fn negate(&self) -> Result<Value, LoxError> {
        match self {
            Value::Number(n) => Ok(Value::Number(n.neg())),
        }
    }

    pub(crate) fn add(&self, other: &Value) -> Result<Value, LoxError> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
        }
    }

    pub(crate) fn sub(&self, other: &Value) -> Result<Value, LoxError> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
        }
    }

    pub(crate) fn mul(&self, other: &Value) -> Result<Value, LoxError> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
        }
    }

    pub(crate) fn div(&self, other: &Value) -> Result<Value, LoxError> {
        match (self, other) {
            (_, Value::Number(b)) if *b == 0.0 => Err(LoxError::DivByZero),
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a / b)),
        }
    }

    fn kind(&self) -> ValueKind {
        match self {
            Value::Number(_) => ValueKind::Number,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
        }
    }
}

impl Display for ValueKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}
