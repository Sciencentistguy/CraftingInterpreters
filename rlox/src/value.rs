use std::{fmt::Display, ops::{Neg, Not}, cmp};

use crate::error::LoxError;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Nil,
}

mod kind {
    pub(super) const NUMBER: &str = "number";
    pub(super) const BOOLEAN: &str = "boolean";
    pub(super) const NIL: &str = "nil";
}

impl Value {
    pub fn negate(&self) -> Result<Value, LoxError> {
        match self {
            Value::Number(n) => Ok(Value::Number(n.neg())),
            _ => Err(LoxError::RuntimeError(format!(
                "Type error: Cannot negate value of type {}",
                self.kind()
            ))),
        }
    }

    fn to_bool(&self) -> Value {
        match self {
            Value::Boolean(false) | Value::Nil => Value::Boolean(false),
            _ => Value::Boolean(true),
        }
    }

    pub fn not(&self) -> Value {
        match self.to_bool() {
            Value::Boolean(a) => Value::Boolean(a.not()),
            _ => unreachable!(),
        }
    }

    pub fn add(&self, other: &Value) -> Result<Value, LoxError> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
            _ => Err(LoxError::RuntimeError(format!(
                "Type error: Cannot add values of type {} and {}",
                self.kind(),
                other.kind()
            ))),
        }
    }

    pub fn sub(&self, other: &Value) -> Result<Value, LoxError> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
            _ => Err(LoxError::RuntimeError(format!(
                "Type error: Cannot subtract value of type {} from {}",
                other.kind(),
                self.kind()
            ))),
        }
    }

    pub fn mul(&self, other: &Value) -> Result<Value, LoxError> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
            _ => Err(LoxError::RuntimeError(format!(
                "Type error: Cannot multiply values of type {} and {}",
                self.kind(),
                other.kind()
            ))),
        }
    }

    pub fn div(&self, other: &Value) -> Result<Value, LoxError> {
        match (self, other) {
            (_, Value::Number(b)) if *b == 0.0 => Err(LoxError::DivByZero),
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a / b)),
            _ => Err(LoxError::RuntimeError(format!(
                "Type error: Cannot divide value of type {} by {}",
                self.kind(),
                other.kind()
            ))),
        }
    }

    const fn kind(&self) -> &'static str {
        match self {
            Value::Number(_) => kind::NUMBER,
            Value::Boolean(_) => kind::BOOLEAN,
            Value::Nil => kind::NIL,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::Boolean(n) => write!(f, "{n}"),
            Value::Nil => write!(f, "Nil"),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a.partial_cmp(b),
            (Value::Boolean(a), Value::Boolean(b)) => a.partial_cmp(b),
            (Value::Nil, Value::Nil) => Some(cmp::Ordering::Equal),
            _ => None,
        }
    }
}
