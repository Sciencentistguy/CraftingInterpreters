use std::{
    cmp,
    fmt::Display,
    ops::{Neg, Not},
};

use string_interner::symbol::SymbolUsize;

use crate::{chunk::Chunk, error::LoxError, INTERNER};

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Nil,
    String(SymbolUsize),
    Function(Box<LoxFunction>),
}

#[derive(Debug, Clone)]
pub struct LoxFunction {
    arity: usize,
    pub chunk: Chunk,
    pub name: SymbolUsize,
}

impl LoxFunction {
    pub fn new(arity: usize, chunk: Chunk, name: SymbolUsize) -> Self {
        Self { arity, chunk, name }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum FunctionKind {
    Function,
    Script,
}

mod kind {
    pub(super) const NUMBER: &str = "number";
    pub(super) const BOOLEAN: &str = "boolean";
    pub(super) const NIL: &str = "nil";
    pub(super) const STRING: &str = "string";
    pub(super) const FUNCTION: &str = "function";
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

    pub fn to_bool(&self) -> bool {
        // I prefer it
        #[allow(clippy::match_like_matches_macro)]
        match self {
            Value::Boolean(false) | Value::Nil => false,
            _ => true,
        }
    }

    pub fn not(&self) -> Value {
        Value::Boolean(self.to_bool().not())
    }

    pub fn add(&self, other: &Value) -> Result<Value, LoxError> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
            (Value::String(a), Value::String(b)) => {
                let mut interner = INTERNER.lock();
                let a = interner.resolve(*a).ok_or(LoxError::MissingString(*a))?;
                let b = interner.resolve(*b).ok_or(LoxError::MissingString(*b))?;

                let s = format!("{a}{b}");

                Ok(Value::String(interner.get_or_intern(s)))
            }
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
            Value::String(_) => kind::STRING,
            Value::Function(_) => kind::FUNCTION,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::Boolean(n) => write!(f, "{n}"),
            Value::Nil => write!(f, "Nil"),
            Value::String(n) => write!(f, "{}", INTERNER.lock().resolve(*n).unwrap()),
            Value::Function(func) => {
                write!(f, "<fn {}>", INTERNER.lock().resolve(func.name).unwrap())
            }
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a.partial_cmp(b),
            (Value::Boolean(a), Value::Boolean(b)) => a.partial_cmp(b),
            (Value::Nil, Value::Nil) => Some(cmp::Ordering::Equal),
            (Value::String(a), Value::String(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        self.partial_cmp(other)
            .map(|x| x == cmp::Ordering::Equal)
            .unwrap_or(false)
    }
}
