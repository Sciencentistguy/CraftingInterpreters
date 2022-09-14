use core::fmt;
use std::{
    cmp,
    fmt::Display,
    mem,
    ops::{Neg, Not},
    sync::Arc,
};

use string_interner::symbol::SymbolUsize;

use crate::{chunk::Chunk, error::LoxError, INTERNER};

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Nil,
    String(SymbolUsize),
    Function(Arc<LoxFunction>),
    NativeFn(Arc<NativeFn>),
}

#[allow(clippy::type_complexity)]
pub struct NativeFn {
    arity: usize,
    pub function: Box<dyn Fn(&[Value]) -> Result<Value, LoxError>>,
    name: &'static str,
}

impl NativeFn {
    pub fn new<F>(function: F, arity: usize, name: &'static str) -> Self
    where
        F: Fn(&[Value]) -> Result<Value, LoxError> + 'static,
    {
        Self {
            arity,
            function: Box::new(function),
            name,
        }
    }
}

#[derive(Clone)]
pub struct LoxFunction {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: SymbolUsize,
}

/// A value that can be called
#[repr(transparent)]
#[derive(Debug, Clone)]
pub struct Callable(Value);

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
    pub(super) const NATIVE_FUNCTION: &str = "native function";
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

    pub const fn kind(&self) -> &'static str {
        match self {
            Value::Number(_) => kind::NUMBER,
            Value::Boolean(_) => kind::BOOLEAN,
            Value::Nil => kind::NIL,
            Value::String(_) => kind::STRING,
            Value::Function(_) => kind::FUNCTION,
            Value::NativeFn(_) => kind::NATIVE_FUNCTION,
        }
    }

    pub fn as_callable(&self) -> Option<&Callable> {
        if matches!(self, Value::Function(_) | Value::NativeFn(_)) {
            // Safety: We just checked that it is a function. Repr(transparent) means transmute is
            // okay
            Some(unsafe { mem::transmute(self) })
        } else {
            None
        }
    }
}

impl Callable {
    pub fn arity(&self) -> usize {
        match &self.0 {
            Value::Function(f) => f.arity,
            Value::NativeFn(f) => f.arity,
            _ => unreachable!(),
        }
    }

    pub fn into_function(self) -> Arc<LoxFunction> {
        match self.0 {
            Value::Function(f) => f,
            _ => unreachable!(),
        }
    }

    pub fn as_nativefn(&self) -> Option<&NativeFn> {
        match &self.0 {
            Value::NativeFn(f) => Some(f),
            _ => None,
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
            Value::Function(n) => write!(f, "{n}"),
            Value::NativeFn(n) => write!(f, "{n}"),
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

impl fmt::Debug for LoxFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LoxFunction")
            .field("arity", &self.arity)
            .field("chunk", &self.chunk)
            .field("name", &INTERNER.lock().resolve(self.name))
            .finish()
    }
}

impl fmt::Display for LoxFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fn {}>", INTERNER.lock().resolve(self.name).unwrap())
    }
}

impl fmt::Debug for NativeFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("NativeFn")
            .field("arity", &self.arity)
            .field("name", &self.name)
            .finish()
    }
}

impl fmt::Display for NativeFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn {}>", self.name)
    }
}
