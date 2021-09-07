use std::fmt::Debug;
use std::fmt::Display;
use std::rc::Rc;

use crate::chunk::Chunk;
use crate::Result;

/// A Lox Value
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
    String(Rc<String>),
    Nil,
    Function(Rc<LoxFunction>),
    NativeFunction(NativeFunction),
}

/// A first-class function. Owns its code, and knows its name and arity
#[derive(Clone)]
pub struct LoxFunction {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: String,
}

/// A native function written in Rust that is callable from Lox
#[derive(Clone, Copy)]
pub struct NativeFunction {
    pub func: fn(args: &[Value]) -> Result<Value>,
    pub name: &'static str,
}

impl NativeFunction {
    /// Create a named `NativeFunction` with the given function pointer
    pub fn new(func: fn(&[Value]) -> Result<Value>, name: &'static str) -> Self {
        Self { func, name }
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NativeFunction")
            .field("name", &self.name)
            .finish()
    }
}

impl Debug for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LoxFunction")
            .field("name", &self.name)
            .field("arity", &self.arity)
            .finish()
    }
}

impl LoxFunction {
    /// Create a new, blank, `LoxFunction`
    pub fn new() -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name: String::new(),
        }
    }

    /// Create a named `LoxFunction`
    pub fn with_name(name: String) -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name,
        }
    }
}
impl Default for LoxFunction {
    fn default() -> Self {
        Self::new()
    }
}

impl PartialEq for LoxFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Value::Number(x) => write!(f, "{}", x),
            Value::Bool(x) => write!(f, "{}", x),
            Value::String(x) => write!(f, "{}", x),
            Value::Nil => write!(f, "nil"),
            Value::Function(x) => write!(f, "<fn {}>", x.name),
            Value::NativeFunction(x) => write!(f, "<native fn {}>", x.name),
        }
    }
}

impl Value {
    /// Check if a value contains a number
    pub fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }

    /// Check if a value contains a bool
    pub fn is_bool(&self) -> bool {
        matches!(self, Value::Bool(_))
    }

    /// Check if a value contains a Nil
    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }

    /// Check if a value contains a String
    pub fn is_string(&self) -> bool {
        matches!(self, Value::String(_))
    }

    /// Check if a value contains a Function
    pub fn is_function(&self) -> bool {
        matches!(self, Value::Function(_))
    }

    /// Get the contained number, if it exists
    pub fn as_number(&self) -> Option<f64> {
        if let Value::Number(x) = self {
            Some(*x)
        } else {
            None
        }
    }

    /// Perform type coercion to Bool
    pub fn coerce_bool(&self) -> bool {
        !matches!(self, Self::Nil | Self::Bool(false))
    }
}

impl std::ops::Add for Value {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a + b),
            (Value::String(a), Value::String(b)) => {
                Value::String(Rc::new((*a).clone() + b.as_str()))
            }
            _ => panic!("Cannot add two Values that are not both numbers"),
        }
    }
}

impl std::ops::Sub for Value {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a - b),
            _ => panic!("Cannot subtract two Values that are not both numbers"),
        }
    }
}

impl std::ops::Mul for Value {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a * b),
            _ => panic!("Cannot multiply two Values that are not both numbers"),
        }
    }
}

impl std::ops::Div for Value {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(a), Value::Number(b)) => Value::Number(a / b),
            _ => panic!("Cannot divide two Values that are not both numbers"),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a.partial_cmp(b),
            _ => panic!("Cannot compare two Values that are not both numbers"),
        }
    }
}
