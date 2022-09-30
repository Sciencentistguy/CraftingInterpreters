use core::fmt;
use std::{
    cmp,
    collections::HashMap,
    fmt::Display,
    ops::{Neg, Not},
    sync::Arc,
};

use parking_lot::Mutex;
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
    Closure(Arc<LoxClosure>),
    Class(Arc<LoxClass>),
    Instance(Arc<LoxInstance>),
}

#[derive(Debug)]
pub struct LoxInstance {
    pub class: Arc<LoxClass>,
    pub fields: Mutex<HashMap<SymbolUsize, Value>>,
}

impl LoxInstance {
    pub fn new(class: Arc<LoxClass>) -> Self {
        Self {
            class,
            fields: Default::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LoxClass {
    name: SymbolUsize,
}

impl LoxClass {
    pub fn new(name: SymbolUsize) -> Self {
        Self { name }
    }
}

#[derive(Debug)]
pub struct LoxClosure {
    pub function: Arc<LoxFunction>,
    pub upvalues: Vec<Arc<RuntimeUpvalue>>,
}

impl LoxClosure {
    pub fn new(function: Arc<LoxFunction>) -> Self {
        Self {
            function,
            upvalues: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct RuntimeUpvalue {
    pub slot: usize,
    closed: Mutex<Option<Value>>,
}

impl RuntimeUpvalue {
    pub fn new(slot: usize) -> Self {
        Self {
            slot,
            closed: Mutex::new(None),
        }
    }

    pub fn get(&self, stack: &[Value]) -> Value {
        if let Some(closed) = &mut *self.closed.lock() {
            closed.clone()
        } else {
            stack.get(self.slot).cloned().expect(
                "Internal error: Upvalue slot out of bounds. Should probably have been closed",
            )
        }
    }

    pub fn set(&self, val: Value, stack: &mut [Value]) {
        if let Some(closed) = &mut *self.closed.lock() {
            *closed = val;
        } else {
            stack[self.slot] = val;
        }
    }

    pub fn close(&self, val: Value) {
        let mut closed = self.closed.lock();
        if closed.is_none() {
            eprintln!(
                "Closing upvalue for slot {:?} with value '{val}'",
                self.slot
            );
            *closed = Some(val);
        } else {
            eprintln!("Attempted to close already-closed value")
        }
    }

    pub fn is_open(&self, slot: usize) -> bool {
        self.slot == slot && self.closed.lock().is_none()
    }
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
    upvalue_count: usize,
}

impl LoxFunction {
    pub fn new(arity: usize, chunk: Chunk, name: SymbolUsize) -> Self {
        Self {
            arity,
            chunk,
            name,
            upvalue_count: 0,
        }
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
    pub(super) const CLOSURE: &str = "closure";
    pub(super) const CLASS: &str = "class";
    pub(super) const INSTANCE: &str = "instance";
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
            Value::Closure(_) => kind::CLOSURE,
            Value::Class(_) => kind::CLASS,
            Value::Instance(_) => kind::INSTANCE,
        }
    }

    pub fn as_function(&self) -> Option<&Arc<LoxFunction>> {
        if let Self::Function(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_closure(&self) -> Option<&Arc<LoxClosure>> {
        if let Self::Closure(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl Value {
    pub fn arity(&self) -> Option<usize> {
        match self {
            Value::Closure(c) => Some(c.function.arity),
            Value::NativeFn(f) => Some(f.arity),
            Value::Class(_) => Some(0),
            _ => None,
        }
    }

    pub fn get_closure(&self) -> Option<Arc<LoxClosure>> {
        match self {
            Value::Closure(f) => Some(f.clone()),
            _ => None,
        }
    }

    pub fn as_nativefn(&self) -> Option<&NativeFn> {
        match self {
            Value::NativeFn(f) => Some(f),
            _ => None,
        }
    }
    pub fn get_class(&self) -> Option<Arc<LoxClass>> {
        match self {
            Value::Class(c) => Some(c.clone()),
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
            Value::Closure(n) => write!(f, "{n}"),
            Value::Class(n) => write!(f, "{n}"),
            Value::Instance(n) => write!(f, "{n}"),
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

impl fmt::Display for LoxClosure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<fn {}>",
            INTERNER.lock().resolve(self.function.name).unwrap()
        )
    }
}

impl fmt::Display for LoxClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<class {}>", INTERNER.lock().resolve(self.name).unwrap())
    }
}

impl fmt::Display for LoxInstance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<instance of {}>", self.class)
    }
}
