use std::fmt::Display;

pub enum Value {
    Number(f64),
    //Value(&str),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Value::Number(x) => write!(f, "{}", x),
        }
    }
}
