#![allow(unused_imports)]

type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

mod block;
mod bool;
mod closure;
mod comment;
mod expression;
mod r#for;
mod function;
mod r#if;
mod logical_operators;
mod nil;
mod number;
mod operator;
mod print;
mod variable;
mod r#while;

#[macro_export]
macro_rules! check_error_msg {
    ($invocation:expr, $expected:expr, $panic_msg: expr) => {{
        if let Err(e) = $invocation {
            println!("{}", e);
            assert_eq!(e.to_string(), $expected, "Incorrect error message.");
        } else {
            panic!($panic_msg);
        }
    }};
}
