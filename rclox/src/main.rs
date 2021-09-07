#![warn(missing_debug_implementations, missing_copy_implementations)]

use std::error::Error;
use std::io::Write;
use std::path::Path;
use std::result;

mod chunk;
mod compiler;
mod debug;
mod error;
mod instruction;
mod lexer;
mod value;
mod vm;

use vm::VM;

use text_io::read;

type Result<T> = std::result::Result<T, error::RcloxError>;

fn main() -> result::Result<(), Box<dyn Error>> {
    better_panic::install();

    if std::env::args().len() == 1 {
        repl()
    } else if std::env::args().len() == 2 {
        let file = std::env::args().nth(1).ok_or("Usage: rclox [file]")?;
        run_file(file)
    } else {
        Err("Usage: rclox [file]".into())
    }
}

fn repl() -> result::Result<(), Box<dyn Error>> {
    let mut vm = VM::new();
    loop {
        print!("> ");
        std::io::stdout().lock().flush()?;
        let line: String = read!("{}\n");

        if line.is_empty() {
            return Ok(());
        }

        if let Err(e) = vm.interpret(line.as_str()) {
            println!("{}", e)
        }
    }
}

fn run_file<P: AsRef<Path>>(path: P) -> result::Result<(), Box<dyn Error>> {
    let source = std::fs::read_to_string(path)?;
    let mut vm = VM::new();
    match vm.interpret(source.trim()) {
        Ok(_) => {}
        Err(e) => {
            println!("{}", e)
        }
    };
    Ok(())
}
