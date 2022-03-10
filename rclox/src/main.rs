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

use color_eyre::eyre::ContextCompat;
use vm::VM;

use text_io::read;
use color_eyre::eyre::eyre;

type Result<T> = std::result::Result<T, error::RcloxError>;

fn main() -> color_eyre::Result<()> {
    color_eyre::install().unwrap();

    if std::env::args().len() == 1 {
        repl()
    } else if std::env::args().len() == 2 {
        let file = std::env::args().nth(1).wrap_err("Usage: rclox [file]")?;
        run_file(file)
    } else {
        Err(eyre!("Usage: rclox [file]"))
    }
}

fn repl() -> color_eyre::Result<()> {
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

fn run_file<P: AsRef<Path>>(path: P) -> color_eyre::Result<()> {
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
