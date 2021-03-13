use std::io::Write;
use std::path::Path;

mod chunk;
mod compiler;
mod debug;
mod lexer;
mod opcode;
mod value;
mod vm;

use vm::VM;

use text_io::read;

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    if std::env::args().len() == 1 {
        repl()
    } else if std::env::args().len() == 2 {
        let file = std::env::args()
            .nth(1)
            .ok_or_else(|| "Usage: rclox [file]".to_string())?;
        run_file(file)
    } else {
        Err("Usage: rclox [file]".into())
    }
}

fn repl() -> Result<()> {
    let mut vm = VM::new();
    loop {
        print!("> ");
        std::io::stdout().lock().flush()?;
        let line: String = read!("{}\n");

        if line.is_empty() {
            return Ok(());
        }

        match vm.interpret(line.as_str()) {
            Ok(_) => {}
            Err(e) => {
                println!("{}", e)
            }
        };
    }
}

fn run_file<P: AsRef<Path>>(path: P) -> Result<()> {
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
