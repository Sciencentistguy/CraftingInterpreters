mod chunk;
mod compiler;
mod debug;
mod lexer;
mod value;
mod vm;

use chunk::Chunk;
use chunk::OpCode;
use compiler::Compiler;
use lexer::Lexer;
use value::Value;
use vm::VM;

use std::io::Write;
use std::path::Path;

use text_io::read;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    if std::env::args().len() == 1 {
        repl()
    } else if std::env::args().len() == 2 {
        let file = match std::env::args().nth(1) {
            Some(x) => x,
            None => return Err("Usage: rclox [file]".into()),
        };
        run_file(file)
    } else {
        return Err("Usage: rclox [file]".into());
    }
}

fn repl() -> Result<(), Box<dyn std::error::Error>> {
    loop {
        print!("> ");
        std::io::stdout().lock().flush()?;
        let line: String = read!("{}\n");

        interpret(line);
    }
}

fn run_file<P: AsRef<Path>>(path: P) -> Result<(), Box<dyn std::error::Error>> {
    let source = std::fs::read_to_string(path)?;
    interpret(source);

    Ok(())
}

fn interpret(source: String) {
    let mut lexer = Lexer::new(&source);
    let tokens = lexer.lex();

    //let mut compiler = Compiler::new(tokens);
}
