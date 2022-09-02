#![allow(dead_code)]

use std::path::{Path, PathBuf};

use clap::Parser;
use error::LoxError;
use rustyline::{error::ReadlineError, Editor};
use virtual_machine::VirtualMachine;

mod chunk;
mod debug;
mod error;
mod lexer;
mod opcode;
mod parser;
mod value;
mod virtual_machine;

#[derive(Parser, Debug)]
struct Opt {
    file: Option<PathBuf>,
}

fn main() {
    let opt = Opt::parse();

    if let Err(error) = if let Some(path) = opt.file {
        file(&path)
    } else {
        repl()
    } {
        eprintln!("{error}");
    }
}

fn repl() -> Result<(), LoxError> {
    let mut rl = Editor::<()>::new()?;
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                println!("Line: {}", line);

                let mut vm = VirtualMachine::init(&line)?;
                vm.start()?;
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                return Err(err.into());
            }
        }
    }

    rl.save_history("history.txt")?;
    Ok(())
}

fn file(path: &Path) -> Result<(), LoxError> {
    let source = std::fs::read_to_string(path)?;

    let mut vm = VirtualMachine::init(&source)?;

    vm.start()
}
