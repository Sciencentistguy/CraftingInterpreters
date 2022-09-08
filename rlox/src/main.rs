#![allow(dead_code)]

use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use clap::Parser;
use error::LoxError;
use once_cell::sync::Lazy;
use parking_lot::Mutex;
use rustyline::{error::ReadlineError, Editor};
use string_interner::{backend::BufferBackend, symbol::SymbolUsize, StringInterner};
use virtual_machine::VirtualMachine;

mod chunk;
mod compiler;
mod debug;
mod error;
mod lexer;
mod opcode;
mod value;
mod virtual_machine;

type Interner = StringInterner<BufferBackend<SymbolUsize>>;

static INTERNER: Lazy<Arc<Mutex<Interner>>> = Lazy::new(|| Arc::new(Mutex::new(Interner::new())));

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

    let mut vm = VirtualMachine::new();

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());

                if let Err(err) = vm.reset(&line) {
                    eprintln!("{err}");
                    continue;
                }

                if let Err(err) = vm.start() {
                    eprintln!("{err}");
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("Ctrl-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("Ctrl-D");
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

    let mut vm = VirtualMachine::new();

    vm.reset(&source)?;

    vm.start()
}
