use rlox::{error::LoxError, virtual_machine::VirtualMachine};

use clap::Parser;
use rustyline::{Editor, error::ReadlineError};
use std::path::{Path, PathBuf};

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

    let mut line = 0;

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(source) => {
                rl.add_history_entry(source.as_str());
                rl.save_history("history.txt")?;

                line += 1;

                if let Err(err) = vm.reset(&source, line - 1) {
                    eprintln!("{err}");
                    continue;
                }

                if let Err(err) = vm.start() {
                    eprintln!("{err}");
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("Ctrl-C");
                continue;
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
    Ok(())
}

fn file(path: &Path) -> Result<(), LoxError> {
    let source = std::fs::read_to_string(path)?;

    let mut vm = VirtualMachine::new();

    vm.reset(&source, 0)?;

    vm.start()
}