mod ast;
mod interpreter;
mod parser;
mod tokenizer;

use std::io::Write;

use clap::{Args, Parser, Subcommand};
use interpreter::InterpretError;
use justerror::Error;

use crate::interpreter::Interpreter;

#[derive(Debug, Parser)]
struct Cli {
    #[command(subcommand)]
    command: Option<Command>,
}

impl Cli {
    pub fn command(&self) -> &Command {
        self.command.as_ref().unwrap_or(&Command::Repl)
    }
}

#[derive(Debug, Subcommand)]
enum Command {
    Repl,
    Run(RunArgs),
}

#[derive(Debug, Args)]
struct RunArgs {
    file: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Cli::parse();

    match args.command() {
        Command::Repl => {
            repl_command()?;
        }
        Command::Run(args) => {
            run_command(args)?;
        }
    }

    Ok(())
}

#[Error]
enum ReplCommandError {
    Io(#[from] std::io::Error),
    Interpret(#[from] interpreter::InterpretError),
}

fn repl_command() -> Result<(), ReplCommandError> {
    println!("Welcome to the Lox REPL!");
    println!("EOF to exit. (Ctrl+D on *nix, Ctrl+Z on Windows)");

    let mut interpreter = Interpreter::new();

    loop {
        let mut input = String::new();

        print!("> ");
        std::io::stdout().flush()?;

        let read = std::io::stdin().read_line(&mut input)?;
        if read == 0 {
            break;
        }

        let trimmed_input = input.trim();
        let value = interpreter.interpret(&trimmed_input)?;
        println!("{}", value);

        input.clear()
    }
    Ok(())
}

#[Error]
enum RuncCommandError {
    Io(#[from] std::io::Error),
    Interpret(#[from] InterpretError),
}

fn run_command(args: &RunArgs) -> Result<(), RuncCommandError> {
    let file = std::fs::read_to_string(&args.file)?;
    let mut interpreter = Interpreter::new();
    interpreter.interpret(&file)?;
    Ok(())
}
