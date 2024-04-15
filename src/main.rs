mod ast;
mod interpreter;
mod parser;
mod span;
mod tokenizer;

use std::io::Write;

use clap::{Args, Parser, Subcommand};
use interpreter::ExecutionError;
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
    Run(RunArgs),
    Repl,
    Expand(ExpandArgs),
}

#[derive(Debug, Args)]
struct RunArgs {
    file: String,
}

#[derive(Debug, Args)]
struct ExpandArgs {
    file: String,
}

fn main() {
    match runner() {
        Ok(()) => {}
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    }
}

fn runner() -> Result<(), Box<dyn std::error::Error>> {
    let args = Cli::parse();

    match args.command() {
        Command::Repl => {
            repl_command()?;
        }
        Command::Run(args) => {
            run_command(args)?;
        }
        Command::Expand(args) => {
            expand_command(args)?;
        }
    }

    Ok(())
}

#[Error]
enum ReplCommandError {
    Io(#[from] std::io::Error),
    Parse(#[from] parser::ParseErrors),
    Tokenize(#[from] tokenizer::TokenizeError),
    Interpret(#[from] interpreter::ExecutionError),
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

        let source = input.trim();
        let tokens = tokenizer::tokens(source)?;
        let program = parser::program(&tokens)?;
        match interpreter.interpret(&program) {
            Ok(()) => {}
            Err(e) => {
                println!("Error: {}", e)
            }
        }

        input.clear()
    }
    Ok(())
}

#[Error]
enum RuncCommandError {
    Io(#[from] std::io::Error),
    Tokenize(#[from] tokenizer::TokenizeError),
    Parse(#[from] parser::ParseErrors),
    Interpret(#[from] ExecutionError),
}

fn run_command(args: &RunArgs) -> Result<(), RuncCommandError> {
    let source = std::fs::read_to_string(&args.file)?;
    let tokens = tokenizer::tokens(&source)?;
    let program = parser::program(&tokens)?;
    let mut interpreter = Interpreter::new();
    if let Err(e) = interpreter.interpret(&program) {
        println!("{e}");
    }
    Ok(())
}

fn expand_command(args: &ExpandArgs) -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string(&args.file)?;
    let tokens = tokenizer::tokens(&file)?;
    let ast = parser::program(&tokens)?;
    println!("{}", ast);
    Ok(())
}
