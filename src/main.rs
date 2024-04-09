use std::io::Write;

use clap::{Args, Parser, Subcommand};
use justerror::Error;

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
            repl()?;
        }
        Command::Run(args) => {
            run(args)?;
        }
    }

    Ok(())
}

#[Error]
enum ReplError {
    Io(#[from] std::io::Error),
}

fn repl() -> Result<(), ReplError> {
    println!("Welcome to the Lox REPL!");
    println!("EOF to exit. (Ctrl+D on *nix, Ctrl+Z on Windows)");
    loop {
        let mut input = String::new();

        print!("> ");
        std::io::stdout().flush()?;

        let read = std::io::stdin().read_line(&mut input)?;
        if read == 0 {
            break;
        }

        let trimmed_input = input.trim();

        println!("{}", trimmed_input);

        input.clear()
    }
    Ok(())
}

#[Error]
enum RunError {
    Io(#[from] std::io::Error),
}

fn run(args: &RunArgs) -> Result<(), RunError> {
    let file = std::fs::read_to_string(&args.file)?;
    println!("{}", file);
    Ok(())
}
