use std::io::Write;

use clap::{Args, Parser, Subcommand};
use justerror::Error;

use lox::{tree_walk_interpreter::Interpreter, vm};

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
    Benchmark,
    RunVM,
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
        Command::Benchmark => {
            benchmark_command()?;
        }
        Command::RunVM => {
            run_vm()?;
        }
    }

    Ok(())
}

#[Error]
enum ReplCommandError {
    Io(#[from] std::io::Error),
    Tokenize(#[from] lox::tokenizer::TokenizeError),
    Parse(#[from] lox::parser::ParseErrors),
    Interpret(#[from] lox::tree_walk_interpreter::ExecutionError),
}

fn repl_command() -> Result<(), ReplCommandError> {
    println!("Welcome to the Lox REPL!");
    println!("EOF to exit. (Ctrl+D on *nix, Ctrl+Z on Windows)");

    let mut interpreter = Interpreter::default();

    loop {
        let mut input = String::new();

        print!("> ");
        std::io::stdout().flush()?;

        let read = std::io::stdin().read_line(&mut input)?;
        if read == 0 {
            break;
        }

        let source = input.trim();
        let tokens = lox::tokenizer::tokens(source)?;
        let program = lox::parser::program(&tokens)?;
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
    Tokenize(#[from] lox::tokenizer::TokenizeError),
    Parse(#[from] lox::parser::ParseErrors),
    Interpret(#[from] lox::tree_walk_interpreter::ExecutionError),
}

fn run_command(args: &RunArgs) -> Result<(), RuncCommandError> {
    let source = std::fs::read_to_string(&args.file)?;
    let tokens = lox::tokenizer::tokens(&source)?;
    let program = lox::parser::program(&tokens)?;
    let mut interpreter = Interpreter::default();
    if let Err(e) = interpreter.interpret(&program) {
        println!("{e}");
    }
    Ok(())
}

fn expand_command(args: &ExpandArgs) -> Result<(), Box<dyn std::error::Error>> {
    let file = std::fs::read_to_string(&args.file)?;
    let tokens = lox::tokenizer::tokens(&file)?;
    let ast = lox::parser::program(&tokens)?;
    println!("{}", ast);
    Ok(())
}

fn benchmark_command() -> Result<(), Box<dyn std::error::Error>> {
    let source = lox_fib_source();
    let tokens = lox::tokenizer::tokens(&source)?;
    let program = lox::parser::program(&tokens)?;
    let mut interpreter = Interpreter::default();

    let start = std::time::Instant::now();
    interpreter.interpret(&program)?;
    let lox_elapsed = start.elapsed();
    println!("Lox Took: {:?}", lox_elapsed);

    let start = std::time::Instant::now();
    rust_fib();
    let fib_elapsed = start.elapsed();
    println!("Fib Took: {:?}", fib_elapsed);

    println!(
        "Rust is {}x faster than lox-rs",
        lox_elapsed.as_secs_f64() / fib_elapsed.as_secs_f64()
    );

    Ok(())
}

fn lox_fib_source() -> &'static str {
    r#"
    fun fib(n) {
        if (n <= 1) {
            return n;
        }
        return fib(n-1) + fib(n-2);
    }
        
    print fib(40);
    "#
}

fn rust_fib() {
    println!("{}", fib(40));
}

fn fib(n: i64) -> i64 {
    if n <= 1 {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

fn run_vm() -> Result<(), Box<dyn std::error::Error>> {
    vm::main()?;
    Ok(())
}
