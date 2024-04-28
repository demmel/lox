use std::io::Write;

use clap::{Args, Parser, Subcommand};

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
    Benchmark,
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
    let args = Cli::parse();

    match args.command() {
        Command::Repl => {
            repl_command();
        }
        Command::Run(args) => {
            run_command(args);
        }
        Command::Benchmark => {
            benchmark_command();
        }
    }
}

fn repl_command() {
    println!("Welcome to the Lox REPL!");
    println!("EOF to exit. (Ctrl+D on *nix, Ctrl+Z on Windows)");

    loop {
        let mut input = String::new();

        print!("> ");
        std::io::stdout()
            .flush()
            .expect("should be able to flush stdout");

        let read = std::io::stdin()
            .read_line(&mut input)
            .expect("should be able to read line from stdin");

        if read == 0 {
            break;
        }

        let source = input.trim();
        match interpret(&source) {
            Ok(()) => {}
            Err(e) => {
                println!("Error: {}", e)
            }
        }

        input.clear()
    }
}

fn run_command(args: &RunArgs) {
    let source = std::fs::read_to_string(&args.file).expect("should be able to read source file");
    if let Err(e) = interpret(&source) {
        println!("{e}");
    }
}

fn benchmark_command() {
    let source = lox_fib_source();

    let start = std::time::Instant::now();
    if let Err(e) = interpret(source) {
        println!("Failed to run lox fib code: {e}");
        std::process::exit(1);
    }
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

#[derive(Debug, thiserror::Error)]
enum InterpretError {
    #[error(transparent)]
    Tokenize(#[from] lox::tokenizer::TokenizeError),
}

fn interpret(source: &str) -> Result<(), InterpretError> {
    compile(source)?;
    Ok(())
}

fn compile(source: &str) -> Result<(), lox::tokenizer::TokenizeError> {
    let mut tokenizer = lox::tokenizer::Tokenizer::new(source);
    let mut line = 0;
    loop {
        let token = tokenizer.token()?;
        if token.line != line {
            print!("{:4} ", token.line);
            line = token.line;
        } else {
            print!("   | ");
        }

        println!("{:<10} {}", format!("{:?}", token.token_type), token.lexeme);

        if token.token_type == lox::tokenizer::TokenType::Eof {
            break;
        }
    }

    Ok(())
}
