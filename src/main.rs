use std::{cell::RefCell, io::Write, rc::Rc};

use clap::{Args, Parser, Subcommand, ValueEnum};
use lox::vm::Vm;
use thiserror::Error;

#[derive(Debug, Parser)]
struct Cli {
    #[command(subcommand)]
    command: Option<Command>,
}

const DEFAULT_COMMAND: Command = Command::Repl(ReplArgs {
    interpreter: InterpreterKind::Bytecode,
});

impl Cli {
    pub fn command(&self) -> &Command {
        self.command.as_ref().unwrap_or(&DEFAULT_COMMAND)
    }
}

#[derive(Debug, Subcommand)]
enum Command {
    Run(RunArgs),
    Repl(ReplArgs),
    Benchmark(BenchmarkArgs),
}

#[derive(Debug, Clone, Default, ValueEnum)]
enum InterpreterKind {
    #[default]
    Bytecode,
    TreeWalk,
}

#[derive(Debug, Args)]
struct ReplArgs {
    #[clap(short, long, default_value_t, value_enum)]
    interpreter: InterpreterKind,
}

impl Default for ReplArgs {
    fn default() -> Self {
        Self {
            interpreter: InterpreterKind::Bytecode,
        }
    }
}

#[derive(Debug, Args)]
struct RunArgs {
    file: String,
    #[clap(short, long, default_value_t, value_enum)]
    interpreter: InterpreterKind,
}

#[derive(Debug, Args)]
struct BenchmarkArgs {
    #[clap(short, long, default_value_t, value_enum)]
    interpreter: InterpreterKind,
}

fn main() {
    let args = Cli::parse();

    match args.command() {
        Command::Repl(args) => {
            repl_command(args);
        }
        Command::Run(args) => {
            run_command(args);
        }
        Command::Benchmark(args) => {
            benchmark_command(args);
        }
    }
}

fn repl_command(args: &ReplArgs) {
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
        match interpret(&args.interpreter, &source) {
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
    if let Err(e) = interpret(&args.interpreter, &source) {
        println!("{e}");
    }
}

fn benchmark_command(args: &BenchmarkArgs) {
    let source = lox_fib_source();

    let start = std::time::Instant::now();
    if let Err(e) = interpret(&args.interpreter, source) {
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
enum InterpretError<'a> {
    #[error(transparent)]
    Compile(lox::compiler::CompileError<'a>),
    #[error(transparent)]
    Interpret(#[from] lox::vm::InterpretError),
    #[error(transparent)]
    TreeWalk(TreeWalkInterpretError<'a>),
}

impl<'a> From<lox::compiler::CompileError<'a>> for InterpretError<'a> {
    fn from(e: lox::compiler::CompileError<'a>) -> Self {
        Self::Compile(e)
    }
}

impl<'a> From<TreeWalkInterpretError<'a>> for InterpretError<'a> {
    fn from(e: TreeWalkInterpretError<'a>) -> Self {
        Self::TreeWalk(e)
    }
}

fn interpret<'a>(
    interpreter_args: &InterpreterKind,
    source: &'a str,
) -> Result<(), InterpretError<'a>> {
    match interpreter_args {
        InterpreterKind::Bytecode => interpret_bytecode(source)?,
        InterpreterKind::TreeWalk => interpret_tree_walk(source)?,
    }
    Ok(())
}

fn interpret_bytecode(source: &str) -> Result<(), InterpretError> {
    let mut vm = Vm::new();

    let chunk = lox::compiler::compile(source)?;

    vm.interpret(&chunk)?;

    Ok(())
}

#[derive(Debug, Error)]
enum TreeWalkInterpretError<'a> {
    #[error(transparent)]
    Tokenize(lox::tokenizer::TokenizeError<'a>),
    #[error(transparent)]
    Parse(lox::parser::ParseErrors<'a>),
    #[error(transparent)]
    Interpret(#[from] lox::tree_walk_interpreter::ExecutionError),
}

impl<'a> From<lox::tokenizer::TokenizeError<'a>> for TreeWalkInterpretError<'a> {
    fn from(e: lox::tokenizer::TokenizeError<'a>) -> Self {
        Self::Tokenize(e)
    }
}

impl<'a> From<lox::parser::ParseErrors<'a>> for TreeWalkInterpretError<'a> {
    fn from(e: lox::parser::ParseErrors<'a>) -> Self {
        Self::Parse(e)
    }
}

fn interpret_tree_walk(source: &str) -> Result<(), TreeWalkInterpretError> {
    let tokens = lox::tokenizer::tokens(source)?;
    let ast = lox::parser::program(&tokens)?;
    let mut interpreter =
        lox::tree_walk_interpreter::Interpreter::new(Rc::new(RefCell::new(std::io::stdout())));
    interpreter.interpret(&ast)?;

    Ok(())
}
