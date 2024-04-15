use std::{cell::RefCell, rc::Rc};

use lox::interpreter::Interpreter;

fn test_valid_program(source: &str, expected_output: &str) {
    let tokens = lox::tokenizer::tokens(source).expect("Tokenize should work on valid program");
    let program = lox::parser::program(&tokens).expect("Parse should work on valid program");
    let output = Rc::new(RefCell::new(Vec::new()));
    let mut interpreter = Interpreter::new(output.clone());
    interpreter
        .interpret(&program)
        .expect("Interpret should work on valid program");
    let output = String::from_utf8(output.take()).expect("Output should be valid UTF-8");
    assert_eq!(output, expected_output);
}

#[test]
fn test_fib() {
    let source = r#"
    fun fib(n) {
        if (n <= 1) return n;
        return fib(n - 1) + fib(n - 2);
    }
    
    for (var i = 0; i < 10; i = i + 1) {
        print fib(i);
    }
    "#;
    let expected_output = "0\n1\n1\n2\n3\n5\n8\n13\n21\n34\n";
    test_valid_program(source, expected_output);
}

#[test]
fn test_closure() {
    let source = r#"
    fun makeCounter() {
        var i = 0;
        fun count() {
            i = i + 1;
            return i;
        }
        return count;
    }
    
    var counter = makeCounter();
    print counter(); // 1
    print counter(); // 2
    "#;
    let expected_output = "1\n2\n";
    test_valid_program(source, expected_output);
}
