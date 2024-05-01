use crate::{
    bytecode::{Chunk, OpCode},
    tokenizer::{Token, TokenType, TokenizeError, Tokenizer},
    vm::Value,
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl Precedence {
    fn higher(&self) -> Precedence {
        match self {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => Precedence::Primary,
        }
    }
}

#[derive(Debug)]
pub struct CompileError<'a> {
    errors: Vec<CompileErrorKind<'a>>,
}

impl std::error::Error for CompileError<'_> {}

impl std::fmt::Display for CompileError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Compile error: ")?;
        for error in &self.errors {
            writeln!(f, "{}", error)?;
        }
        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
pub enum CompileErrorKind<'a> {
    #[error("Failed to tokenize")]
    Tokenize(TokenizeError<'a>),
    #[error("Expected token '{expected}', found '{found}' at line {line}")]
    ExpectedToken {
        expected: TokenType,
        found: TokenType,
        line: usize,
    },
    #[error("Expected one of '{expected:?}', found '{found}' at line {line}")]
    ExpectedOneOf {
        expected: &'static [TokenType],
        found: TokenType,
        line: usize,
    },
    #[error("Failed to parse number")]
    ParseFloatError(#[from] std::num::ParseFloatError),
}

pub fn compile(source: &str) -> Result<Chunk, CompileError> {
    let tokenizer = Tokenizer::new(source);
    let compiler_state = Compiler::new(tokenizer);
    compiler_state.compile()
}

struct Compiler<'a> {
    tokenizer: Tokenizer<'a>,
    chunk: Chunk,
    errors: Vec<CompileErrorKind<'a>>,
    current: Token<'a>,
    previous: Token<'a>,
}

impl<'a> Compiler<'a> {
    fn new(tokenizer: Tokenizer<'a>) -> Self {
        Self {
            tokenizer,
            chunk: Chunk::new(),
            errors: Vec::new(),
            current: Token {
                lexeme: "",
                token_type: TokenType::Eof,
                line: 1,
                column: 1,
            },
            previous: Token {
                lexeme: "",
                token_type: TokenType::Eof,
                line: 1,
                column: 1,
            },
        }
    }

    fn compile(mut self) -> Result<Chunk, CompileError<'a>> {
        self.advance();
        self.expression();
        self.consume(TokenType::Eof);
        self.chunk.add_bytecode(OpCode::Return, self.previous.line);

        if self.errors.is_empty() {
            #[cfg(feature = "disassemble")]
            self.chunk.disassemble("code");
            Ok(self.chunk)
        } else {
            Err(CompileError {
                errors: self.errors,
            })
        }
    }

    fn advance(&mut self) {
        self.previous = self.current.clone();
        loop {
            match self.tokenizer.token() {
                Ok(token) => {
                    self.current = token;
                    break;
                }
                Err(e) => {
                    self.log_error(CompileErrorKind::Tokenize(e));
                }
            }
        }
    }

    fn consume(&mut self, token_type: TokenType) {
        if self.current.token_type == token_type {
            self.advance();
        } else {
            self.log_error(CompileErrorKind::ExpectedToken {
                expected: token_type,
                found: self.current.token_type,
                line: self.current.line,
            });
        }
    }

    fn log_error(&mut self, error: CompileErrorKind<'a>) {
        self.errors.push(error);
        self.tokenizer.recover();
    }

    fn precedence(&mut self, precedence: Precedence) {
        self.advance();
        match self.previous.token_type {
            TokenType::LeftParen => self.grouping(),
            TokenType::Minus | TokenType::Bang => self.unary(),
            TokenType::Number => self.number(),
            TokenType::Nil | TokenType::True | TokenType::False => self.literal(),
            t => {
                self.log_error(CompileErrorKind::ExpectedOneOf {
                    expected: &[TokenType::LeftParen, TokenType::Minus, TokenType::Number],
                    found: t,
                    line: self.previous.line,
                });
            }
        }

        while precedence <= get_token_type_inifix_precedence(&self.current.token_type) {
            self.advance();
            match self.previous.token_type {
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Star
                | TokenType::Slash
                | TokenType::BangEqual
                | TokenType::EqualEqual
                | TokenType::Greater
                | TokenType::GreaterEqual
                | TokenType::Less
                | TokenType::LessEqual => {
                    self.binary();
                }
                _ => {
                    self.log_error(CompileErrorKind::ExpectedOneOf {
                        expected: &[
                            TokenType::Plus,
                            TokenType::Minus,
                            TokenType::Star,
                            TokenType::Slash,
                        ],
                        found: self.previous.token_type,
                        line: self.previous.line,
                    });
                }
            }
        }
    }

    fn expression(&mut self) {
        self.precedence(Precedence::Assignment);
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen);
    }

    fn unary(&mut self) {
        let token = self.previous.clone();
        self.precedence(Precedence::Unary);
        match token.token_type {
            TokenType::Minus => self.chunk.add_bytecode(OpCode::Negate, token.line),
            TokenType::Bang => self.chunk.add_bytecode(OpCode::Not, token.line),
            _ => unreachable!(),
        }
    }

    fn binary(&mut self) {
        let token = self.previous.clone();
        let precedence = get_token_type_inifix_precedence(&token.token_type);
        self.precedence(precedence.higher());
        match token.token_type {
            TokenType::Plus => self.chunk.add_bytecode(OpCode::Add, token.line),
            TokenType::Minus => self.chunk.add_bytecode(OpCode::Subtract, token.line),
            TokenType::Star => self.chunk.add_bytecode(OpCode::Multiply, token.line),
            TokenType::Slash => self.chunk.add_bytecode(OpCode::Divide, token.line),
            TokenType::BangEqual => {
                self.chunk.add_bytecode(OpCode::Equal, token.line);
                self.chunk.add_bytecode(OpCode::Not, token.line);
            }
            TokenType::EqualEqual => self.chunk.add_bytecode(OpCode::Equal, token.line),
            TokenType::Greater => self.chunk.add_bytecode(OpCode::Greater, token.line),
            TokenType::GreaterEqual => {
                self.chunk.add_bytecode(OpCode::Less, token.line);
                self.chunk.add_bytecode(OpCode::Not, token.line);
            }
            TokenType::Less => self.chunk.add_bytecode(OpCode::Less, token.line),
            TokenType::LessEqual => {
                self.chunk.add_bytecode(OpCode::Greater, token.line);
                self.chunk.add_bytecode(OpCode::Not, token.line);
            }
            _ => unreachable!(),
        }
    }

    fn number(&mut self) {
        let token = self.previous.clone();
        let number = match token.lexeme.parse::<f64>() {
            Ok(number) => number,
            Err(e) => {
                self.log_error(e.into());
                return;
            }
        };
        self.emit_constant(Value::Number(number), &token);
    }

    fn literal(&mut self) {
        match self.previous.token_type {
            TokenType::True => self.chunk.add_bytecode(OpCode::True, self.previous.line),
            TokenType::False => self.chunk.add_bytecode(OpCode::False, self.previous.line),
            TokenType::Nil => self.chunk.add_bytecode(OpCode::Nil, self.previous.line),
            _ => unreachable!(),
        }
    }

    fn emit_constant(&mut self, value: Value, token: &Token<'_>) {
        let c = self.chunk.add_constant(value);
        self.chunk.add_bytecode(OpCode::Constant, token.line);
        self.chunk.add_bytecode(c, token.line);
    }
}

fn get_token_type_inifix_precedence(token_type: &TokenType) -> Precedence {
    match token_type {
        TokenType::Plus | TokenType::Minus => Precedence::Term,
        TokenType::Star | TokenType::Slash => Precedence::Factor,
        _ => Precedence::None,
    }
}
