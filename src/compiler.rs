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
    tokenizer: PeekableTokenizer<'a>,
    chunk: Chunk,
    errors: Vec<CompileErrorKind<'a>>,
}

impl<'a> Compiler<'a> {
    fn new(tokenizer: Tokenizer<'a>) -> Self {
        Self {
            tokenizer: PeekableTokenizer::new(tokenizer),
            chunk: Chunk::new(),
            errors: Vec::new(),
        }
    }

    fn compile(mut self) -> Result<Chunk, CompileError<'a>> {
        self.expression();
        let token = self.consume(TokenType::Eof);
        self.chunk.add_bytecode(OpCode::Return, token.line);

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

    fn peek(&mut self) -> Token<'a> {
        loop {
            if let Err(e) = self.tokenizer.peek() {
                self.errors.push(CompileErrorKind::Tokenize(e));
                self.tokenizer.recover();
            } else {
                return self.tokenizer.peek().unwrap();
            }
        }
    }

    fn advance(&mut self) -> Token<'a> {
        loop {
            if let Err(e) = self.tokenizer.peek() {
                self.errors.push(CompileErrorKind::Tokenize(e));
                self.tokenizer.recover();
            } else {
                return self.tokenizer.next().unwrap();
            }
        }
    }

    fn consume(&mut self, token_type: TokenType) -> Token<'a> {
        let token = self.advance();
        if token.token_type != token_type {
            self.errors.push(CompileErrorKind::ExpectedToken {
                expected: token_type,
                found: token.token_type,
                line: token.line,
            });
        }
        token
    }

    fn precedence(&mut self, precedence: Precedence) {
        let token = self.peek();
        match token.token_type {
            TokenType::LeftParen => self.grouping(),
            TokenType::Minus | TokenType::Bang => self.unary(),
            TokenType::Number => self.number(),
            TokenType::Nil | TokenType::True | TokenType::False => self.literal(),
            t => {
                self.errors.push(CompileErrorKind::ExpectedOneOf {
                    expected: &[
                        TokenType::LeftParen,
                        TokenType::Minus,
                        TokenType::Number,
                        TokenType::Nil,
                        TokenType::True,
                        TokenType::False,
                    ],
                    found: t,
                    line: token.line,
                });
            }
        }

        let mut token = self.peek();
        while precedence <= get_token_type_inifix_precedence(&token.token_type) {
            match token.token_type {
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
                    self.errors.push(CompileErrorKind::ExpectedOneOf {
                        expected: &[
                            TokenType::Plus,
                            TokenType::Minus,
                            TokenType::Star,
                            TokenType::Slash,
                            TokenType::BangEqual,
                            TokenType::EqualEqual,
                            TokenType::Greater,
                            TokenType::GreaterEqual,
                            TokenType::Less,
                            TokenType::LessEqual,
                        ],
                        found: token.token_type,
                        line: token.line,
                    });
                }
            }
            token = self.peek();
        }
    }

    fn expression(&mut self) {
        self.precedence(Precedence::Assignment);
    }

    fn grouping(&mut self) {
        self.advance();
        self.expression();
        self.consume(TokenType::RightParen);
    }

    fn unary(&mut self) {
        let token = self.advance();
        self.precedence(Precedence::Unary);
        match token.token_type {
            TokenType::Minus => self.chunk.add_bytecode(OpCode::Negate, token.line),
            TokenType::Bang => self.chunk.add_bytecode(OpCode::Not, token.line),
            _ => unreachable!(),
        }
    }

    fn binary(&mut self) {
        let token = self.advance();
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
        let token = self.advance();
        let number = match token.lexeme.parse::<f64>() {
            Ok(number) => number,
            Err(e) => {
                self.errors.push(e.into());
                return;
            }
        };
        self.emit_constant(Value::Number(number), &token);
    }

    fn literal(&mut self) {
        let token = self.advance();
        match token.token_type {
            TokenType::True => self.chunk.add_bytecode(OpCode::True, token.line),
            TokenType::False => self.chunk.add_bytecode(OpCode::False, token.line),
            TokenType::Nil => self.chunk.add_bytecode(OpCode::Nil, token.line),
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
        TokenType::BangEqual | TokenType::EqualEqual => Precedence::Equality,
        TokenType::Greater | TokenType::GreaterEqual | TokenType::Less | TokenType::LessEqual => {
            Precedence::Comparison
        }
        _ => Precedence::None,
    }
}

struct PeekableTokenizer<'a> {
    tokenizer: Tokenizer<'a>,
    peeked: Option<Token<'a>>,
}

impl<'a> PeekableTokenizer<'a> {
    fn new(tokenizer: Tokenizer<'a>) -> Self {
        Self {
            tokenizer,
            peeked: None,
        }
    }

    fn peek(&mut self) -> Result<Token<'a>, TokenizeError<'a>> {
        if self.peeked.is_none() {
            self.peeked = Some(self.tokenizer.token()?);
        }
        Ok(self.peeked.clone().unwrap())
    }

    fn next(&mut self) -> Result<Token<'a>, TokenizeError> {
        if let Some(peeked) = self.peeked.take() {
            Ok(peeked)
        } else {
            self.tokenizer.token()
        }
    }

    fn recover(&mut self) {
        self.tokenizer.recover();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile() {
        let source = "1 + 2 * 3 - 4 / 5";
        let chunk = compile(source).unwrap();
        let expected = vec![
            OpCode::Constant as u8,
            0,
            OpCode::Constant as u8,
            1,
            OpCode::Constant as u8,
            2,
            OpCode::Multiply as u8,
            OpCode::Add as u8,
            OpCode::Constant as u8,
            3,
            OpCode::Constant as u8,
            4,
            OpCode::Divide as u8,
            OpCode::Subtract as u8,
            OpCode::Return as u8,
        ];

        for (i, &bytecode) in expected.iter().enumerate() {
            assert_eq!(bytecode, chunk.get_bytecode(i));
        }
    }
}
