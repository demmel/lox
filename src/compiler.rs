use thiserror::Error;

use crate::{
    bytecode::Chunk,
    tokenizer::{TokenType, TokenizeError, Tokenizer},
};

#[derive(Debug, Error)]
pub enum CompileError<'a> {
    #[error("Failed to tokenize")]
    Tokenize { errors: Vec<TokenizeError<'a>> },
}

pub fn compile(source: &str) -> Result<Chunk, CompileError> {
    let mut chunk = Chunk::new();
    let mut tokenizer = Tokenizer::new(source);
    let mut tokenizer_errors = Vec::new();

    loop {
        match tokenizer.token() {
            Ok(token) => {
                if token.token_type == TokenType::Eof {
                    break;
                }
            }
            Err(e) => {
                tokenizer_errors.push(e);
                tokenizer.recover();
            }
        }
    }

    if !tokenizer_errors.is_empty() {
        return Err(CompileError::Tokenize {
            errors: tokenizer_errors,
        });
    }

    Ok(chunk)
}
