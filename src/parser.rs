use justerror::Error;

use crate::{
    ast::{Expression, InfixOperator, Literal, Program, Statement, UnaryOperator},
    tokenizer::{Token, TokenType},
};

#[derive(Debug)]
pub struct ParseErrors(Vec<ParseError>);

impl std::error::Error for ParseErrors {}

impl std::fmt::Display for ParseErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for error in &self.0 {
            writeln!(f, "{}", error)?;
        }
        Ok(())
    }
}

#[Error]
pub enum ParseError {
    ExpectedIdentifier,
    ExpectedExpression,
    ExpectedSemicolon,
    ExpectedEof,
}

pub fn program(tokens: &[Token]) -> Result<Program, ParseErrors> {
    let mut statments = Vec::new();
    let mut tokens = tokens;
    let mut errors = Vec::new();

    while tokens.len() > 1 {
        match declaration(tokens) {
            Ok((stmt, rest)) => {
                statments.push(stmt);
                tokens = rest;
            }
            Err(err) => {
                errors.push(err);
                // Skip to the next statement
                while let Some(token) = tokens.first() {
                    if token.token_type() == &TokenType::Semicolon {
                        tokens = &tokens[1..];
                        break;
                    }
                    tokens = &tokens[1..];
                }
            }
        }
    }

    if tokens.len() != 1 && tokens[0].token_type() != &TokenType::Eof {
        errors.push(ParseError::ExpectedEof);
    }

    if !errors.is_empty() {
        return Err(ParseErrors(errors));
    }

    Ok(Program(statments))
}

fn declaration(tokens: &[Token]) -> Result<(Statement, &[Token]), ParseError> {
    match tokens.first().map(Token::token_type) {
        Some(TokenType::Var) => var_declaration(&tokens[1..]),
        _ => statement(tokens),
    }
}

fn var_declaration(tokens: &[Token]) -> Result<(Statement, &[Token]), ParseError> {
    let name = match tokens.first().map(Token::token_type) {
        Some(TokenType::Identifier(name)) => name.clone(),
        _ => return Err(ParseError::ExpectedIdentifier),
    };

    let (expr, rest) = match tokens.get(1).map(Token::token_type) {
        Some(TokenType::Equal) => expression(&tokens[2..])?,
        _ => (Expression::Literal(Literal::Nil), &tokens[0..]),
    };

    match rest.first().map(Token::token_type) {
        Some(TokenType::Semicolon) => Ok((Statement::VarDeclaration(name, expr), &rest[1..])),
        _ => Err(ParseError::ExpectedSemicolon),
    }
}

fn statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ParseError> {
    match tokens.first().map(Token::token_type) {
        Some(TokenType::Print) => print_statement(&tokens[1..]),
        _ => expression_statement(tokens),
    }
}

fn expression_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ParseError> {
    let (expr, rest) = expression(tokens)?;
    match rest.first().map(Token::token_type) {
        Some(TokenType::Semicolon) => Ok((Statement::Expression(expr), &rest[1..])),
        _ => Err(ParseError::ExpectedSemicolon),
    }
}

fn print_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ParseError> {
    let (expr, rest) = expression(tokens)?;
    match rest.first().map(Token::token_type) {
        Some(TokenType::Semicolon) => Ok((Statement::Print(expr), &rest[1..])),
        _ => Err(ParseError::ExpectedSemicolon),
    }
}

pub fn expression(tokens: &[Token]) -> Result<(Expression, &[Token]), ParseError> {
    equality(tokens)
}

fn binary(
    precedence: impl Fn(&[Token]) -> Result<(Expression, &[Token]), ParseError>,
    operator: impl Fn(&Token) -> Option<InfixOperator>,
    tokens: &[Token],
) -> Result<(Expression, &[Token]), ParseError> {
    let (mut expr, mut tokens) = precedence(tokens)?;

    while let Some(token) = tokens.first() {
        let op = match operator(token) {
            Some(op) => op,
            None => break,
        };
        tokens = &tokens[1..];
        let (right, rest) = precedence(tokens)?;
        expr = Expression::Binary(Box::new(expr), op, Box::new(right));
        tokens = rest;
    }

    Ok((expr, tokens))
}

fn equality(tokens: &[Token]) -> Result<(Expression, &[Token]), ParseError> {
    binary(
        comparison,
        |token| match token.token_type() {
            TokenType::EqualEqual => Some(InfixOperator::Equal),
            TokenType::BangEqual => Some(InfixOperator::NotEqual),
            _ => None,
        },
        tokens,
    )
}

fn comparison(tokens: &[Token]) -> Result<(Expression, &[Token]), ParseError> {
    binary(
        term,
        |token| match token.token_type() {
            TokenType::Less => Some(InfixOperator::LessThan),
            TokenType::LessEqual => Some(InfixOperator::LessThanOrEqual),
            TokenType::Greater => Some(InfixOperator::GreaterThan),
            TokenType::GreaterEqual => Some(InfixOperator::GreaterThanOrEqual),
            _ => None,
        },
        tokens,
    )
}

fn term(tokens: &[Token]) -> Result<(Expression, &[Token]), ParseError> {
    binary(
        factor,
        |token| match token.token_type() {
            TokenType::Plus => Some(InfixOperator::Plus),
            TokenType::Minus => Some(InfixOperator::Minus),
            _ => None,
        },
        tokens,
    )
}

fn factor(tokens: &[Token]) -> Result<(Expression, &[Token]), ParseError> {
    binary(
        unary,
        |token| match token.token_type() {
            TokenType::Star => Some(InfixOperator::Multiply),
            TokenType::Slash => Some(InfixOperator::Divide),
            _ => None,
        },
        tokens,
    )
}

fn unary(tokens: &[Token]) -> Result<(Expression, &[Token]), ParseError> {
    let operator = match tokens.first().map(Token::token_type) {
        Some(TokenType::Minus) => UnaryOperator::Negate,
        Some(TokenType::Bang) => UnaryOperator::Not,
        _ => return primary(tokens),
    };

    let (right, rest) = unary(&tokens[1..])?;
    Ok((Expression::Unary(operator, Box::new(right)), rest))
}

fn primary(tokens: &[Token]) -> Result<(Expression, &[Token]), ParseError> {
    let Some(token) = tokens.first() else {
        return Err(ParseError::ExpectedExpression);
    };

    match token.token_type() {
        TokenType::Number(n) => Ok((Expression::Literal(Literal::Number(*n)), &tokens[1..])),
        TokenType::String(s) => Ok((
            Expression::Literal(Literal::String(s.clone())),
            &tokens[1..],
        )),
        TokenType::True => Ok((Expression::Literal(Literal::Boolean(true)), &tokens[1..])),
        TokenType::False => Ok((Expression::Literal(Literal::Boolean(false)), &tokens[1..])),
        TokenType::Nil => Ok((Expression::Literal(Literal::Nil), &tokens[1..])),
        TokenType::LeftParen => {
            let (expr, rest) = expression(&tokens[1..])?;
            match rest.first().map(Token::token_type) {
                Some(TokenType::RightParen) => {
                    Ok((Expression::Grouping(Box::new(expr)), &rest[1..]))
                }
                _ => Err(ParseError::ExpectedExpression),
            }
        }
        TokenType::Identifier(name) => Ok((Expression::Identifier(name.clone()), &tokens[1..])),
        _ => Err(ParseError::ExpectedExpression),
    }
}
