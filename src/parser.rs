use justerror::Error;

use crate::{
    ast::{Expression, InfixOperator, Literal, UnaryOperator},
    tokenizer::{Token, TokenType},
};

#[Error]
pub enum ParseError {
    ExpectedExpression,
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
        _ => Err(ParseError::ExpectedExpression),
    }
}
