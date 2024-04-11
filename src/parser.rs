use std::fmt::Display;

use justerror::Error;

use crate::tokenizer::Token;

#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
    Grouping(Box<Expression>),
    Binary(Box<Expression>, InfixOperator, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
}

#[derive(Debug)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Negate,
    Not,
}

#[derive(Debug)]
pub enum InfixOperator {
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Plus,
    Minus,
    Multiply,
    Divide,
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Literal(literal) => write!(f, "{}", literal),
            Expression::Grouping(expr) => write!(f, "({})", expr),
            Expression::Binary(left, op, right) => write!(f, "({} {} {})", op, left, right),
            Expression::Unary(op, right) => write!(f, "({} {})", op, right),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(n) => write!(f, "{}", n),
            Literal::String(s) => write!(f, "{}", s),
            Literal::Boolean(b) => write!(f, "{}", b),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

impl Display for InfixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InfixOperator::Equal => write!(f, "=="),
            InfixOperator::NotEqual => write!(f, "!="),
            InfixOperator::LessThan => write!(f, "<"),
            InfixOperator::LessThanOrEqual => write!(f, "<="),
            InfixOperator::GreaterThan => write!(f, ">"),
            InfixOperator::GreaterThanOrEqual => write!(f, ">="),
            InfixOperator::Plus => write!(f, "+"),
            InfixOperator::Minus => write!(f, "-"),
            InfixOperator::Multiply => write!(f, "*"),
            InfixOperator::Divide => write!(f, "/"),
        }
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::Negate => write!(f, "-"),
            UnaryOperator::Not => write!(f, "!"),
        }
    }
}

#[Error]
pub enum ParseError {
    UnexpectedToken,
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
        |token| match token {
            Token::EqualEqual => Some(InfixOperator::Equal),
            Token::BangEqual => Some(InfixOperator::NotEqual),
            _ => None,
        },
        tokens,
    )
}

fn comparison(tokens: &[Token]) -> Result<(Expression, &[Token]), ParseError> {
    binary(
        term,
        |token| match token {
            Token::Less => Some(InfixOperator::LessThan),
            Token::LessEqual => Some(InfixOperator::LessThanOrEqual),
            Token::Greater => Some(InfixOperator::GreaterThan),
            Token::GreaterEqual => Some(InfixOperator::GreaterThanOrEqual),
            _ => None,
        },
        tokens,
    )
}

fn term(tokens: &[Token]) -> Result<(Expression, &[Token]), ParseError> {
    binary(
        factor,
        |token| match token {
            Token::Plus => Some(InfixOperator::Plus),
            Token::Minus => Some(InfixOperator::Minus),
            _ => None,
        },
        tokens,
    )
}

fn factor(tokens: &[Token]) -> Result<(Expression, &[Token]), ParseError> {
    binary(
        unary,
        |token| match token {
            Token::Star => Some(InfixOperator::Multiply),
            Token::Slash => Some(InfixOperator::Divide),
            _ => None,
        },
        tokens,
    )
}

fn unary(tokens: &[Token]) -> Result<(Expression, &[Token]), ParseError> {
    let operator = match tokens.first() {
        Some(Token::Minus) => UnaryOperator::Negate,
        Some(Token::Bang) => UnaryOperator::Not,
        _ => return primary(tokens),
    };

    let (right, rest) = unary(&tokens[1..])?;
    Ok((Expression::Unary(operator, Box::new(right)), rest))
}

fn primary(tokens: &[Token]) -> Result<(Expression, &[Token]), ParseError> {
    let Some(token) = tokens.first() else {
        return Err(ParseError::UnexpectedToken);
    };

    match token {
        Token::Number(n) => Ok((Expression::Literal(Literal::Number(*n)), &tokens[1..])),
        Token::String(s) => Ok((
            Expression::Literal(Literal::String(s.clone())),
            &tokens[1..],
        )),
        Token::True => Ok((Expression::Literal(Literal::Boolean(true)), &tokens[1..])),
        Token::False => Ok((Expression::Literal(Literal::Boolean(false)), &tokens[1..])),
        Token::Nil => Ok((Expression::Literal(Literal::Nil), &tokens[1..])),
        Token::LeftParen => {
            let (expr, rest) = expression(&tokens[1..])?;
            match rest.first() {
                Some(Token::RightParen) => Ok((Expression::Grouping(Box::new(expr)), &rest[1..])),
                _ => Err(ParseError::UnexpectedToken),
            }
        }
        _ => Err(ParseError::UnexpectedToken),
    }
}
