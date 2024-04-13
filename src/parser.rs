use crate::{
    ast::{Expression, InfixOperator, Literal, Program, Statement, UnaryOperator},
    tokenizer::{Token, TokenType},
};

#[derive(Debug)]
pub struct ParseErrors(Vec<ParseErrorWithContext>);

impl std::error::Error for ParseErrors {}

impl std::fmt::Display for ParseErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for error in &self.0 {
            writeln!(f, "{}", error)?;
        }
        Ok(())
    }
}

impl From<ParseErrorWithContext> for ParseErrors {
    fn from(error: ParseErrorWithContext) -> Self {
        ParseErrors(vec![error])
    }
}

#[derive(Debug)]
pub struct ParseErrorWithContext {
    pub error: ParseError,
    pub token: Option<Token>,
}

impl std::fmt::Display for ParseErrorWithContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {:?}", self.error, self.token)
    }
}

#[justerror::Error]
pub enum ParseError {
    ExpectedRightBrace,
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
            Err(mut err) => {
                errors.append(&mut err.0);
                tokens = consume_until_after(tokens, &[TokenType::Semicolon]);
            }
        }
    }

    if tokens.len() == 0 || tokens.len() > 1 || tokens[0].token_type() != &TokenType::Eof {
        errors.push(ParseErrorWithContext {
            error: ParseError::ExpectedEof,
            token: tokens.first().cloned(),
        });
    }

    if !errors.is_empty() {
        return Err(ParseErrors(errors));
    }

    Ok(Program(statments))
}

fn consume_until_after<'a>(tokens: &'a [Token], token_types: &[TokenType]) -> &'a [Token] {
    let mut tokens = tokens;
    while let Some(token) = tokens.first() {
        if token_types.iter().any(|t| t == token.token_type()) {
            return &tokens[1..];
        }
        tokens = &tokens[1..];
    }
    tokens
}

fn declaration(tokens: &[Token]) -> Result<(Statement, &[Token]), ParseErrors> {
    match tokens.first().map(Token::token_type) {
        Some(TokenType::Var) => Ok(var_declaration(&tokens[1..])?),
        _ => statement(tokens),
    }
}

fn var_declaration(tokens: &[Token]) -> Result<(Statement, &[Token]), ParseErrorWithContext> {
    let name = match tokens.first().map(Token::token_type) {
        Some(TokenType::Identifier(name)) => name.clone(),
        _ => {
            return Err(ParseErrorWithContext {
                error: ParseError::ExpectedIdentifier,
                token: tokens.first().cloned(),
            })
        }
    };

    let (expr, rest) = match tokens.get(1).map(Token::token_type) {
        Some(TokenType::Equal) => expression(&tokens[2..])?,
        _ => (Expression::Literal(Literal::Nil), &tokens[1..]),
    };

    let tokens = consume(rest, TokenType::Semicolon, ParseError::ExpectedSemicolon)?;

    Ok((Statement::VarDeclaration(name, expr), &tokens))
}

fn statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ParseErrors> {
    match tokens.first().map(Token::token_type) {
        Some(TokenType::Print) => Ok(print_statement(&tokens[1..])?),
        Some(TokenType::LeftBrace) => block(&tokens[1..]),
        Some(TokenType::If) => Ok(if_statement(&tokens[1..])?),
        Some(TokenType::While) => Ok(while_statement(&tokens[1..])?),
        _ => Ok(expression_statement(tokens)?),
    }
}

fn while_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ParseErrors> {
    let (condition, rest) = expression(tokens)?;
    let (body, rest) = block(rest)?;
    Ok((Statement::While(condition, Box::new(body)), rest))
}

fn if_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ParseErrors> {
    let (condition, rest) = expression(tokens)?;
    let (then_branch, rest) = block(rest)?;
    if let Some(TokenType::Else) = rest.first().map(Token::token_type) {
        let (else_branch, rest) = block(&rest[1..])?;
        Ok((
            Statement::If(
                condition,
                Box::new(then_branch),
                Some(Box::new(else_branch)),
            ),
            rest,
        ))
    } else {
        Ok((Statement::If(condition, Box::new(then_branch), None), rest))
    }
}

fn block(tokens: &[Token]) -> Result<(Statement, &[Token]), ParseErrors> {
    let mut statements = Vec::new();
    let mut tokens = tokens;
    let mut errors = Vec::new();

    while let Some(token) = tokens.first() {
        if token.token_type() == &TokenType::RightBrace {
            return Ok((Statement::Block(statements), &tokens[1..]));
        }

        match declaration(tokens) {
            Ok((stmt, rest)) => {
                statements.push(stmt);
                tokens = rest;
            }
            Err(mut err) => {
                errors.append(&mut err.0);
                tokens =
                    consume_until_after(tokens, &[TokenType::Semicolon, TokenType::RightBrace]);
            }
        }
    }

    errors.push(ParseErrorWithContext {
        error: ParseError::ExpectedRightBrace,
        token: tokens.first().cloned(),
    });

    Err(ParseErrors(errors))
}

fn expression_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ParseErrorWithContext> {
    let (expr, tokens) = expression(tokens)?;
    let tokens = consume(tokens, TokenType::Semicolon, ParseError::ExpectedSemicolon)?;
    Ok((Statement::Expression(expr), &tokens))
}

fn print_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), ParseErrorWithContext> {
    let (expr, rest) = expression(tokens)?;
    let tokens = consume(rest, TokenType::Semicolon, ParseError::ExpectedSemicolon)?;
    Ok((Statement::Print(expr), &tokens))
}

pub fn expression(tokens: &[Token]) -> Result<(Expression, &[Token]), ParseErrorWithContext> {
    assignment(tokens)
}

fn binary(
    precedence: impl Fn(&[Token]) -> Result<(Expression, &[Token]), ParseErrorWithContext>,
    operator: impl Fn(&Token) -> Option<InfixOperator>,
    tokens: &[Token],
) -> Result<(Expression, &[Token]), ParseErrorWithContext> {
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

fn assignment(tokens: &[Token]) -> Result<(Expression, &[Token]), ParseErrorWithContext> {
    let (expr, rest) = logical_or(tokens)?;

    match rest.first().map(Token::token_type) {
        Some(TokenType::Equal) => match expr {
            Expression::Identifier(name) => {
                let (value, rest) = assignment(&rest[1..])?;
                Ok((Expression::Assign(name, Box::new(value)), rest))
            }
            _ => Err(ParseErrorWithContext {
                error: ParseError::ExpectedIdentifier,
                token: rest.first().cloned(),
            }),
        },
        _ => Ok((expr, rest)),
    }
}

fn logical_or(tokens: &[Token]) -> Result<(Expression, &[Token]), ParseErrorWithContext> {
    let (mut left, mut tokens) = logical_and(tokens)?;
    loop {
        match tokens.first().map(Token::token_type) {
            Some(TokenType::Or) => {
                tokens = &tokens[1..];
                let (right, rest) = logical_and(tokens)?;
                left = Expression::Binary(Box::new(left), InfixOperator::Or, Box::new(right));
                tokens = rest;
            }
            _ => break,
        }
    }
    Ok((left, tokens))
}

fn logical_and(tokens: &[Token]) -> Result<(Expression, &[Token]), ParseErrorWithContext> {
    let (mut left, mut tokens) = equality(tokens)?;
    loop {
        match tokens.first().map(Token::token_type) {
            Some(TokenType::And) => {
                tokens = &tokens[1..];
                let (right, rest) = equality(tokens)?;
                left = Expression::Binary(Box::new(left), InfixOperator::And, Box::new(right));
                tokens = rest;
            }
            _ => break,
        }
    }
    Ok((left, tokens))
}

fn equality(tokens: &[Token]) -> Result<(Expression, &[Token]), ParseErrorWithContext> {
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

fn comparison(tokens: &[Token]) -> Result<(Expression, &[Token]), ParseErrorWithContext> {
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

fn term(tokens: &[Token]) -> Result<(Expression, &[Token]), ParseErrorWithContext> {
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

fn factor(tokens: &[Token]) -> Result<(Expression, &[Token]), ParseErrorWithContext> {
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

fn unary(tokens: &[Token]) -> Result<(Expression, &[Token]), ParseErrorWithContext> {
    let operator = match tokens.first().map(Token::token_type) {
        Some(TokenType::Minus) => UnaryOperator::Negate,
        Some(TokenType::Bang) => UnaryOperator::Not,
        _ => return primary(tokens),
    };

    let (right, rest) = unary(&tokens[1..])?;
    Ok((Expression::Unary(operator, Box::new(right)), rest))
}

fn primary(tokens: &[Token]) -> Result<(Expression, &[Token]), ParseErrorWithContext> {
    let Some(token) = tokens.first() else {
        return Err(ParseErrorWithContext {
            error: ParseError::ExpectedExpression,
            token: tokens.first().cloned(),
        });
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
            let tokens = consume(rest, TokenType::RightParen, ParseError::ExpectedRightBrace)?;
            Ok((Expression::Grouping(Box::new(expr)), tokens))
        }
        TokenType::Identifier(name) => Ok((Expression::Identifier(name.clone()), &tokens[1..])),
        _ => Err(ParseErrorWithContext {
            error: ParseError::ExpectedExpression,
            token: tokens.first().cloned(),
        }),
    }
}

fn consume(
    tokens: &[Token],
    token_type: TokenType,
    error: ParseError,
) -> Result<&[Token], ParseErrorWithContext> {
    match tokens.first().map(Token::token_type) {
        Some(t) if t == &token_type => Ok(&tokens[1..]),
        _ => Err(ParseErrorWithContext {
            error,
            token: tokens.first().cloned(),
        }),
    }
}
