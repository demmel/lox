use std::cell::RefCell;

use crate::{
    ast::{Expression, InfixOperator, Literal, Program, Statement, UnaryOperator},
    tokenizer::{Token, TokenType},
};

#[derive(Debug)]
pub struct ParseErrors(Vec<ParseErrorWithContext>);

impl std::error::Error for ParseErrors {}

impl std::fmt::Display for ParseErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Found {} errors during parsing", self.0.len())?;
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
    context: ParseContext,
    pub token: Option<Token>,
}

impl std::fmt::Display for ParseErrorWithContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "While parsing {}",
            self.context.stack.borrow().join(" > ")
        )?;
        write!(f, "{}", self.error)?;
        if let Some(token) = &self.token {
            write!(
                f,
                " at {},{} but found \"{}\"",
                token.span.start_line, token.span.start_column, token.token_type
            )?;
        }
        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("Expected \"{0}\"")]
    Expected(TokenType),
    #[error("Expected one of {0:?}")]
    ExpectedOneOf(Vec<TokenType>),
    #[error("Unexpected \"{0}\"")]
    Unexpected(TokenType),
    #[error("Unexpected identifier")]
    ExpectedIdentifier,
}

#[derive(Debug, Clone)]
struct ParseContext {
    stack: RefCell<Vec<&'static str>>,
}

impl ParseContext {
    fn new() -> Self {
        Self {
            stack: RefCell::new(vec![""]),
        }
    }

    fn push(&self, name: &'static str) -> ParseContextGuard {
        self.stack.borrow_mut().push(name);
        ParseContextGuard::new(self)
    }

    fn pop(&self) {
        self.stack.borrow_mut().pop();
    }
}

struct ParseContextGuard<'a> {
    context: &'a ParseContext,
}

impl<'a> ParseContextGuard<'a> {
    fn new(context: &'a ParseContext) -> Self {
        Self { context }
    }
}

impl<'a> Drop for ParseContextGuard<'a> {
    fn drop(&mut self) {
        self.context.pop();
    }
}

pub fn program(tokens: &[Token]) -> Result<Program, ParseErrors> {
    let context = ParseContext::new();
    let mut statments = Vec::new();
    let mut tokens = tokens;
    let mut errors = Vec::new();

    let _guard = context.push("program");

    while tokens.len() > 1 {
        match declaration(&context, tokens) {
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
            error: ParseError::Expected(TokenType::Eof),
            context: context.clone(),
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

fn declaration<'a>(
    context: &ParseContext,
    tokens: &'a [Token],
) -> Result<(Statement, &'a [Token]), ParseErrors> {
    let _guard = context.push("declaration");
    match tokens.first().map(Token::token_type) {
        Some(TokenType::Var) => Ok(var_declaration(context, &tokens[1..])?),
        Some(TokenType::Fun) => Ok(function(context, &tokens[1..])?),
        _ => statement(context, tokens),
    }
}

fn var_declaration<'a>(
    context: &ParseContext,
    tokens: &'a [Token],
) -> Result<(Statement, &'a [Token]), ParseErrorWithContext> {
    let _guard = context.push("var_declaration");
    let (name, tokens) = match_identifier(context, tokens)?;
    let (expr, tokens) = match tokens.first().map(Token::token_type) {
        Some(TokenType::Equal) => expression(context, &tokens[1..])?,
        _ => (Expression::Literal(Literal::Nil), tokens),
    };
    let tokens = consume(context, tokens, TokenType::Semicolon)?;
    Ok((Statement::VarDeclaration(name, expr), &tokens))
}

fn statement<'a>(
    context: &ParseContext,
    tokens: &'a [Token],
) -> Result<(Statement, &'a [Token]), ParseErrors> {
    let _guard = context.push("statement");
    match tokens.first().map(Token::token_type) {
        Some(TokenType::Print) => Ok(print_statement(context, &tokens[1..])?),
        Some(TokenType::LeftBrace) => block(context, &tokens[1..]),
        Some(TokenType::If) => Ok(if_statement(context, &tokens[1..])?),
        Some(TokenType::While) => Ok(while_statement(context, &tokens[1..])?),
        Some(TokenType::For) => Ok(for_statement(context, &tokens[1..])?),
        _ => Ok(expression_statement(context, tokens)?),
    }
}

fn function<'a>(
    context: &ParseContext,
    tokens: &'a [Token],
) -> Result<(Statement, &'a [Token]), ParseErrors> {
    let _guard = context.push("function");
    let (name, tokens) = match_identifier(context, tokens)?;
    let mut tokens = consume(context, tokens, TokenType::LeftParen)?;
    let mut args = vec![];
    loop {
        if let Ok(rest) = consume(context, tokens, TokenType::RightParen) {
            tokens = rest;
            break;
        }

        let (arg_name, rest) = match_identifier(context, tokens)?;
        args.push(arg_name);
        tokens = rest;

        match tokens.first().map(Token::token_type) {
            Some(TokenType::Comma) => tokens = &tokens[1..],
            Some(TokenType::RightParen) => {
                tokens = &tokens[1..];
                break;
            }
            _ => {
                return Err(ParseErrorWithContext {
                    error: ParseError::ExpectedOneOf(vec![TokenType::Comma, TokenType::RightParen]),
                    context: context.clone(),
                    token: tokens.first().cloned(),
                }
                .into())
            }
        }
    }
    let (body, tokens) = block(context, tokens)?;
    Ok((Statement::Function(name, args, Box::new(body)), tokens))
}

fn while_statement<'a>(
    context: &ParseContext,
    tokens: &'a [Token],
) -> Result<(Statement, &'a [Token]), ParseErrors> {
    let _guard = context.push("while_statement");
    let tokens = consume(context, tokens, TokenType::LeftParen)?;
    let (condition, tokens) = expression(context, tokens)?;
    let tokens = consume(context, tokens, TokenType::RightParen)?;
    let (body, tokens) = statement(context, tokens)?;
    Ok((Statement::While(condition, Box::new(body)), tokens))
}

fn if_statement<'a>(
    context: &ParseContext,
    tokens: &'a [Token],
) -> Result<(Statement, &'a [Token]), ParseErrors> {
    let _guard = context.push("if_statement");
    let tokens = consume(context, tokens, TokenType::LeftParen)?;
    let (condition, tokens) = expression(context, tokens)?;
    let tokens = consume(context, tokens, TokenType::RightParen)?;
    let (then_branch, tokens) = statement(context, tokens)?;
    if let Some(TokenType::Else) = tokens.first().map(Token::token_type) {
        let (else_branch, tokens) = statement(context, &tokens[1..])?;
        Ok((
            Statement::If(
                condition,
                Box::new(then_branch),
                Some(Box::new(else_branch)),
            ),
            tokens,
        ))
    } else {
        Ok((
            Statement::If(condition, Box::new(then_branch), None),
            tokens,
        ))
    }
}

fn for_statement<'a>(
    context: &ParseContext,
    tokens: &'a [Token],
) -> Result<(Statement, &'a [Token]), ParseErrors> {
    let _guard = context.push("for_statement");
    let tokens = consume(context, tokens, TokenType::LeftParen)?;

    let (initializer, tokens) = match tokens.first().map(Token::token_type) {
        Some(TokenType::Semicolon) => (
            Statement::Expression(Expression::Literal(Literal::Nil)),
            &tokens[1..],
        ),
        Some(TokenType::Var) => var_declaration(context, &tokens[1..])?,
        _ => expression_statement(context, tokens)?,
    };

    let (condition, tokens) =
        if tokens.first().map(Token::token_type) != Some(&TokenType::Semicolon) {
            expression(context, tokens)?
        } else {
            (Expression::Literal(Literal::Boolean(true)), tokens)
        };

    let tokens = consume(context, tokens, TokenType::Semicolon)?;

    let (increment, tokens) = match tokens.first().map(Token::token_type) {
        Some(TokenType::RightParen) => (Expression::Literal(Literal::Nil), tokens),
        _ => expression(context, tokens)?,
    };

    let tokens = consume(context, tokens, TokenType::RightParen)?;

    let (statement, tokens) = statement(context, tokens)?;

    Ok((
        Statement::Block(vec![
            initializer,
            Statement::While(
                condition,
                Box::new(Statement::Block(vec![
                    statement,
                    Statement::Expression(increment),
                ])),
            ),
        ]),
        tokens,
    ))
}

fn block<'a>(
    context: &ParseContext,
    tokens: &'a [Token],
) -> Result<(Statement, &'a [Token]), ParseErrors> {
    let _guard = context.push("block");
    let mut statements = Vec::new();
    let mut tokens = tokens;
    let mut errors = Vec::new();

    while let Some(token) = tokens.first() {
        if token.token_type() == &TokenType::RightBrace {
            return Ok((Statement::Block(statements), &tokens[1..]));
        }

        match declaration(context, tokens) {
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
        error: ParseError::Expected(TokenType::RightBrace),
        context: context.clone(),
        token: tokens.first().cloned(),
    });

    Err(ParseErrors(errors))
}

fn expression_statement<'a>(
    context: &ParseContext,
    tokens: &'a [Token],
) -> Result<(Statement, &'a [Token]), ParseErrorWithContext> {
    let _guard = context.push("expression_statement");
    let (expr, tokens) = expression(context, tokens)?;
    let tokens = consume(context, tokens, TokenType::Semicolon)?;
    Ok((Statement::Expression(expr), &tokens))
}

fn print_statement<'a>(
    context: &ParseContext,
    tokens: &'a [Token],
) -> Result<(Statement, &'a [Token]), ParseErrorWithContext> {
    let _guard = context.push("print_statement");
    let (expr, rest) = expression(context, tokens)?;
    let tokens = consume(context, rest, TokenType::Semicolon)?;
    Ok((Statement::Print(expr), &tokens))
}

fn expression<'a>(
    context: &ParseContext,
    tokens: &'a [Token],
) -> Result<(Expression, &'a [Token]), ParseErrorWithContext> {
    let _guard = context.push("expression");
    assignment(context, tokens)
}

fn assignment<'a>(
    context: &ParseContext,
    tokens: &'a [Token],
) -> Result<(Expression, &'a [Token]), ParseErrorWithContext> {
    let _guard = context.push("assignment");
    let (expr, rest) = logical_or(context, tokens)?;

    match rest.first().map(Token::token_type) {
        Some(TokenType::Equal) => match expr {
            Expression::Identifier(name) => {
                let (value, rest) = assignment(context, &rest[1..])?;
                Ok((Expression::Assign(name, Box::new(value)), rest))
            }
            _ => Err(ParseErrorWithContext {
                error: ParseError::ExpectedIdentifier,
                context: context.clone(),
                token: rest.first().cloned(),
            }),
        },
        _ => Ok((expr, rest)),
    }
}

fn binary<'a>(
    context: &ParseContext,
    precedence: impl Fn(
        &ParseContext,
        &'a [Token],
    ) -> Result<(Expression, &'a [Token]), ParseErrorWithContext>,
    operator: impl Fn(&Token) -> Option<InfixOperator>,
    tokens: &'a [Token],
) -> Result<(Expression, &'a [Token]), ParseErrorWithContext> {
    let (mut expr, mut tokens) = precedence(context, tokens)?;

    while let Some(token) = tokens.first() {
        let op = match operator(token) {
            Some(op) => op,
            None => break,
        };
        tokens = &tokens[1..];
        let (right, rest) = precedence(context, tokens)?;
        expr = Expression::Binary(Box::new(expr), op, Box::new(right));
        tokens = rest;
    }

    Ok((expr, tokens))
}

fn logical_or<'a>(
    context: &ParseContext,
    tokens: &'a [Token],
) -> Result<(Expression, &'a [Token]), ParseErrorWithContext> {
    let _guard = context.push("logical_or");
    binary(
        context,
        logical_and,
        |token| match token.token_type() {
            TokenType::Or => Some(InfixOperator::Or),
            _ => None,
        },
        tokens,
    )
}

fn logical_and<'a>(
    context: &ParseContext,
    tokens: &'a [Token],
) -> Result<(Expression, &'a [Token]), ParseErrorWithContext> {
    let _guard = context.push("logical_and");
    binary(
        context,
        equality,
        |token| match token.token_type() {
            TokenType::And => Some(InfixOperator::And),
            _ => None,
        },
        tokens,
    )
}

fn equality<'a>(
    context: &ParseContext,
    tokens: &'a [Token],
) -> Result<(Expression, &'a [Token]), ParseErrorWithContext> {
    let _guard = context.push("equality");
    binary(
        context,
        comparison,
        |token| match token.token_type() {
            TokenType::EqualEqual => Some(InfixOperator::Equal),
            TokenType::BangEqual => Some(InfixOperator::NotEqual),
            _ => None,
        },
        tokens,
    )
}

fn comparison<'a>(
    context: &ParseContext,
    tokens: &'a [Token],
) -> Result<(Expression, &'a [Token]), ParseErrorWithContext> {
    let _guard = context.push("comparison");
    binary(
        context,
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

fn term<'a>(
    context: &ParseContext,
    tokens: &'a [Token],
) -> Result<(Expression, &'a [Token]), ParseErrorWithContext> {
    let _guard = context.push("term");
    binary(
        context,
        factor,
        |token| match token.token_type() {
            TokenType::Plus => Some(InfixOperator::Plus),
            TokenType::Minus => Some(InfixOperator::Minus),
            _ => None,
        },
        tokens,
    )
}

fn factor<'a>(
    context: &ParseContext,
    tokens: &'a [Token],
) -> Result<(Expression, &'a [Token]), ParseErrorWithContext> {
    let _guard = context.push("factor");
    binary(
        context,
        unary,
        |token| match token.token_type() {
            TokenType::Star => Some(InfixOperator::Multiply),
            TokenType::Slash => Some(InfixOperator::Divide),
            _ => None,
        },
        tokens,
    )
}

fn unary<'a>(
    context: &ParseContext,
    tokens: &'a [Token],
) -> Result<(Expression, &'a [Token]), ParseErrorWithContext> {
    let _guard = context.push("unary");

    let operator = match tokens.first().map(Token::token_type) {
        Some(TokenType::Minus) => UnaryOperator::Negate,
        Some(TokenType::Bang) => UnaryOperator::Not,
        _ => return call(context, tokens),
    };

    let (right, rest) = unary(context, &tokens[1..])?;
    Ok((Expression::Unary(operator, Box::new(right)), rest))
}

fn call<'a>(
    context: &ParseContext,
    tokens: &'a [Token],
) -> Result<(Expression, &'a [Token]), ParseErrorWithContext> {
    let _guard = context.push("call");
    let (mut expr, mut tokens) = primary(context, tokens)?;

    if let Expression::Identifier(name) = &expr {
        if let Some(TokenType::LeftParen) = tokens.first().map(Token::token_type) {
            let mut args = Vec::new();
            tokens = &tokens[1..];

            loop {
                if tokens.first().map(Token::token_type) == Some(&TokenType::RightParen) {
                    tokens = &tokens[1..];
                    break;
                }
                let (arg, rest) = expression(context, tokens)?;
                args.push(arg);
                tokens = rest;
                match tokens.first().map(Token::token_type) {
                    Some(TokenType::Comma) => tokens = &tokens[1..],
                    Some(TokenType::RightParen) => {
                        tokens = &tokens[1..];
                        break;
                    }
                    _ => {
                        return Err(ParseErrorWithContext {
                            error: ParseError::ExpectedOneOf(vec![
                                TokenType::Comma,
                                TokenType::RightParen,
                            ]),
                            context: context.clone(),
                            token: tokens.first().cloned(),
                        }
                        .into())
                    }
                }
            }

            expr = Expression::FunctionCall(name.clone(), args);
        }
    }

    Ok((expr, tokens))
}

fn primary<'a>(
    context: &ParseContext,
    tokens: &'a [Token],
) -> Result<(Expression, &'a [Token]), ParseErrorWithContext> {
    let _guard = context.push("primary");
    let Some(token) = tokens.first() else {
        return Err(ParseErrorWithContext {
            error: ParseError::Unexpected(TokenType::Eof),
            context: context.clone(),
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
            let (expr, rest) = expression(context, &tokens[1..])?;
            let tokens = consume(context, rest, TokenType::RightParen)?;
            Ok((Expression::Grouping(Box::new(expr)), tokens))
        }
        TokenType::Identifier(name) => Ok((Expression::Identifier(name.clone()), &tokens[1..])),
        token_type => Err(ParseErrorWithContext {
            error: ParseError::Unexpected(token_type.clone()),
            context: context.clone(),
            token: tokens.first().cloned(),
        }),
    }
}

fn consume<'a>(
    context: &ParseContext,
    tokens: &'a [Token],
    token_type: TokenType,
) -> Result<&'a [Token], ParseErrorWithContext> {
    match tokens.first().map(Token::token_type) {
        Some(t) if t == &token_type => Ok(&tokens[1..]),
        _ => Err(ParseErrorWithContext {
            error: ParseError::Expected(token_type),
            context: context.clone(),
            token: tokens.first().cloned(),
        }),
    }
}

fn match_identifier<'a>(
    context: &ParseContext,
    tokens: &'a [Token],
) -> Result<(String, &'a [Token]), ParseErrorWithContext> {
    match tokens.first().map(Token::token_type) {
        Some(TokenType::Identifier(name)) => Ok((name.clone(), &tokens[1..])),
        _ => Err(ParseErrorWithContext {
            error: ParseError::ExpectedIdentifier,
            context: context.clone(),
            token: tokens.first().cloned(),
        }),
    }
}
