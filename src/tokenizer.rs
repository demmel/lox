use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier(String),
    String(String),
    Number(f64),

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    // End of file
    Eof,
}

impl Eq for TokenType {}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let repr = match self {
            TokenType::LeftParen => "(",
            TokenType::RightParen => ")",
            TokenType::LeftBrace => "{",
            TokenType::RightBrace => "}",
            TokenType::Comma => ",",
            TokenType::Dot => ".",
            TokenType::Minus => "-",
            TokenType::Plus => "+",
            TokenType::Semicolon => ";",
            TokenType::Slash => "/",
            TokenType::Star => "*",
            TokenType::Bang => "!",
            TokenType::BangEqual => "!=",
            TokenType::Equal => "=",
            TokenType::EqualEqual => "==",
            TokenType::Greater => ">",
            TokenType::GreaterEqual => ">=",
            TokenType::Less => "<",
            TokenType::LessEqual => "<=",
            TokenType::Identifier(id) => return write!(f, "{id}"),
            TokenType::String(s) => return write!(f, "\'{s}\""),
            TokenType::Number(n) => return write!(f, "{n}"),
            TokenType::And => "and",
            TokenType::Class => "class",
            TokenType::Else => "else",
            TokenType::False => "false",
            TokenType::Fun => "fun",
            TokenType::For => "for",
            TokenType::If => "if",
            TokenType::Nil => "nil",
            TokenType::Or => "or",
            TokenType::Print => "print",
            TokenType::Return => "return",
            TokenType::Super => "super",
            TokenType::This => "this",
            TokenType::True => "true",
            TokenType::Var => "var",
            TokenType::While => "while",
            TokenType::Eof => "",
        };
        write!(f, "{repr}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub token_type: TokenType,
    pub span: Span,
}

impl Token {
    pub fn token_type(&self) -> &TokenType {
        &self.token_type
    }
}

#[justerror::Error]
pub enum TokenizeError {
    #[error(desc = "Unexpected character {0} at line {1}, column {2}")]
    UnexpectedCharacter(char, usize, usize),
}

#[derive(Debug, Clone)]
struct TokenizerState<'a> {
    remaining: &'a str,
    line: usize,
    column: usize,
}

pub fn tokens(source: &str) -> Result<Vec<Token>, TokenizeError> {
    let mut tokens = Vec::new();
    let mut state = TokenizerState {
        remaining: source,
        line: 1,
        column: 1,
    };

    loop {
        let (token, next_state) = token(state)?;
        state = next_state;
        if matches!(token.token_type, TokenType::Eof) {
            tokens.push(token);
            break;
        }
        tokens.push(token);
    }

    Ok(tokens)
}

fn token(mut state: TokenizerState) -> Result<(Token, TokenizerState), TokenizeError> {
    while let Some((_, next_state)) = maximal(&[whitespace, comment], state.clone()) {
        state = next_state;
    }

    if state.remaining.is_empty() {
        return Ok((
            Token {
                token_type: TokenType::Eof,
                span: Span {
                    start_line: state.line,
                    start_column: state.column,
                    end_line: state.line,
                    end_column: state.column,
                },
            },
            state,
        ));
    }

    maximal(
        &[
            // Single-character tokens
            left_paren,
            right_paren,
            left_brace,
            right_brace,
            comma,
            dot,
            minus,
            plus,
            semicolon,
            slash,
            star,
            // one or two character tokens
            bang,
            bang_equal,
            equal,
            equal_equal,
            greater,
            greater_equal,
            less,
            less_equal,
            // keywords
            and,
            class,
            else_,
            false_,
            fun,
            for_,
            if_,
            nil,
            or,
            print_,
            return_,
            super_,
            this,
            true_,
            var,
            while_,
            // literals
            identifier,
            string,
            number,
        ],
        state.clone(),
    )
    .ok_or(TokenizeError::UnexpectedCharacter(
        state.remaining.chars().next().unwrap(),
        state.line,
        state.column,
    ))
}

fn maximal<'a, T: std::fmt::Debug>(
    parsers: &[fn(TokenizerState) -> Option<(T, TokenizerState)>],
    state: TokenizerState<'a>,
) -> Option<(T, TokenizerState<'a>)> {
    let mut min_left = state.remaining.len() + 1;
    let mut max_match = None;

    let matching_parsers = parsers.iter().filter_map(|parser| parser(state.clone()));
    for (m, next_state) in matching_parsers {
        let left = next_state.remaining.len();
        if left < min_left {
            min_left = left;
            max_match = Some((m, next_state));
        }
    }

    max_match
}

fn whitespace(mut state: TokenizerState) -> Option<((), TokenizerState)> {
    let mut chars = state.remaining.chars();
    let mut len = 0;
    while let Some(c) = chars.next() {
        if c.is_whitespace() {
            len += c.len_utf8();
            state.column += 1;
            if c == '\n' {
                state.line += 1;
                state.column = 1;
            }
        } else {
            break;
        }
    }
    if len > 0 {
        Some((
            (),
            TokenizerState {
                remaining: &state.remaining[len..],
                ..state
            },
        ))
    } else {
        None
    }
}

fn comment(mut state: TokenizerState) -> Option<((), TokenizerState)> {
    if state.remaining.starts_with("//") {
        let mut chars = state.remaining.chars();
        while let Some(c) = chars.next() {
            if c == '\n' {
                return Some((
                    (),
                    TokenizerState {
                        remaining: chars.as_str(),
                        line: state.line + 1,
                        column: 1,
                    },
                ));
            } else {
                state.column += 1;
            }
        }
        return Some((
            (),
            TokenizerState {
                remaining: chars.as_str(),
                ..state
            },
        ));
    } else {
        None
    }
}

fn literal<'a>(
    state: TokenizerState<'a>,
    literal: &str,
    token_type: TokenType,
) -> Option<(Token, TokenizerState<'a>)> {
    if state.remaining.starts_with(literal) {
        let count = literal.chars().count();
        Some((
            Token {
                token_type,
                span: Span {
                    start_line: state.line,
                    start_column: state.column,
                    end_line: state.line,
                    end_column: state.column + count,
                },
            },
            TokenizerState {
                remaining: &state.remaining[literal.len()..],
                column: state.column + count,
                ..state
            },
        ))
    } else {
        None
    }
}

macro_rules! literal {
    ($name:ident, $literal:expr, $token_type:expr) => {
        fn $name(state: TokenizerState) -> Option<(Token, TokenizerState)> {
            literal(state, $literal, $token_type)
        }
    };
}

literal! {left_paren, "(", TokenType::LeftParen}
literal! {right_paren, ")", TokenType::RightParen}
literal! {left_brace, "{", TokenType::LeftBrace}
literal! {right_brace, "}", TokenType::RightBrace}
literal! {comma, ",", TokenType::Comma}
literal! {dot, ".", TokenType::Dot}
literal! {minus, "-", TokenType::Minus}
literal! {plus, "+", TokenType::Plus}
literal! {semicolon, ";", TokenType::Semicolon}
literal! {slash, "/", TokenType::Slash}
literal! {star, "*", TokenType::Star}
literal! {bang, "!", TokenType::Bang}
literal! {bang_equal, "!=", TokenType::BangEqual}
literal! {equal, "=", TokenType::Equal}
literal! {equal_equal, "==", TokenType::EqualEqual}
literal! {greater, ">", TokenType::Greater}
literal! {greater_equal, ">=", TokenType::GreaterEqual}
literal! {less, "<", TokenType::Less}
literal! {less_equal, "<=", TokenType::LessEqual}
literal! {and, "and", TokenType::And}
literal! {class, "class", TokenType::Class}
literal! {else_, "else", TokenType::Else}
literal! {false_, "false", TokenType::False}
literal! {fun, "fun", TokenType::Fun}
literal! {for_, "for", TokenType::For}
literal! {if_, "if", TokenType::If}
literal! {nil, "nil", TokenType::Nil}
literal! {or, "or", TokenType::Or}
literal! {print_, "print", TokenType::Print}
literal! {return_, "return", TokenType::Return}
literal! {super_, "super", TokenType::Super}
literal! {this, "this", TokenType::This}
literal! {true_, "true", TokenType::True}
literal! {var, "var", TokenType::Var}
literal! {while_, "while", TokenType::While}

fn identifier(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    let mut chars = state.remaining.chars();

    let first = chars.next()?;
    if !first.is_ascii_alphabetic() && first != '_' {
        return None;
    }

    let take_alphas = chars.take_while(|c| c.is_ascii_alphanumeric() || *c == '_');

    let len = first.len_utf8() + take_alphas.clone().map(char::len_utf8).sum::<usize>();
    let count = 1 + take_alphas.count();

    if len > 0 {
        Some((
            Token {
                token_type: TokenType::Identifier(state.remaining[..len].to_string()),
                span: Span {
                    start_line: state.line,
                    start_column: state.column,
                    end_line: state.line,
                    end_column: state.column + count,
                },
            },
            TokenizerState {
                remaining: &state.remaining[len..],
                column: state.column + count,
                ..state
            },
        ))
    } else {
        None
    }
}

fn string(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    if !state.remaining.starts_with('"') {
        return None;
    }

    let mut chars = state.remaining.chars().skip(1);
    let mut len = 1;
    let mut count = 1;
    while let Some(c) = chars.next() {
        len += c.len_utf8();
        count += 1;
        match c {
            '"' => {
                return Some((
                    Token {
                        token_type: TokenType::String(state.remaining[1..len - 1].to_string()),
                        span: Span {
                            start_line: state.line,
                            start_column: state.column,
                            end_line: state.line,
                            end_column: state.column + count,
                        },
                    },
                    TokenizerState {
                        remaining: &state.remaining[len..],
                        column: state.column + count,
                        ..state
                    },
                ))
            }
            _ => {}
        }
    }
    None
}

fn number(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    let mut chars = state.remaining.chars().peekable();
    let first = chars.next()?;
    if !first.is_ascii_digit() && first != '.' {
        return None;
    }

    let mut len = first.len_utf8();
    let mut count = 1;

    let mut phase = 0;
    if first == '.' {
        phase = 1;
    }

    while let Some(&c) = chars.peek() {
        match phase {
            0 => {
                if c == '.' {
                    phase = 1;
                } else if !c.is_ascii_digit() {
                    break;
                }
                chars.next();
                len += c.len_utf8();
                count += 1;
            }
            1 => {
                if !c.is_ascii_digit() {
                    break;
                }
                chars.next();
                len += c.len_utf8();
                count += 1;
            }
            _ => unreachable!(),
        }
    }

    Some((
        Token {
            token_type: TokenType::Number(state.remaining[..len].parse().unwrap()),
            span: Span {
                start_line: state.line,
                start_column: state.column,
                end_line: state.line,
                end_column: state.column + count,
            },
        },
        TokenizerState {
            remaining: &state.remaining[len..],
            column: state.column + count,
            ..state
        },
    ))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_tokens() {
        let source = "var x = 1;";
        let expected = vec![
            Token {
                token_type: TokenType::Var,
                span: Span {
                    start_line: 1,
                    start_column: 1,
                    end_line: 1,
                    end_column: 4,
                },
            },
            Token {
                token_type: TokenType::Identifier("x".to_string()),
                span: Span {
                    start_line: 1,
                    start_column: 5,
                    end_line: 1,
                    end_column: 6,
                },
            },
            Token {
                token_type: TokenType::Equal,
                span: Span {
                    start_line: 1,
                    start_column: 7,
                    end_line: 1,
                    end_column: 8,
                },
            },
            Token {
                token_type: TokenType::Number(1.0),
                span: Span {
                    start_line: 1,
                    start_column: 9,
                    end_line: 1,
                    end_column: 10,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                span: Span {
                    start_line: 1,
                    start_column: 10,
                    end_line: 1,
                    end_column: 11,
                },
            },
            Token {
                token_type: TokenType::Eof,
                span: Span {
                    start_line: 1,
                    start_column: 11,
                    end_line: 1,
                    end_column: 11,
                },
            },
        ];
        assert_eq!(tokens(source).unwrap(), expected);
    }

    #[test]
    fn test_tokens_with_comments() {
        let source = "var x = 1; // comment";
        let expected = vec![
            Token {
                token_type: TokenType::Var,
                span: Span {
                    start_line: 1,
                    start_column: 1,
                    end_line: 1,
                    end_column: 4,
                },
            },
            Token {
                token_type: TokenType::Identifier("x".to_string()),
                span: Span {
                    start_line: 1,
                    start_column: 5,
                    end_line: 1,
                    end_column: 6,
                },
            },
            Token {
                token_type: TokenType::Equal,
                span: Span {
                    start_line: 1,
                    start_column: 7,
                    end_line: 1,
                    end_column: 8,
                },
            },
            Token {
                token_type: TokenType::Number(1.0),
                span: Span {
                    start_line: 1,
                    start_column: 9,
                    end_line: 1,
                    end_column: 10,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                span: Span {
                    start_line: 1,
                    start_column: 10,
                    end_line: 1,
                    end_column: 11,
                },
            },
            Token {
                token_type: TokenType::Eof,
                span: Span {
                    start_line: 1,
                    start_column: 22,
                    end_line: 1,
                    end_column: 22,
                },
            },
        ];
        assert_eq!(tokens(source).unwrap(), expected);
    }

    #[test]
    fn test_tokens_with_whitespace() {
        let source = "var x = 1; ";
        let expected = vec![
            Token {
                token_type: TokenType::Var,
                span: Span {
                    start_line: 1,
                    start_column: 1,
                    end_line: 1,
                    end_column: 4,
                },
            },
            Token {
                token_type: TokenType::Identifier("x".to_string()),
                span: Span {
                    start_line: 1,
                    start_column: 5,
                    end_line: 1,
                    end_column: 6,
                },
            },
            Token {
                token_type: TokenType::Equal,
                span: Span {
                    start_line: 1,
                    start_column: 7,
                    end_line: 1,
                    end_column: 8,
                },
            },
            Token {
                token_type: TokenType::Number(1.0),
                span: Span {
                    start_line: 1,
                    start_column: 9,
                    end_line: 1,
                    end_column: 10,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                span: Span {
                    start_line: 1,
                    start_column: 10,
                    end_line: 1,
                    end_column: 11,
                },
            },
            Token {
                token_type: TokenType::Eof,
                span: Span {
                    start_line: 1,
                    start_column: 12,
                    end_line: 1,
                    end_column: 12,
                },
            },
        ];
        assert_eq!(tokens(source).unwrap(), expected);
    }

    #[test]
    fn test_tokens_with_string() {
        let source = "var x = \"hello\";";
        let expected = vec![
            Token {
                token_type: TokenType::Var,
                span: Span {
                    start_line: 1,
                    start_column: 1,
                    end_line: 1,
                    end_column: 4,
                },
            },
            Token {
                token_type: TokenType::Identifier("x".to_string()),
                span: Span {
                    start_line: 1,
                    start_column: 5,
                    end_line: 1,
                    end_column: 6,
                },
            },
            Token {
                token_type: TokenType::Equal,
                span: Span {
                    start_line: 1,
                    start_column: 7,
                    end_line: 1,
                    end_column: 8,
                },
            },
            Token {
                token_type: TokenType::String("hello".to_string()),
                span: Span {
                    start_line: 1,
                    start_column: 9,
                    end_line: 1,
                    end_column: 16,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                span: Span {
                    start_line: 1,
                    start_column: 16,
                    end_line: 1,
                    end_column: 17,
                },
            },
            Token {
                token_type: TokenType::Eof,
                span: Span {
                    start_line: 1,
                    start_column: 17,
                    end_line: 1,
                    end_column: 17,
                },
            },
        ];
        assert_eq!(tokens(source).unwrap(), expected);
    }

    #[test]
    fn test_tokens_with_number() {
        let source = "var x = 1.0;";
        let expected = vec![
            Token {
                token_type: TokenType::Var,
                span: Span {
                    start_line: 1,
                    start_column: 1,
                    end_line: 1,
                    end_column: 4,
                },
            },
            Token {
                token_type: TokenType::Identifier("x".to_string()),
                span: Span {
                    start_line: 1,
                    start_column: 5,
                    end_line: 1,
                    end_column: 6,
                },
            },
            Token {
                token_type: TokenType::Equal,
                span: Span {
                    start_line: 1,
                    start_column: 7,
                    end_line: 1,
                    end_column: 8,
                },
            },
            Token {
                token_type: TokenType::Number(1.0),
                span: Span {
                    start_line: 1,
                    start_column: 9,
                    end_line: 1,
                    end_column: 12,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                span: Span {
                    start_line: 1,
                    start_column: 12,
                    end_line: 1,
                    end_column: 13,
                },
            },
            Token {
                token_type: TokenType::Eof,
                span: Span {
                    start_line: 1,
                    start_column: 13,
                    end_line: 1,
                    end_column: 13,
                },
            },
        ];
        assert_eq!(tokens(source).unwrap(), expected);
    }

    #[test]
    fn test_tokens_with_keywords() {
        let source = "var x = true;";
        let expected = vec![
            Token {
                token_type: TokenType::Var,
                span: Span {
                    start_line: 1,
                    start_column: 1,
                    end_line: 1,
                    end_column: 4,
                },
            },
            Token {
                token_type: TokenType::Identifier("x".to_string()),
                span: Span {
                    start_line: 1,
                    start_column: 5,
                    end_line: 1,
                    end_column: 6,
                },
            },
            Token {
                token_type: TokenType::Equal,
                span: Span {
                    start_line: 1,
                    start_column: 7,
                    end_line: 1,
                    end_column: 8,
                },
            },
            Token {
                token_type: TokenType::True,
                span: Span {
                    start_line: 1,
                    start_column: 9,
                    end_line: 1,
                    end_column: 13,
                },
            },
            Token {
                token_type: TokenType::Semicolon,
                span: Span {
                    start_line: 1,
                    start_column: 13,
                    end_line: 1,
                    end_column: 14,
                },
            },
            Token {
                token_type: TokenType::Eof,
                span: Span {
                    start_line: 1,
                    start_column: 14,
                    end_line: 1,
                    end_column: 14,
                },
            },
        ];
        assert_eq!(tokens(source).unwrap(), expected);
    }

    #[test]
    fn test_double_equal() {
        let source = "a==b";
        let expected = vec![
            Token {
                token_type: TokenType::Identifier("a".to_string()),
                span: Span {
                    start_line: 1,
                    start_column: 1,
                    end_line: 1,
                    end_column: 2,
                },
            },
            Token {
                token_type: TokenType::EqualEqual,
                span: Span {
                    start_line: 1,
                    start_column: 2,
                    end_line: 1,
                    end_column: 4,
                },
            },
            Token {
                token_type: TokenType::Identifier("b".to_string()),
                span: Span {
                    start_line: 1,
                    start_column: 4,
                    end_line: 1,
                    end_column: 5,
                },
            },
            Token {
                token_type: TokenType::Eof,
                span: Span {
                    start_line: 1,
                    start_column: 5,
                    end_line: 1,
                    end_column: 5,
                },
            },
        ];
        assert_eq!(tokens(source).unwrap(), expected);
    }

    #[test]
    fn test_invalid_token() {
        let source = "a@b";
        assert!(matches!(
            tokens(source),
            Err(TokenizeError::UnexpectedCharacter('@', 1, 2))
        ));
    }
}
