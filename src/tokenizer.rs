use justerror::Error;

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

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    token_type: TokenType,
    line: usize,
    column: usize,
}

impl Token {
    pub fn token_type(&self) -> &TokenType {
        &self.token_type
    }
}

#[Error]
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
                line: state.line,
                column: state.column,
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
        Some((
            Token {
                token_type,
                line: state.line,
                column: state.column,
            },
            TokenizerState {
                remaining: &state.remaining[literal.len()..],
                column: state.column + literal.len(),
                ..state
            },
        ))
    } else {
        None
    }
}

fn left_paren(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "(", TokenType::LeftParen)
}

fn right_paren(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, ")", TokenType::RightParen)
}

fn left_brace(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "{", TokenType::LeftBrace)
}

fn right_brace(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "}", TokenType::RightBrace)
}

fn comma(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, ",", TokenType::Comma)
}

fn dot(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, ".", TokenType::Dot)
}

fn minus(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "-", TokenType::Minus)
}

fn plus(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "+", TokenType::Plus)
}

fn semicolon(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, ";", TokenType::Semicolon)
}

fn slash(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "/", TokenType::Slash)
}

fn star(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "*", TokenType::Star)
}

fn bang(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "!", TokenType::Bang)
}

fn bang_equal(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "!=", TokenType::BangEqual)
}

fn equal(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "=", TokenType::Equal)
}

fn equal_equal(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "==", TokenType::EqualEqual)
}

fn greater(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, ">", TokenType::Greater)
}

fn greater_equal(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, ">=", TokenType::GreaterEqual)
}

fn less(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "<", TokenType::Less)
}

fn less_equal(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "<=", TokenType::LessEqual)
}

fn and(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "and", TokenType::And)
}

fn class(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "class", TokenType::Class)
}

fn else_(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "else", TokenType::Else)
}

fn false_(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "false", TokenType::False)
}

fn fun(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "fun", TokenType::Fun)
}

fn for_(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "for", TokenType::For)
}

fn if_(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "if", TokenType::If)
}

fn nil(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "nil", TokenType::Nil)
}

fn or(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "or", TokenType::Or)
}

fn print_(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "print", TokenType::Print)
}

fn return_(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "return", TokenType::Return)
}

fn super_(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "super", TokenType::Super)
}

fn this(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "this", TokenType::This)
}

fn true_(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "true", TokenType::True)
}

fn var(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "var", TokenType::Var)
}

fn while_(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    literal(state, "while", TokenType::While)
}

fn identifier(state: TokenizerState) -> Option<(Token, TokenizerState)> {
    let mut chars = state.remaining.chars();

    let first = chars.next()?;
    if !first.is_ascii_alphabetic() && first != '_' {
        return None;
    }

    let len = first.len_utf8()
        + chars
            .take_while(|c| c.is_ascii_alphanumeric() || *c == '_')
            .map(char::len_utf8)
            .sum::<usize>();

    if len > 0 {
        Some((
            Token {
                token_type: TokenType::Identifier(state.remaining[..len].to_string()),
                line: state.line,
                column: state.column,
            },
            TokenizerState {
                remaining: &state.remaining[len..],
                column: state.column + len,
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
    while let Some(c) = chars.next() {
        len += c.len_utf8();
        match c {
            '"' => {
                return Some((
                    Token {
                        token_type: TokenType::String(state.remaining[1..len - 1].to_string()),
                        line: state.line,
                        column: state.column,
                    },
                    TokenizerState {
                        remaining: &state.remaining[len..],
                        column: state.column + len,
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
            }
            1 => {
                if !c.is_ascii_digit() {
                    break;
                }
                chars.next();
                len += c.len_utf8();
            }
            _ => unreachable!(),
        }
    }

    Some((
        Token {
            token_type: TokenType::Number(state.remaining[..len].parse().unwrap()),
            line: state.line,
            column: state.column,
        },
        TokenizerState {
            remaining: &state.remaining[len..],
            column: state.column + len,
            ..state
        },
    ))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_literal() {
        let source = "var";
        let expected = vec![
            Token {
                token_type: TokenType::Var,
                line: 1,
                column: 1,
            },
            Token {
                token_type: TokenType::Eof,
                line: 1,
                column: 4,
            },
        ];
        assert_eq!(tokens(source).unwrap(), expected);
    }

    #[test]
    fn test_tokens() {
        let source = "var x = 1;";
        let expected = vec![
            Token {
                token_type: TokenType::Var,
                line: 1,
                column: 1,
            },
            Token {
                token_type: TokenType::Identifier("x".to_string()),
                line: 1,
                column: 5,
            },
            Token {
                token_type: TokenType::Equal,
                line: 1,
                column: 7,
            },
            Token {
                token_type: TokenType::Number(1.0),
                line: 1,
                column: 9,
            },
            Token {
                token_type: TokenType::Semicolon,
                line: 1,
                column: 10,
            },
            Token {
                token_type: TokenType::Eof,
                line: 1,
                column: 11,
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
                line: 1,
                column: 1,
            },
            Token {
                token_type: TokenType::Identifier("x".to_string()),
                line: 1,
                column: 5,
            },
            Token {
                token_type: TokenType::Equal,
                line: 1,
                column: 7,
            },
            Token {
                token_type: TokenType::Number(1.0),
                line: 1,
                column: 9,
            },
            Token {
                token_type: TokenType::Semicolon,
                line: 1,
                column: 10,
            },
            Token {
                token_type: TokenType::Eof,
                line: 1,
                column: 22,
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
                line: 1,
                column: 1,
            },
            Token {
                token_type: TokenType::Identifier("x".to_string()),
                line: 1,
                column: 5,
            },
            Token {
                token_type: TokenType::Equal,
                line: 1,
                column: 7,
            },
            Token {
                token_type: TokenType::Number(1.0),
                line: 1,
                column: 9,
            },
            Token {
                token_type: TokenType::Semicolon,
                line: 1,
                column: 10,
            },
            Token {
                token_type: TokenType::Eof,
                line: 1,
                column: 12,
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
                line: 1,
                column: 1,
            },
            Token {
                token_type: TokenType::Identifier("x".to_string()),
                line: 1,
                column: 5,
            },
            Token {
                token_type: TokenType::Equal,
                line: 1,
                column: 7,
            },
            Token {
                token_type: TokenType::String("hello".to_string()),
                line: 1,
                column: 9,
            },
            Token {
                token_type: TokenType::Semicolon,
                line: 1,
                column: 16,
            },
            Token {
                token_type: TokenType::Eof,
                line: 1,
                column: 17,
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
                line: 1,
                column: 1,
            },
            Token {
                token_type: TokenType::Identifier("x".to_string()),
                line: 1,
                column: 5,
            },
            Token {
                token_type: TokenType::Equal,
                line: 1,
                column: 7,
            },
            Token {
                token_type: TokenType::Number(1.0),
                line: 1,
                column: 9,
            },
            Token {
                token_type: TokenType::Semicolon,
                line: 1,
                column: 12,
            },
            Token {
                token_type: TokenType::Eof,
                line: 1,
                column: 13,
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
                line: 1,
                column: 1,
            },
            Token {
                token_type: TokenType::Identifier("x".to_string()),
                line: 1,
                column: 5,
            },
            Token {
                token_type: TokenType::Equal,
                line: 1,
                column: 7,
            },
            Token {
                token_type: TokenType::True,
                line: 1,
                column: 9,
            },
            Token {
                token_type: TokenType::Semicolon,
                line: 1,
                column: 13,
            },
            Token {
                token_type: TokenType::Eof,
                line: 1,
                column: 14,
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
                line: 1,
                column: 1,
            },
            Token {
                token_type: TokenType::EqualEqual,
                line: 1,
                column: 2,
            },
            Token {
                token_type: TokenType::Identifier("b".to_string()),
                line: 1,
                column: 4,
            },
            Token {
                token_type: TokenType::Eof,
                line: 1,
                column: 5,
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
