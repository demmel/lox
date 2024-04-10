use justerror::Error;

#[derive(Debug, PartialEq)]
pub enum Token {
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

impl Eq for Token {}

#[Error]
pub enum TokensError {
    TokenError(#[from] TokenError),
}

pub fn tokens(source: &str) -> Result<Vec<Token>, TokensError> {
    let mut tokens = Vec::new();
    let mut remaining = source;

    loop {
        let (token, rest) = token(remaining)?;
        remaining = rest;
        if matches!(token, Token::Eof) {
            tokens.push(token);
            break;
        }
        tokens.push(token);
    }

    Ok(tokens)
}

#[Error]
pub enum TokenError {
    UnexpectedCharacter(char),
}

pub fn token(mut source: &str) -> Result<(Token, &str), TokenError> {
    while let Some((_, rest)) = maximal(&[whitespace, comment], source) {
        source = rest;
    }

    if source.is_empty() {
        return Ok((Token::Eof, source));
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
        source,
    )
    .ok_or(TokenError::UnexpectedCharacter(
        source.chars().next().unwrap(),
    ))
}

fn maximal<'a, T: std::fmt::Debug>(
    parsers: &[fn(&str) -> Option<(T, &str)>],
    source: &'a str,
) -> Option<(T, &'a str)> {
    let mut min_left = source.len() + 1;
    let mut max_match = None;

    let matching_parsers = parsers.iter().filter_map(|parser| parser(source));
    for (m, rest) in matching_parsers {
        let left = rest.len();
        if left < min_left {
            min_left = left;
            max_match = Some((m, rest));
        }
    }

    max_match
}

fn whitespace(source: &str) -> Option<((), &str)> {
    let len = source
        .chars()
        .take_while(|c| c.is_whitespace())
        .map(char::len_utf8)
        .sum();
    if len > 0 {
        Some(((), &source[len..]))
    } else {
        None
    }
}

fn comment(source: &str) -> Option<((), &str)> {
    if source.starts_with("//") {
        let len = source
            .chars()
            .take_while(|c| *c != '\n')
            .map(char::len_utf8)
            .sum();
        Some(((), &source[len..]))
    } else {
        None
    }
}

macro_rules! match_literal {
    ($name:ident, $word:literal, $token:expr) => {
        fn $name(source: &str) -> Option<(Token, &str)> {
            if source.starts_with($word) {
                Some(($token, &source[$word.len()..]))
            } else {
                None
            }
        }
    };
}

match_literal! { left_paren, "(", Token::LeftParen }
match_literal! { right_paren, ")", Token::RightParen }
match_literal! { left_brace, "{", Token::LeftBrace }
match_literal! { right_brace, "}", Token::RightBrace }
match_literal! { comma, ",", Token::Comma }
match_literal! { dot, ".", Token::Dot }
match_literal! { minus, "-", Token::Minus }
match_literal! { plus, "+", Token::Plus }
match_literal! { semicolon, ";", Token::Semicolon }
match_literal! { slash, "/", Token::Slash }
match_literal! { star, "*", Token::Star }
match_literal! { bang, "!", Token::Bang }
match_literal! { equal, "=", Token::Equal }
match_literal! { greater, ">", Token::Greater }
match_literal! { less, "<", Token::Less }
match_literal! { bang_equal, "!=", Token::BangEqual }
match_literal! { equal_equal, "==", Token::EqualEqual }
match_literal! { greater_equal, ">=", Token::GreaterEqual }
match_literal! { less_equal, "<=", Token::LessEqual }
match_literal! { and, "and", Token::And }
match_literal! { class, "class", Token::Class }
match_literal! { else_, "else", Token::Else }
match_literal! { false_, "false", Token::False }
match_literal! { fun, "fun", Token::Fun }
match_literal! { for_, "for", Token::For }
match_literal! { if_, "if", Token::If }
match_literal! { nil, "nil", Token::Nil }
match_literal! { or, "or", Token::Or }
match_literal! { print_, "print", Token::Print }
match_literal! { return_, "return", Token::Return }
match_literal! { super_, "super", Token::Super }
match_literal! { this, "this", Token::This }
match_literal! { true_, "true", Token::True }
match_literal! { var, "var", Token::Var }
match_literal! { while_, "while", Token::While }

fn identifier(source: &str) -> Option<(Token, &str)> {
    let mut chars = source.chars();

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
        Some((Token::Identifier(source[..len].to_string()), &source[len..]))
    } else {
        None
    }
}

fn string(source: &str) -> Option<(Token, &str)> {
    if !source.starts_with('"') {
        return None;
    }

    let mut chars = source.chars().skip(1);
    let mut len = 1;
    while let Some(c) = chars.next() {
        len += c.len_utf8();
        match c {
            '"' => {
                return Some((
                    Token::String(source[1..len - 1].to_string()),
                    &source[len..],
                ))
            }
            _ => {}
        }
    }
    None
}

fn number(source: &str) -> Option<(Token, &str)> {
    let mut chars = source.chars().peekable();
    let first = chars.next()?;
    if !first.is_ascii_digit() && first != '.' && first != '-' {
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
        Token::Number(source[..len].parse().unwrap()),
        &source[len..],
    ))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_tokens() {
        let source = "var x = 1;";
        let expected = vec![
            Token::Var,
            Token::Identifier("x".to_string()),
            Token::Equal,
            Token::Number(1.0),
            Token::Semicolon,
            Token::Eof,
        ];
        assert_eq!(tokens(source).unwrap(), expected);
    }

    #[test]
    fn test_tokens_with_comments() {
        let source = "var x = 1; // comment";
        let expected = vec![
            Token::Var,
            Token::Identifier("x".to_string()),
            Token::Equal,
            Token::Number(1.0),
            Token::Semicolon,
            Token::Eof,
        ];
        assert_eq!(tokens(source).unwrap(), expected);
    }

    #[test]
    fn test_tokens_with_whitespace() {
        let source = "var x = 1; ";
        let expected = vec![
            Token::Var,
            Token::Identifier("x".to_string()),
            Token::Equal,
            Token::Number(1.0),
            Token::Semicolon,
            Token::Eof,
        ];
        assert_eq!(tokens(source).unwrap(), expected);
    }

    #[test]
    fn test_tokens_with_string() {
        let source = "var x = \"hello\";";
        let expected = vec![
            Token::Var,
            Token::Identifier("x".to_string()),
            Token::Equal,
            Token::String("hello".to_string()),
            Token::Semicolon,
            Token::Eof,
        ];
        assert_eq!(tokens(source).unwrap(), expected);
    }

    #[test]
    fn test_tokens_with_number() {
        let source = "var x = 1.0;";
        let expected = vec![
            Token::Var,
            Token::Identifier("x".to_string()),
            Token::Equal,
            Token::Number(1.0),
            Token::Semicolon,
            Token::Eof,
        ];
        assert_eq!(tokens(source).unwrap(), expected);
    }

    #[test]
    fn test_tokens_with_keywords() {
        let source = "var x = true;";
        let expected = vec![
            Token::Var,
            Token::Identifier("x".to_string()),
            Token::Equal,
            Token::True,
            Token::Semicolon,
            Token::Eof,
        ];
        assert_eq!(tokens(source).unwrap(), expected);
    }

    #[test]
    fn test_double_equal() {
        let source = "a==b";
        let expected = vec![
            Token::Identifier("a".to_string()),
            Token::EqualEqual,
            Token::Identifier("b".to_string()),
            Token::Eof,
        ];
        assert_eq!(tokens(source).unwrap(), expected);
    }
}
