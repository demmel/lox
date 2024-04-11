use justerror::Error;

#[derive(Debug, Clone, PartialEq)]
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
pub enum TokenizeError {
    UnexpectedCharacter(char),
}

pub fn tokens(source: &str) -> Result<Vec<Token>, TokenizeError> {
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

pub fn token(mut source: &str) -> Result<(Token, &str), TokenizeError> {
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
    .ok_or(TokenizeError::UnexpectedCharacter(
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

fn literal<'a>(source: &'a str, literal: &str, token: Token) -> Option<(Token, &'a str)> {
    if source.starts_with(literal) {
        Some((token.clone(), &source[literal.len()..]))
    } else {
        None
    }
}

fn left_paren(source: &str) -> Option<(Token, &str)> {
    literal(source, "(", Token::LeftParen)
}

fn right_paren(source: &str) -> Option<(Token, &str)> {
    literal(source, ")", Token::RightParen)
}

fn left_brace(source: &str) -> Option<(Token, &str)> {
    literal(source, "{", Token::LeftBrace)
}

fn right_brace(source: &str) -> Option<(Token, &str)> {
    literal(source, "}", Token::RightBrace)
}

fn comma(source: &str) -> Option<(Token, &str)> {
    literal(source, ",", Token::Comma)
}

fn dot(source: &str) -> Option<(Token, &str)> {
    literal(source, ".", Token::Dot)
}

fn minus(source: &str) -> Option<(Token, &str)> {
    literal(source, "-", Token::Minus)
}

fn plus(source: &str) -> Option<(Token, &str)> {
    literal(source, "+", Token::Plus)
}

fn semicolon(source: &str) -> Option<(Token, &str)> {
    literal(source, ";", Token::Semicolon)
}

fn slash(source: &str) -> Option<(Token, &str)> {
    literal(source, "/", Token::Slash)
}

fn star(source: &str) -> Option<(Token, &str)> {
    literal(source, "*", Token::Star)
}

fn bang(source: &str) -> Option<(Token, &str)> {
    literal(source, "!", Token::Bang)
}

fn bang_equal(source: &str) -> Option<(Token, &str)> {
    literal(source, "!=", Token::BangEqual)
}

fn equal(source: &str) -> Option<(Token, &str)> {
    literal(source, "=", Token::Equal)
}

fn equal_equal(source: &str) -> Option<(Token, &str)> {
    literal(source, "==", Token::EqualEqual)
}

fn greater(source: &str) -> Option<(Token, &str)> {
    literal(source, ">", Token::Greater)
}

fn greater_equal(source: &str) -> Option<(Token, &str)> {
    literal(source, ">=", Token::GreaterEqual)
}

fn less(source: &str) -> Option<(Token, &str)> {
    literal(source, "<", Token::Less)
}

fn less_equal(source: &str) -> Option<(Token, &str)> {
    literal(source, "<=", Token::LessEqual)
}

fn and(source: &str) -> Option<(Token, &str)> {
    literal(source, "and", Token::And)
}

fn class(source: &str) -> Option<(Token, &str)> {
    literal(source, "class", Token::Class)
}

fn else_(source: &str) -> Option<(Token, &str)> {
    literal(source, "else", Token::Else)
}

fn false_(source: &str) -> Option<(Token, &str)> {
    literal(source, "false", Token::False)
}

fn fun(source: &str) -> Option<(Token, &str)> {
    literal(source, "fun", Token::Fun)
}

fn for_(source: &str) -> Option<(Token, &str)> {
    literal(source, "for", Token::For)
}

fn if_(source: &str) -> Option<(Token, &str)> {
    literal(source, "if", Token::If)
}

fn nil(source: &str) -> Option<(Token, &str)> {
    literal(source, "nil", Token::Nil)
}

fn or(source: &str) -> Option<(Token, &str)> {
    literal(source, "or", Token::Or)
}

fn print_(source: &str) -> Option<(Token, &str)> {
    literal(source, "print", Token::Print)
}

fn return_(source: &str) -> Option<(Token, &str)> {
    literal(source, "return", Token::Return)
}

fn super_(source: &str) -> Option<(Token, &str)> {
    literal(source, "super", Token::Super)
}

fn this(source: &str) -> Option<(Token, &str)> {
    literal(source, "this", Token::This)
}

fn true_(source: &str) -> Option<(Token, &str)> {
    literal(source, "true", Token::True)
}

fn var(source: &str) -> Option<(Token, &str)> {
    literal(source, "var", Token::Var)
}

fn while_(source: &str) -> Option<(Token, &str)> {
    literal(source, "while", Token::While)
}

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
