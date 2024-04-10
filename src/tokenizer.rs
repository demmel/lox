use justerror::Error;

#[derive(Debug)]
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
    Identifier,
    String,
    Number,

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

    Eof,
}

#[Error]
pub enum TokensError {
    UnexpectedCharacter(char),
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

pub fn token(source: &str) -> Result<(Token, &str), TokensError> {
    let mut char_indices = source.char_indices().peekable();

    let (next, token) = loop {
        let Some((mut i, c)) = char_indices.next() else {
            return Ok((Token::Eof, source));
        };

        let token = match c {
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            ',' => Token::Comma,
            '.' => Token::Dot,
            '-' => Token::Minus,
            '+' => Token::Plus,
            ';' => Token::Semicolon,
            '*' => Token::Star,
            '!' => {
                if let Some(&(j, '=')) = char_indices.peek() {
                    i = j;
                    char_indices.next();
                    Token::BangEqual
                } else {
                    Token::Bang
                }
            }
            '=' => {
                if let Some(&(j, '=')) = char_indices.peek() {
                    i = j;
                    char_indices.next();
                    Token::EqualEqual
                } else {
                    Token::Equal
                }
            }
            '<' => {
                if let Some(&(j, '=')) = char_indices.peek() {
                    i = j;
                    char_indices.next();
                    Token::LessEqual
                } else {
                    Token::Less
                }
            }
            '>' => {
                if let Some(&(j, '=')) = char_indices.peek() {
                    i = j;
                    char_indices.next();
                    Token::GreaterEqual
                } else {
                    Token::Greater
                }
            }
            '/' => {
                if let Some(&(_, '/')) = char_indices.peek() {
                    while let Some((_, c)) = char_indices.next() {
                        if c == '\n' {
                            break;
                        }
                    }
                    continue;
                } else {
                    Token::Slash
                }
            }
            c => {
                if c.is_whitespace() {
                    continue;
                } else {
                    return Err(TokensError::UnexpectedCharacter(c));
                }
            }
        };

        break (i + c.len_utf8(), token);
    };

    Ok((token, &source[next..]))
}
