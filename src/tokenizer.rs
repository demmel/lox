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
            TokenType::Identifier => "identifier",
            TokenType::String => "string",
            TokenType::Number => "number",
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
pub struct Token<'a> {
    pub token_type: TokenType,
    pub lexeme: &'a str,
    pub line: usize,
    pub column: usize,
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?} {}:{} {}",
            self.token_type, self.line, self.column, self.lexeme
        )
    }
}

impl Token<'_> {
    pub fn token_type(&self) -> &TokenType {
        &self.token_type
    }
}

#[derive(Debug, Clone)]
pub enum TokenizeErrorKind {
    UnexpectedCharacter,
}

#[derive(Debug, Clone)]
pub struct TokenizeError<'a> {
    pub state: Tokenizer<'a>,
    pub kind: TokenizeErrorKind,
}

impl std::error::Error for TokenizeError<'_> {}

impl std::fmt::Display for TokenizeError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            TokenizeErrorKind::UnexpectedCharacter => write!(
                f,
                "Unexpected character '{}'",
                self.state.remaining.chars().next().unwrap(),
            )?,
        }
        write!(
            f,
            " at line {} column {}",
            self.state.line, self.state.column
        )
    }
}

#[derive(Debug, Clone)]
pub struct Tokenizer<'a> {
    remaining: &'a str,
    line: usize,
    column: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a str) -> Tokenizer<'a> {
        Tokenizer {
            remaining: source,
            line: 1,
            column: 1,
        }
    }

    pub fn token<'b>(&'b mut self) -> Result<Token<'a>, TokenizeError<'a>> {
        let (token, next_state) = token(self.clone())?;
        *self = next_state;
        Ok(token)
    }

    pub fn recover(&mut self) {
        let mut chars = self.remaining.chars();
        while let Some(c) = chars.next() {
            self.column += 1;
            if c == ';' {
                self.remaining = chars.as_str();
                return;
            } else if c == '\n' {
                self.remaining = chars.as_str();
                self.line += 1;
                self.column = 1;
                return;
            }
        }
    }

    fn into_error(self, kind: TokenizeErrorKind) -> TokenizeError<'a> {
        TokenizeError { state: self, kind }
    }
}

pub fn tokens(source: &str) -> Result<Vec<Token>, TokenizeError> {
    let mut tokens = Vec::new();
    let mut state = Tokenizer {
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

pub fn token(mut state: Tokenizer) -> Result<(Token, Tokenizer), TokenizeError> {
    let parsers = [whitespace, comment];
    while let Some((_, next_state)) = maximal(&parsers, &state) {
        state = next_state;
    }

    if state.remaining.is_empty() {
        return Ok((
            Token {
                token_type: TokenType::Eof,
                lexeme: state.remaining,
                line: state.line,
                column: state.column,
            },
            state,
        ));
    }

    let parsers = [
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
    ];

    maximal(&parsers, &state)
        .ok_or_else(|| state.into_error(TokenizeErrorKind::UnexpectedCharacter))
}

fn maximal<'a, T, F>(parsers: &[F], state: &Tokenizer<'a>) -> Option<(T, Tokenizer<'a>)>
where
    F: Fn(Tokenizer<'a>) -> Option<(T, Tokenizer<'a>)>,
{
    let state = state.clone();
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

fn whitespace(mut state: Tokenizer) -> Option<((), Tokenizer)> {
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
            Tokenizer {
                remaining: &state.remaining[len..],
                ..state
            },
        ))
    } else {
        None
    }
}

fn comment(mut state: Tokenizer) -> Option<((), Tokenizer)> {
    if state.remaining.starts_with("//") {
        let mut chars = state.remaining.chars();
        while let Some(c) = chars.next() {
            if c == '\n' {
                return Some((
                    (),
                    Tokenizer {
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
            Tokenizer {
                remaining: chars.as_str(),
                ..state
            },
        ));
    } else {
        None
    }
}

fn literal<'a>(
    state: Tokenizer<'a>,
    literal: &str,
    token_type: TokenType,
) -> Option<(Token<'a>, Tokenizer<'a>)> {
    if state.remaining.starts_with(literal) {
        let len = literal.len();
        let count = literal.chars().count();
        Some((
            Token {
                token_type,
                lexeme: &state.remaining[..len],
                line: state.line,
                column: state.column,
            },
            Tokenizer {
                remaining: &state.remaining[len..],
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
        fn $name<'a>(state: Tokenizer<'a>) -> Option<(Token<'a>, Tokenizer<'a>)> {
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

fn identifier(state: Tokenizer) -> Option<(Token, Tokenizer)> {
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
                token_type: TokenType::Identifier,
                lexeme: &state.remaining[..len],
                line: state.line,
                column: state.column,
            },
            Tokenizer {
                remaining: &state.remaining[len..],
                column: state.column + count,
                ..state
            },
        ))
    } else {
        None
    }
}

fn string(state: Tokenizer) -> Option<(Token, Tokenizer)> {
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
                        token_type: TokenType::String,
                        lexeme: &state.remaining[..len],
                        line: state.line,
                        column: state.column,
                    },
                    Tokenizer {
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

fn number(state: Tokenizer) -> Option<(Token, Tokenizer)> {
    // Valid number types
    // 123
    // 123.456
    // .456
    // 123.
    //
    // We need to make sure we don't consume the dot if it's not followed by a number.

    let mut chars = state.remaining.chars();
    let mut len = 0;
    let mut count = 0;
    let mut has_dot = false;
    let mut has_number = false;

    while let Some(c) = chars.next() {
        if c.is_ascii_digit() {
            len += c.len_utf8();
            count += 1;
            has_number = true;
        } else if c == '.' {
            if has_dot {
                return None;
            }
            has_dot = true;
            len += c.len_utf8();
            count += 1;
        } else {
            break;
        }
    }

    if has_number {
        Some((
            Token {
                token_type: TokenType::Number,
                lexeme: &state.remaining[..len],
                line: state.line,
                column: state.column,
            },
            Tokenizer {
                remaining: &state.remaining[len..],
                column: state.column + count,
                ..state
            },
        ))
    } else {
        None
    }
}
