enum Expression {
    Literal(Literal),
    Grouping(Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    Binary(Box<Expression>, InfixOperator, Box<Expression>),
}

enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

enum UnaryOperator {
    Negate,
    Not,
}

enum InfixOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}
