use std::fmt::Display;

#[derive(Debug)]
pub struct Program(pub Vec<Statement>);

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    VarDeclaration(String, Expression),
    Print(Expression),
    Block(Vec<Statement>),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    While(Expression, Box<Statement>),
}

#[derive(Debug)]
pub enum Expression {
    Identifier(String),
    Literal(Literal),
    Grouping(Box<Expression>),
    Binary(Box<Expression>, InfixOperator, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    Assign(String, Box<Expression>),
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
    And,
    Or,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for statement in &self.0 {
            writeln!(f, "{}", statement)?;
        }
        Ok(())
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Expression(expr) => write!(f, "{};", expr),
            Statement::Print(expr) => write!(f, "print {};", expr),
            Statement::VarDeclaration(name, expr) => write!(f, "var {} = {};", name, expr),
            Statement::Block(statements) => {
                writeln!(f, "{{")?;
                for statement in statements {
                    writeln!(f, "{}", statement)?;
                }
                write!(f, "}}")
            }
            Statement::If(condition, then_branch, else_branch) => {
                write!(f, "if ({}) ", condition)?;
                writeln!(f, "{}", then_branch)?;
                if let Some(else_branch) = else_branch {
                    writeln!(f, "else {}", else_branch)?;
                }
                Ok(())
            }
            Statement::While(condition, body) => {
                write!(f, "while ({}) ", condition)?;
                writeln!(f, "{}", body)
            }
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(name) => write!(f, "{}", name),
            Expression::Literal(literal) => write!(f, "{}", literal),
            Expression::Grouping(expr) => write!(f, "({})", expr),
            Expression::Binary(left, op, right) => write!(f, "({} {} {})", op, left, right),
            Expression::Unary(op, right) => write!(f, "({} {})", op, right),
            Expression::Assign(name, expr) => write!(f, "{} = {}", name, expr),
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
            InfixOperator::And => write!(f, "&&"),
            InfixOperator::Or => write!(f, "||"),
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
