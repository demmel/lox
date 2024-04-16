use std::fmt::Display;

#[derive(Debug)]
pub struct Program(pub Vec<Statement>);

#[derive(Debug, Clone)]
pub enum Statement {
    Expression(Expression),
    VarDeclaration(String, Expression),
    FunctionDeclaration(String, Vec<String>, Box<Statement>),
    Print(Expression),
    Block(Vec<Statement>),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    While(Expression, Box<Statement>),
    Return(Option<Expression>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier {
        name: String,
        scope_depth: usize,
    },
    Literal(Literal),
    Grouping(Box<Expression>),
    Binary(Box<Expression>, InfixOperator, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    Assign {
        name: String,
        expr: Box<Expression>,
        scope_depth: usize,
    },
    Call(Box<Expression>, Vec<Expression>),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Negate,
    Not,
}

#[derive(Debug, Clone)]
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
            Statement::FunctionDeclaration(name, args, body) => {
                write!(f, "fun {name}(")?;
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{arg}")?;
                    if i != args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                writeln!(f, ") {body}")
            }
            Statement::Return(expr) => {
                if let Some(expr) = expr {
                    write!(f, "return {};", expr)
                } else {
                    write!(f, "return;")
                }
            }
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier { name, scope_depth } => {
                if *scope_depth == 0 {
                    write!(f, "{}", name)
                } else {
                    write!(f, "scope[{}].{}", scope_depth, name)
                }
            }
            Expression::Literal(literal) => write!(f, "{}", literal),
            Expression::Grouping(expr) => write!(f, "({})", expr),
            Expression::Binary(left, op, right) => write!(f, "({} {} {})", op, left, right),
            Expression::Unary(op, right) => write!(f, "({} {})", op, right),
            Expression::Assign {
                name,
                expr,
                scope_depth,
            } => {
                if *scope_depth == 0 {
                    write!(f, "{} = {}", name, expr)
                } else {
                    write!(f, "scope[{}].{} = {}", scope_depth, name, expr)
                }
            }
            Expression::Call(name, args) => {
                write!(f, "{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{}", arg)?;
                    if i != args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
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
