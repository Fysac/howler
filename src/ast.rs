use crate::token::Token;
use std::fmt;

pub(crate) trait Node: fmt::Display {
    fn token_literal(&self) -> String;
}

pub(crate) struct Program {
    pub(crate) statements: Vec<Statement>,
}
impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, s) in self.statements.iter().enumerate() {
            write!(f, "{}", s)?;
            if i < self.statements.len() - 1 {
                write!(f, " ")?;
            }
        }
        Ok(())
    }
}
impl Node for Program {
    fn token_literal(&self) -> String {
        match self.statements.len() {
            0 => "".to_owned(),
            _ => self.statements[0].token_literal(),
        }
    }
}

pub enum Statement {
    Let {
        token: Token,
        name: Identifier,
        value: Expression,
    },
    Return {
        token: Token,
        value: Expression,
    },
    Expression {
        token: Token,
        expression: Expression,
    },
}
impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let { name, value, .. } => {
                write!(f, "{} {} = {};", self.token_literal(), name, value)
            }
            Statement::Return { value, .. } => write!(f, "{} {};", self.token_literal(), value),
            Statement::Expression { expression, .. } => write!(f, "{};", expression),
        }
    }
}
impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::Let { token, .. }
            | Statement::Return { token, .. }
            | Statement::Expression { token, .. } => token.to_string(),
        }
    }
}

pub enum Expression {
    IntLiteral {
        token: Token,
        value: i64,
    },
    Identifier {
        token: Token,
    },
    Prefix {
        token: Token,
        right: Box<Expression>,
    },
    Infix {
        token: Token,
        operator: Token,
        left: Box<Expression>,
        right: Box<Expression>,
    },
}
impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::IntLiteral { token, .. } | Expression::Identifier { token, .. } => {
                write!(f, "{}", token.to_string())
            }
            Expression::Prefix { token, right } => {
                write!(f, "({}{})", token.to_string(), right)
            }
            Expression::Infix {
                operator,
                left,
                right,
                ..
            } => {
                write!(f, "({} {} {})", left, operator, right)
            }
        }
    }
}
impl Node for Expression {
    fn token_literal(&self) -> String {
        todo!()
    }
}

pub(crate) struct Identifier {
    pub(crate) token: Token,
}
impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.token_literal())
    }
}
impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.to_string()
    }
}
