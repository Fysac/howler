use crate::token::Token;

pub(crate) trait Node {
    fn token_literal(&self) -> &str;
}

pub(crate) struct Program {
    pub(crate) statements: Vec<Statement>,
}
impl Node for Program {
    fn token_literal(&self) -> &str {
        match self.statements.len() {
            0 => "",
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
}
impl Node for Statement {
    fn token_literal(&self) -> &str {
        match self {
            Statement::Let { token: t, .. } | Statement::Return { token: t, .. } => &t.literal,
        }
    }
}

pub enum Expression {
    IntLiteral { token: Token, value: i64 },
}

pub(crate) struct Identifier {
    pub(crate) token: Token,
}
impl Node for Identifier {
    fn token_literal(&self) -> &str {
        self.token.literal.as_str()
    }
}
