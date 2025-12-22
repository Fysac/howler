use crate::token;
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
        token: token::Token,
        name: Identifier,
        value: Expression,
    },
    Return {
        value: Expression,
    },
}
impl Node for Statement {
    fn token_literal(&self) -> &str {
        match self {
            Statement::Let {
                token: t,
                name: _,
                value: _,
            } => &t.literal,
            _ => panic!("not implemented"),
        }
    }
}

pub enum Expression {
    IntLiteral { token: Token, value: i64 },
}

pub(crate) struct Identifier {
    pub(crate) token: token::Token,
}
impl Node for Identifier {
    fn token_literal(&self) -> &str {
        self.token.literal.as_str()
    }
}
