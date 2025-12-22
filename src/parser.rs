use crate::ast;
use crate::token::*;

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        expected: TokenType,
        found: TokenType,
    },
    NotImplemented,
}
type ParseResult<T> = Result<T, ParseError>;

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Parser {
            lexer,
            cur_token,
            peek_token,
        }
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn cur_token_is(&self, token_type: TokenType) -> bool {
        self.cur_token.type_ == token_type
    }

    // Checks if the next token is the expected type and advances if so
    fn expect_peek(&mut self, token_type: TokenType) -> bool {
        if self.peek_token.type_ == token_type {
            self.next_token();
            return true;
        }
        false
    }

    pub fn parse_program(&mut self) -> ParseResult<ast::Program> {
        let mut statements: Vec<ast::Statement> = vec![];
        while self.cur_token.type_ != TokenType::EOF {
            statements.push(self.parse_statement()?);
            self.next_token();
        }
        Ok(ast::Program { statements })
    }

    fn parse_statement(&mut self) -> ParseResult<ast::Statement> {
        match self.cur_token.type_ {
            TokenType::Let => self.parse_let_statement(),
            _ => return Err(ParseError::NotImplemented),
        }
    }

    fn parse_let_statement(&mut self) -> ParseResult<ast::Statement> {
        let let_token = self.cur_token.clone();
        if !self.expect_peek(TokenType::Ident) {
            return Err(ParseError::UnexpectedToken {
                expected: TokenType::Ident,
                found: self.peek_token.type_.clone(),
            });
        }
        let ident = ast::Identifier {
            token: self.cur_token.clone(),
        };

        if !self.expect_peek(TokenType::Assign) {
            return Err(ParseError::UnexpectedToken {
                expected: TokenType::Assign,
                found: self.peek_token.type_.clone(),
            });
        }
        // Current token is now start of expression
        self.next_token();

        let let_stmt = ast::Statement::Let {
            token: let_token,
            name: ident,
            value: self.parse_expression()?,
        };

        while !(self.cur_token_is(TokenType::Semicolon)) {
            self.next_token();
        }
        Ok(let_stmt)
    }

    fn parse_expression(&mut self) -> ParseResult<ast::Expression> {
        match self.cur_token.type_ {
            TokenType::Int => Ok(ast::Expression::IntLiteral {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.parse().unwrap(),
            }),
            _ => return Err(ParseError::NotImplemented),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Node, Statement};

    #[test]
    fn test_let_statements() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
        "
        .as_bytes()
        .to_vec();

        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let prog = p.parse_program().unwrap_or_else(|e| {
            panic!("{:?}", e);
        });
        assert_eq!(prog.statements.len(), 3);

        let expected_idents = vec!["x", "y", "foobar"];
        for (i, expected_ident) in expected_idents.iter().enumerate() {
            test_let_statement(&prog.statements[i], expected_ident)
        }
    }

    fn test_let_statement(stmt: &Statement, expected_ident: &str) {
        if let Statement::Let { token, name, .. } = stmt {
            assert_eq!(token.literal, "let");
            assert_eq!(name.token_literal(), expected_ident);
        } else {
            assert!(false);
        }
    }
}
