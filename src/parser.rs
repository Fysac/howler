use crate::ast;
use crate::token::*;
use std::fmt;

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    pub errors: Vec<ParseError>,
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        expected: TokenType,
        found: TokenType,
    },
}
type ParseResult<T> = Result<T, ParseError>;

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken { expected, found } => {
                write!(f, "expected token {}, found {}", expected, found)
            }
            _ => todo!(),
        }
    }
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Parser {
            lexer,
            cur_token,
            peek_token,
            errors: vec![],
        }
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn cur_token_is(&self, token_type: TokenType) -> bool {
        self.cur_token.type_ == token_type
    }

    // Checks if the next token is the expected type and advances if so.
    // Otherwise, returns an UnexpectedToken error.
    fn expect_peek(&mut self, token_type: TokenType) -> Result<(), ParseError> {
        if self.peek_token.type_ == token_type {
            self.next_token();
            return Ok(());
        }

        Err(ParseError::UnexpectedToken {
            expected: token_type,
            found: self.peek_token.type_.clone(),
        })
    }

    pub fn parse_program(&mut self) -> ast::Program {
        let mut statements: Vec<ast::Statement> = vec![];
        while self.cur_token.type_ != TokenType::EOF {
            match self.parse_statement() {
                Ok(s) => statements.push(s),
                Err(e) => {
                    self.errors.push(e);
                }
            }
            // Temporary: skip to first semicolon
            while !self.cur_token_is(TokenType::Semicolon) && !self.cur_token_is(TokenType::EOF) {
                self.next_token()
            }
            // Skip over consecutive semicolons
            // TODO: revisit this
            while self.cur_token_is(TokenType::Semicolon) && !self.cur_token_is(TokenType::EOF) {
                self.next_token()
            }
        }
        ast::Program { statements }
    }

    fn parse_statement(&mut self) -> ParseResult<ast::Statement> {
        match self.cur_token.type_ {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => panic!(
                "statement not yet implemented: token {}",
                self.cur_token.type_
            ),
        }
    }

    fn parse_let_statement(&mut self) -> ParseResult<ast::Statement> {
        let let_token = self.cur_token.clone();
        self.expect_peek(TokenType::Ident)?;

        let ident = ast::Identifier {
            token: self.cur_token.clone(),
        };

        self.expect_peek(TokenType::Assign)?;
        // Current token is now start of expression
        self.next_token();

        Ok(ast::Statement::Let {
            token: let_token,
            name: ident,
            value: self.parse_expression()?,
        })
    }

    fn parse_return_statement(&mut self) -> ParseResult<ast::Statement> {
        let return_token = self.cur_token.clone();
        self.next_token();
        Ok(ast::Statement::Return {
            token: return_token,
            value: self.parse_expression()?,
        })
    }

    fn parse_expression(&mut self) -> ParseResult<ast::Expression> {
        match self.cur_token.type_ {
            TokenType::Int => Ok(ast::Expression::IntLiteral {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.parse().unwrap(),
            }),
            _ => panic!(
                "expression not yet implemented: token {}",
                self.cur_token.type_
            ),
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
        let mut parser = Parser::new(l);

        let prog = parser.parse_program();
        assert_eq!(parser.errors.len(), 0);
        assert_eq!(prog.statements.len(), 3);

        let expected_idents = vec!["x", "y", "foobar"];
        for (i, expected_ident) in expected_idents.iter().enumerate() {
            test_let_statement(&prog.statements[i], expected_ident)
        }
    }

    #[test]
    fn test_invalid_let_statements() {
        let input = "
        let x 5;
        let = 10;
        let 838383;
        "
        .as_bytes()
        .to_vec();

        let l = Lexer::new(input);
        let mut parser = Parser::new(l);

        let prog = parser.parse_program();
        assert_eq!(prog.statements.len(), 0);
        let expected_errors = vec![
            "expected token =, found int",
            "expected token ident, found =",
            "expected token ident, found int",
        ];
        assert_eq!(parser.errors.len(), expected_errors.iter().len());
        for (i, e) in parser.errors.iter().enumerate() {
            assert_eq!(e.to_string(), expected_errors[i]);
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

    #[test]
    fn test_return_statements() {
        let input = "
        return 5;
        return 10;
        return 993322;
        "
        .as_bytes()
        .to_vec();

        let l = Lexer::new(input);
        let mut parser = Parser::new(l);

        let prog = parser.parse_program();
        assert_eq!(parser.errors.len(), 0);
        assert_eq!(prog.statements.len(), 3);

        for s in prog.statements {
            test_return_statement(&s);
        }
    }

    fn test_return_statement(stmt: &Statement) {
        if let Statement::Return { token, .. } = stmt {
            assert_eq!(token.literal, "return");
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_display() {
        let prog = ast::Program {
            statements: vec![
                Statement::Let {
                    token: Token {
                        type_: TokenType::Let,
                        literal: "let".to_string(),
                    },
                    name: ast::Identifier {
                        token: Token {
                            type_: TokenType::Ident,
                            literal: "myVar".to_string(),
                        },
                    },
                    value: ast::Expression::IntLiteral {
                        token: Token {
                            type_: TokenType::Int,
                            literal: "10".to_string(),
                        },
                        value: 10,
                    },
                },
                Statement::Return {
                    token: Token {
                        type_: TokenType::Return,
                        literal: "return".to_string(),
                    },
                    value: ast::Expression::IntLiteral {
                        token: Token {
                            type_: TokenType::Int,
                            literal: "-42".to_string(),
                        },
                        value: -42,
                    },
                },
            ],
        };
        assert_eq!(prog.to_string(), "let myVar = 10; return -42;");
    }
}
