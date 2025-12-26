use crate::ast::*;
use crate::token::*;
use std::fmt;

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    pub errors: Vec<ParseError>,
}

pub enum ParseError {
    UnexpectedToken {
        expected: TokenType,
        found: TokenType,
    },
}
type ParseResult<T> = Result<T, ParseError>;

enum Precedence {
    Lowest = 0,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

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

    pub fn parse_program(&mut self) -> Program {
        let mut statements: Vec<Statement> = vec![];
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
        Program { statements }
    }

    fn parse_statement(&mut self) -> ParseResult<Statement> {
        match self.cur_token.type_ {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> ParseResult<Statement> {
        let let_token = self.cur_token.clone();
        self.expect_peek(TokenType::Ident)?;

        let ident = Identifier {
            token: self.cur_token.clone(),
        };

        self.expect_peek(TokenType::Assign)?;
        // Current token is now start of expression
        self.next_token();

        Ok(Statement::Let {
            token: let_token,
            name: ident,
            value: self.parse_expression(Precedence::Lowest)?,
        })
    }

    fn parse_return_statement(&mut self) -> ParseResult<Statement> {
        let return_token = self.cur_token.clone();
        self.next_token();
        Ok(Statement::Return {
            token: return_token,
            value: self.parse_expression(Precedence::Lowest)?,
        })
    }

    fn parse_expression_statement(&mut self) -> ParseResult<Statement> {
        Ok(Statement::Expression {
            token: self.cur_token.clone(),
            expression: self.parse_expression(Precedence::Lowest)?,
        })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ParseResult<Expression> {
        let cur = &self.cur_token;
        match cur.type_ {
            TokenType::Ident => self.parse_identifier(),
            TokenType::Int => self.parse_integer_literal(),
            TokenType::Bang | TokenType::Minus => self.parse_prefix(),
            _ => Err(ParseError::UnexpectedToken {
                expected: TokenType::Int,
                found: cur.type_.clone(),
            })?,
        }
    }

    fn parse_identifier(&self) -> ParseResult<Expression> {
        Ok(Expression::Identifier {
            token: self.cur_token.clone(),
        })
    }

    fn parse_integer_literal(&self) -> ParseResult<Expression> {
        let value = self.cur_token.literal.parse().unwrap();
        Ok(Expression::IntLiteral {
            token: self.cur_token.clone(),
            value,
        })
    }

    fn parse_prefix(&mut self) -> ParseResult<Expression> {
        let prefix_token = self.cur_token.clone();
        self.next_token();
        let rhs = Box::new(self.parse_expression(Precedence::Prefix)?);
        Ok(Expression::Prefix {
            token: prefix_token,
            rhs,
        })
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
    fn test_ident_statements() {
        let input = "
        foobar;
        xyz;
        hello;
        "
        .as_bytes()
        .to_vec();

        let l = Lexer::new(input);
        let mut parser = Parser::new(l);

        let prog = parser.parse_program();
        assert_eq!(parser.errors.len(), 0);
        assert_eq!(prog.statements.len(), 3);
        let expected_idents = vec!["foobar", "xyz", "hello"];
        for (i, expected_ident) in expected_idents.iter().enumerate() {
            if let Statement::Expression { token, .. } = &prog.statements[i] {
                assert_eq!(token.literal, *expected_ident);
            } else {
                assert!(false);
            }
        }
    }

    // TODO: add test for prefix expressions

    #[test]
    fn test_display() {
        let prog = Program {
            statements: vec![
                Statement::Let {
                    token: Token {
                        type_: TokenType::Let,
                        literal: "let".to_string(),
                    },
                    name: Identifier {
                        token: Token {
                            type_: TokenType::Ident,
                            literal: "myVar".to_string(),
                        },
                    },
                    value: Expression::IntLiteral {
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
                    value: Expression::IntLiteral {
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
