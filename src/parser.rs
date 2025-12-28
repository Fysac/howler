use crate::ast::*;
use crate::parser::Precedence::*;
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

#[derive(Copy, Clone)]
enum Precedence {
    Lowest = 0,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}
impl Precedence {
    fn for_token(token_type: &TokenType) -> Precedence {
        match token_type {
            TokenType::Plus | TokenType::Minus => Sum,
            TokenType::Asterisk | TokenType::Slash => Product,
            TokenType::Gt | TokenType::Lt => LessGreater,
            TokenType::Eq | TokenType::NotEq => Equals,
            _ => Lowest,
        }
    }
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

    fn parse_expression(&mut self, min_precedence: Precedence) -> ParseResult<Expression> {
        let cur = self.cur_token.clone();
        let mut left = match cur.type_ {
            TokenType::Ident => self.parse_identifier()?,
            TokenType::Int => self.parse_integer_literal()?,
            TokenType::Bang | TokenType::Minus => self.parse_prefix()?,
            _ => Err(ParseError::UnexpectedToken {
                expected: TokenType::Int,
                found: cur.type_.clone(),
            })?,
        };

        loop {
            let operator = self.peek_token.type_.clone();
            if (Precedence::for_token(&operator) as u8) < (min_precedence as u8) {
                break;
            }
            self.next_token();

            let right = match operator {
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Asterisk
                | TokenType::Slash
                | TokenType::Gt
                | TokenType::Lt
                | TokenType::Eq
                | TokenType::NotEq => {
                    self.next_token();
                    self.parse_expression(Precedence::for_token(&operator))?
                }
                _ => break,
            };

            left = Expression::Infix {
                token: cur.clone(),
                operator,
                left: Box::new(left),
                right: Box::new(right),
            }
        }
        Ok(left)
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
        Ok(Expression::Prefix {
            token: prefix_token,
            right: Box::new(self.parse_expression(Prefix)?),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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

        let expected_bindings = vec![("x", "5"), ("y", "10"), ("foobar", "838383")];
        assert_eq!(prog.statements.len(), expected_bindings.len());
        for (i, b) in expected_bindings.iter().enumerate() {
            test_let_statement(&prog.statements[i], b);
        }
    }
    fn test_let_statement(stmt: &Statement, expected_binding: &(&str, &str)) {
        let Statement::Let { token, name, value } = stmt else {
            panic!("expected Statement::Let");
        };
        assert_eq!(token.literal, "let");
        assert_eq!(name.token_literal(), expected_binding.0);
        let token = test_int_literal_expression(value, expected_binding.1.parse().unwrap());
        assert_eq!(token.literal, expected_binding.1);
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

        let expected_values = vec!["5", "10", "993322"];
        assert_eq!(prog.statements.len(), expected_values.len());
        for (i, s) in prog.statements.iter().enumerate() {
            test_return_statement(&s, expected_values[i]);
        }
    }
    fn test_return_statement(s: &Statement, expected_value: &str) {
        let Statement::Return { token, value } = s else {
            panic!("expected Statement::Return");
        };
        assert_eq!(token.literal, "return");
        let token = test_int_literal_expression(value, expected_value.parse().unwrap());
        assert_eq!(token.literal, expected_value);
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

        let expected_idents = vec!["foobar", "xyz", "hello"];
        assert_eq!(prog.statements.len(), expected_idents.len());
        for (i, expected_ident) in expected_idents.iter().enumerate() {
            let Statement::Expression { token, .. } = &prog.statements[i] else {
                panic!("expected Statement::Expression");
            };
            assert_eq!(token.literal, *expected_ident);
        }
    }

    #[test]
    fn test_prefix_expressions() {
        struct PrefixTest {
            input: String,
            operator: String,
            int_value: i64,
        }
        let tests = vec![
            PrefixTest {
                input: "!5".to_owned(),
                operator: "!".to_owned(),
                int_value: 5,
            },
            PrefixTest {
                input: "-15".to_owned(),
                operator: "-".to_owned(),
                int_value: 15,
            },
        ];

        for t in tests {
            let l = Lexer::new(t.input.into_bytes());
            let mut parser = Parser::new(l);
            let prog = parser.parse_program();
            assert_eq!(parser.errors.len(), 0);

            assert_eq!(prog.statements.len(), 1);
            let Statement::Expression { expression, .. } = &prog.statements[0] else {
                panic!("expected Statement::Expression");
            };
            let Expression::Prefix { token, right } = expression else {
                panic!("expected Expression::Prefix");
            };
            assert_eq!(token.literal, t.operator);
            test_int_literal_expression(right, t.int_value);
        }
    }

    #[test]
    fn test_infix_expressions() {
        struct InfixTest {
            input: String,
            left_value: i64,
            operator: String,
            right_value: i64,
        }
        let tests = vec![
            InfixTest {
                input: "5 + 5".to_owned(),
                left_value: 5,
                operator: "+".to_owned(),
                right_value: 5,
            },
            InfixTest {
                input: "5 - 5".to_owned(),
                left_value: 5,
                operator: "-".to_owned(),
                right_value: 5,
            },
            InfixTest {
                input: "5 * 5".to_owned(),
                left_value: 5,
                operator: "*".to_owned(),
                right_value: 5,
            },
            InfixTest {
                input: "5 / 5".to_owned(),
                left_value: 5,
                operator: "/".to_owned(),
                right_value: 5,
            },
            InfixTest {
                input: "5 > 5".to_owned(),
                left_value: 5,
                operator: ">".to_owned(),
                right_value: 5,
            },
            InfixTest {
                input: "5 < 5".to_owned(),
                left_value: 5,
                operator: "<".to_owned(),
                right_value: 5,
            },
            InfixTest {
                input: "5 == 5".to_owned(),
                left_value: 5,
                operator: "==".to_owned(),
                right_value: 5,
            },
            InfixTest {
                input: "5 != 5".to_owned(),
                left_value: 5,
                operator: "!=".to_owned(),
                right_value: 5,
            },
        ];

        for t in tests {
            let l = Lexer::new(t.input.into_bytes());
            let mut parser = Parser::new(l);
            let prog = parser.parse_program();
            assert_eq!(parser.errors.len(), 0);

            assert_eq!(prog.statements.len(), 1);
            let Statement::Expression { expression, .. } = &prog.statements[0] else {
                panic!("expected Statement::Expression");
            };
            let Expression::Infix {
                token,
                left,
                operator,
                right,
                ..
            } = expression
            else {
                panic!("expected Expression::Infix");
            };
            test_int_literal_expression(left, t.left_value);
            assert_eq!(operator.to_string(), t.operator);
            test_int_literal_expression(left, t.right_value);
        }
    }

    fn test_int_literal_expression(expr: &Expression, expected_value: i64) -> &Token {
        let Expression::IntLiteral { token, value } = expr else {
            panic!("expected Expression::IntLiteral");
        };
        assert_eq!(*value, expected_value);
        token
    }

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
