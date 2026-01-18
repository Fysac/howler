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
        expected: TokenKind,
        found: TokenKind,
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
    fn for_token(token: &Token) -> Precedence {
        match token.kind {
            TokenKind::Plus | TokenKind::Minus => Sum,
            TokenKind::Asterisk | TokenKind::Slash => Product,
            TokenKind::Gt | TokenKind::Lt => LessGreater,
            TokenKind::Eq | TokenKind::NotEq => Equals,
            _ => Lowest,
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken { expected, found } => {
                let expected_str = match expected {
                    // Don't print the literal when ident or int is expected
                    TokenKind::Ident(_) => "ident",
                    TokenKind::Int(_) => "int",
                    _ => &expected.to_string(),
                };
                write!(f, "expected token {}, found {}", expected_str, found)
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

    fn cur_token_is_kind(&self, kind: TokenKind) -> bool {
        self.cur_token.is_kind(&kind)
    }
    fn peek_token_is_kind(&self, kind: TokenKind) -> bool {
        self.peek_token.is_kind(&kind)
    }

    // Checks if the next token is the expected kind (ignoring any associated data) and advances if so.
    // Otherwise, returns an UnexpectedToken error.
    fn expect_peek_kind(&mut self, kind: TokenKind) -> Result<(), ParseError> {
        if self.peek_token.is_kind(&kind) {
            self.next_token();
            return Ok(());
        }
        Err(ParseError::UnexpectedToken {
            expected: kind,
            found: self.peek_token.kind.clone(),
        })
    }

    fn peek_precedence(&self) -> Precedence {
        Precedence::for_token(&self.peek_token)
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements: Vec<Statement> = vec![];
        while !self.cur_token_is_kind(TokenKind::EOF) {
            match self.parse_statement() {
                Ok(s) => statements.push(s),
                Err(e) => {
                    self.errors.push(e);
                }
            }
            // Temporary: skip to first semicolon
            while !self.cur_token_is_kind(TokenKind::Semicolon)
                && !self.cur_token_is_kind(TokenKind::EOF)
            {
                self.next_token()
            }
            // Skip over consecutive semicolons
            // TODO: revisit this
            while self.cur_token_is_kind(TokenKind::Semicolon)
                && !self.cur_token_is_kind(TokenKind::EOF)
            {
                self.next_token()
            }
        }
        Program { statements }
    }

    fn parse_statement(&mut self) -> ParseResult<Statement> {
        match self.cur_token.kind {
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> ParseResult<Statement> {
        let let_token = self.cur_token.clone();
        self.expect_peek_kind(TokenKind::Ident("_".to_owned()))?;

        let ident = Identifier {
            token: self.cur_token.clone(),
        };

        self.expect_peek_kind(TokenKind::Assign)?;
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
        let mut left = match cur.kind {
            TokenKind::Ident(_) => self.parse_identifier(),
            TokenKind::Int(literal) => self.parse_integer_literal(&literal)?,
            TokenKind::Bang | TokenKind::Minus => self.parse_prefix()?,
            _ => Err(ParseError::UnexpectedToken {
                expected: TokenKind::Int("_".to_owned()),
                found: cur.kind,
            })?,
        };

        while !self.peek_token_is_kind(TokenKind::Semicolon)
            && !self.peek_token_is_kind(TokenKind::EOF)
            // TODO: precedence for unknown operators should return an error instead of skipping over the loop and returning left
            // i.e., this is wrong:
            // >> let x = 1 # 1;
            // let x = 1;
            && self.peek_precedence() as u8 > min_precedence as u8
        {
            self.next_token();
            left = self.parse_infix(Box::new(left))?
        }
        Ok(left)
    }

    fn parse_identifier(&self) -> Expression {
        Expression::Identifier {
            token: self.cur_token.clone(),
        }
    }

    fn parse_integer_literal(&self, literal: &str) -> ParseResult<Expression> {
        let value = literal.parse().unwrap();
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

    fn parse_infix(&mut self, left: Box<Expression>) -> ParseResult<Expression> {
        let operator_token = self.cur_token.clone();
        match operator_token.kind {
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Asterisk
            | TokenKind::Slash
            | TokenKind::Gt
            | TokenKind::Lt
            | TokenKind::Eq
            | TokenKind::NotEq => {
                self.next_token();
                Ok(Expression::Infix {
                    token: operator_token.clone(),
                    operator: operator_token.clone(),
                    left,
                    right: Box::new(self.parse_expression(Precedence::for_token(&operator_token))?),
                })
            }
            _ => Err(ParseError::UnexpectedToken {
                // TODO: this is a placeholder
                expected: TokenKind::Plus,
                found: operator_token.kind,
            })?,
        }
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
        assert_eq!(token.to_string(), "let");
        assert_eq!(name.token_literal(), expected_binding.0);
        let token = test_int_literal_expression(value, expected_binding.1.parse().unwrap());
        assert_eq!(token.to_string(), expected_binding.1);
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
            "expected token =, found 5",
            "expected token ident, found =",
            "expected token ident, found 838383",
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
        assert_eq!(token.to_string(), "return");
        let token = test_int_literal_expression(value, expected_value.parse().unwrap());
        assert_eq!(token.to_string(), expected_value);
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
            assert_eq!(token.to_string(), *expected_ident);
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
            assert_eq!(token.to_string(), t.operator);
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

    #[test]
    fn operator_precedence_parsing() {
        struct Test {
            input: String,
            expected: String,
        }
        let tests = vec![
            Test {
                input: "-a * b".to_owned(),
                expected: "((-a) * b);".to_owned(),
            },
            Test {
                input: "!-a".to_owned(),
                expected: "(!(-a));".to_owned(),
            },
            Test {
                input: "a + b + c".to_owned(),
                expected: "((a + b) + c);".to_owned(),
            },
            Test {
                input: "a + b - c".to_owned(),
                expected: "((a + b) - c);".to_owned(),
            },
            Test {
                input: "a * b * c".to_owned(),
                expected: "((a * b) * c);".to_owned(),
            },
            Test {
                input: "a * b / c".to_owned(),
                expected: "((a * b) / c);".to_owned(),
            },
            Test {
                input: "a + b / c".to_owned(),
                expected: "(a + (b / c));".to_owned(),
            },
            Test {
                input: "a + b * c + d / e - f".to_owned(),
                expected: "(((a + (b * c)) + (d / e)) - f);".to_owned(),
            },
            Test {
                input: "3 + 4; -5 * 5".to_owned(),
                expected: "(3 + 4); ((-5) * 5);".to_owned(),
            },
            Test {
                input: "5 > 4 == 3 < 4".to_owned(),
                expected: "((5 > 4) == (3 < 4));".to_owned(),
            },
            Test {
                input: "5 < 4 != 3 > 4".to_owned(),
                expected: "((5 < 4) != (3 > 4));".to_owned(),
            },
            Test {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5".to_owned(),
                expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));".to_owned(),
            },
        ];

        for t in tests {
            let l = Lexer::new(t.input.into_bytes());
            let mut parser = Parser::new(l);
            let prog = parser.parse_program();
            assert_eq!(parser.errors.len(), 0);
            assert_eq!(prog.to_string(), t.expected);
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
                        kind: TokenKind::Let,
                    },
                    name: Identifier {
                        token: Token {
                            kind: TokenKind::Ident("myVar".to_owned()),
                        },
                    },
                    value: Expression::IntLiteral {
                        token: Token {
                            kind: TokenKind::Int("10".to_owned()),
                        },
                        value: 10,
                    },
                },
                Statement::Return {
                    token: Token {
                        kind: TokenKind::Return,
                    },
                    value: Expression::IntLiteral {
                        token: Token {
                            kind: TokenKind::Int("-42".to_owned()),
                        },
                        value: -42,
                    },
                },
            ],
        };
        assert_eq!(prog.to_string(), "let myVar = 10; return -42;");
    }
}
