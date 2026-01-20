use crate::ast::*;
use crate::parser::ParseError::*;
use crate::parser::Precedence::*;
use crate::token::TokenKind::*;
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
        expected: Option<TokenKind>,
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
    fn for_token(token: &Token) -> Option<Precedence> {
        let p = match token.kind {
            Plus | Minus => Sum,
            Asterisk | Slash => Product,
            Gt | Lt => LessGreater,
            Eq | NotEq => Equals,
            _ => return None,
        };
        Some(p)
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnexpectedToken { expected, found } => {
                let expected_str = match expected {
                    // Don't print the literal when ident or int is expected
                    Some(Ident(_)) => "ident",
                    Some(Int(_)) => "int",

                    // Print the expected literal in quotes for all other tokens
                    Some(kind) => &format!("token '{}'", kind.to_string()),

                    // Omit expected part when several tokens were valid and `found` just wasn't one of them
                    None => {
                        return if *found == EOF {
                            write!(f, "unexpected end of file")
                        } else {
                            write!(f, "unexpected token '{}'", found.to_string())
                        };
                    }
                };
                write!(f, "expected {}, found '{}'", expected_str, found)
            }
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
        Err(UnexpectedToken {
            expected: Some(kind),
            found: self.peek_token.kind.clone(),
        })
    }

    fn peek_precedence(&self) -> Option<Precedence> {
        Precedence::for_token(&self.peek_token)
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements: Vec<Statement> = vec![];
        while !self.cur_token_is_kind(EOF) {
            // Skip over empty statements
            while self.cur_token_is_kind(Semicolon) {
                self.next_token()
            }
            if self.cur_token_is_kind(TokenKind::EOF) {
                break;
            }

            match self.parse_statement() {
                Ok(s) => statements.push(s),
                Err(e) => {
                    self.errors.push(e);

                    while !self.cur_token_is_kind(Semicolon) && !self.cur_token_is_kind(EOF) {
                        self.next_token()
                    }
                }
            }
            self.next_token();
        }
        Program { statements }
    }

    fn parse_statement(&mut self) -> ParseResult<Statement> {
        let statement = match self.cur_token.kind {
            Let => self.parse_let_statement()?,
            Return => self.parse_return_statement()?,
            _ => self.parse_expression_statement()?,
        };
        Ok(statement)
    }

    fn parse_let_statement(&mut self) -> ParseResult<Statement> {
        let let_token = self.cur_token.clone();
        self.expect_peek_kind(Ident("_".to_owned()))?;

        let ident = Identifier {
            token: self.cur_token.clone(),
        };

        self.expect_peek_kind(Assign)?;
        // Current token is now start of expression
        self.next_token();

        Ok(Statement::Let {
            token: let_token,
            name: ident,
            value: self.parse_expression(Lowest)?,
        })
    }

    fn parse_return_statement(&mut self) -> ParseResult<Statement> {
        let return_token = self.cur_token.clone();
        self.next_token();
        Ok(Statement::Return {
            token: return_token,
            value: self.parse_expression(Lowest)?,
        })
    }

    fn parse_expression_statement(&mut self) -> ParseResult<Statement> {
        Ok(Statement::Expression {
            token: self.cur_token.clone(),
            expression: self.parse_expression(Lowest)?,
        })
    }

    fn parse_expression(&mut self, min_precedence: Precedence) -> ParseResult<Expression> {
        let cur = self.cur_token.clone();
        let mut left = match cur.kind {
            Ident(_) => self.parse_identifier(),
            Int(literal) => self.parse_integer_literal(&literal)?,
            True | False => self.parse_boolean_literal(self.cur_token.kind == True)?,
            Bang | Minus => self.parse_prefix()?,
            LeftParen => self.parse_grouped_expression()?,
            If => self.parse_if_expression()?,
            _ => Err(UnexpectedToken {
                expected: None,
                found: cur.kind,
            })?,
        };

        loop {
            match self.peek_precedence() {
                Some(p) if p as u8 > min_precedence as u8 => {
                    self.next_token();
                    left = self.parse_infix(Box::new(left))?
                }
                Some(_) => break,
                None => {
                    // End of grouped expression
                    if self.peek_token_is_kind(RightParen) {
                        break;
                    }

                    // End of block statement
                    if self.peek_token_is_kind(RightBrace) {
                        break;
                    }

                    // End of statement
                    if self.peek_token_is_kind(Semicolon) || self.peek_token_is_kind(EOF) {
                        break;
                    }
                    // Next token not valid in expression
                    return Err(UnexpectedToken {
                        expected: None,
                        found: self.peek_token.kind.clone(),
                    });
                }
            }
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

    fn parse_boolean_literal(&self, value: bool) -> ParseResult<Expression> {
        Ok(Expression::BoolLiteral {
            token: self.cur_token.clone(),
            value,
        })
    }

    fn parse_grouped_expression(&mut self) -> ParseResult<Expression> {
        // Consume left parenthesis
        self.next_token();

        let expression = self.parse_expression(Lowest)?;

        // Check for and consume right parenthesis
        self.expect_peek_kind(RightParen)?;
        Ok(expression)
    }

    fn parse_if_expression(&mut self) -> ParseResult<Expression> {
        let if_token = self.cur_token.clone();
        self.expect_peek_kind(LeftParen)?;
        self.next_token();
        let condition = self.parse_expression(Lowest)?;
        self.expect_peek_kind(RightParen)?;

        let consequence = self.parse_block()?;

        let mut alternative = None;
        if self.peek_token_is_kind(Else) {
            self.next_token();
            alternative = Some(self.parse_block()?);
        }

        Ok((Expression::If {
            token: if_token,
            condition: Box::new(condition),
            consequence,
            alternative,
        }))
    }

    // TODO: address parse_program code duplication
    fn parse_block(&mut self) -> ParseResult<Block> {
        self.expect_peek_kind(LeftBrace)?;
        self.next_token();

        let mut statements: Vec<Statement> = vec![];
        while !self.cur_token_is_kind(RightBrace) && !self.cur_token_is_kind(EOF) {
            // Skip over empty statements
            while self.cur_token_is_kind(Semicolon) {
                self.next_token()
            }
            if self.cur_token_is_kind(RightBrace) {
                break;
            }

            match self.parse_statement() {
                Ok(s) => statements.push(s),
                Err(e) => {
                    self.errors.push(e);

                    while !self.cur_token_is_kind(Semicolon)
                        && !self.peek_token_is_kind(RightBrace)
                        && !self.cur_token_is_kind(EOF)
                    {
                        self.next_token()
                    }
                }
            }
            self.next_token();
        }

        if !self.cur_token_is_kind(RightBrace) {
            return Err(UnexpectedToken {
                expected: Some(RightBrace),
                found: self.cur_token.kind.clone(),
            });
        }

        Ok(Block { statements })
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
        let Some(precedence) = Precedence::for_token(&operator_token) else {
            panic!("no precedence for current token (we shouldn't ever get here)");
        };

        match operator_token.kind {
            Plus | Minus | Asterisk | Slash | Gt | Lt | Eq | NotEq => {
                self.next_token();
                Ok(Expression::Infix {
                    token: operator_token.clone(),
                    operator: operator_token.clone(),
                    left,
                    right: Box::new(self.parse_expression(precedence)?),
                })
            }
            _ => panic!("current token not a valid operator (we shouldn't ever get here)"),
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
        test_int_literal_expression(value, expected_binding.1.parse().unwrap());
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
            "expected token '=', found '5'",
            "expected ident, found '='",
            "expected ident, found '838383'",
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
        test_int_literal_expression(value, expected_value.parse().unwrap());
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

    enum ExpectedValue {
        Int(i64),
        Bool(bool),
    }
    #[test]
    fn test_prefix_expressions() {
        struct PrefixTest {
            input: String,
            operator: String,
            value: ExpectedValue,
        }
        let tests = vec![
            PrefixTest {
                input: "!5".to_owned(),
                operator: "!".to_owned(),
                value: ExpectedValue::Int(5),
            },
            PrefixTest {
                input: "-15".to_owned(),
                operator: "-".to_owned(),
                value: ExpectedValue::Int(15),
            },
            PrefixTest {
                input: "!true".to_owned(),
                operator: "!".to_owned(),
                value: ExpectedValue::Bool(true),
            },
            PrefixTest {
                input: "!false".to_owned(),
                operator: "!".to_owned(),
                value: ExpectedValue::Bool(false),
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
            match t.value {
                ExpectedValue::Int(i) => test_int_literal_expression(right, i),
                ExpectedValue::Bool(b) => test_bool_literal_expression(right, b),
            };
        }
    }

    struct InfixTest {
        input: String,
        left_value: ExpectedValue,
        operator: String,
        right_value: ExpectedValue,
    }
    #[test]
    fn test_infix_expressions() {
        let tests = vec![
            InfixTest {
                input: "5 + 5".to_owned(),
                left_value: ExpectedValue::Int(5),
                operator: "+".to_owned(),
                right_value: ExpectedValue::Int(5),
            },
            InfixTest {
                input: "5 - 5".to_owned(),
                left_value: ExpectedValue::Int(5),
                operator: "-".to_owned(),
                right_value: ExpectedValue::Int(5),
            },
            InfixTest {
                input: "5 * 5".to_owned(),
                left_value: ExpectedValue::Int(5),
                operator: "*".to_owned(),
                right_value: ExpectedValue::Int(5),
            },
            InfixTest {
                input: "5 / 5".to_owned(),
                left_value: ExpectedValue::Int(5),
                operator: "/".to_owned(),
                right_value: ExpectedValue::Int(5),
            },
            InfixTest {
                input: "5 > 5".to_owned(),
                left_value: ExpectedValue::Int(5),
                operator: ">".to_owned(),
                right_value: ExpectedValue::Int(5),
            },
            InfixTest {
                input: "5 < 5".to_owned(),
                left_value: ExpectedValue::Int(5),
                operator: "<".to_owned(),
                right_value: ExpectedValue::Int(5),
            },
            InfixTest {
                input: "5 == 5".to_owned(),
                left_value: ExpectedValue::Int(5),
                operator: "==".to_owned(),
                right_value: ExpectedValue::Int(5),
            },
            InfixTest {
                input: "5 != 5".to_owned(),
                left_value: ExpectedValue::Int(5),
                operator: "!=".to_owned(),
                right_value: ExpectedValue::Int(5),
            },
            InfixTest {
                input: "5 != 5".to_owned(),
                left_value: ExpectedValue::Int(5),
                operator: "!=".to_owned(),
                right_value: ExpectedValue::Int(5),
            },
            InfixTest {
                input: "true == true".to_owned(),
                left_value: ExpectedValue::Bool(true),
                operator: "==".to_owned(),
                right_value: ExpectedValue::Bool(true),
            },
            InfixTest {
                input: "true != false".to_owned(),
                left_value: ExpectedValue::Bool(true),
                operator: "!=".to_owned(),
                right_value: ExpectedValue::Bool(false),
            },
            InfixTest {
                input: "false == false".to_owned(),
                left_value: ExpectedValue::Bool(false),
                operator: "==".to_owned(),
                right_value: ExpectedValue::Bool(false),
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

            match t.left_value {
                ExpectedValue::Int(i) => test_int_literal_expression(left, i),
                ExpectedValue::Bool(b) => test_bool_literal_expression(left, b),
            };

            assert_eq!(operator.to_string(), t.operator);

            match t.right_value {
                ExpectedValue::Int(i) => test_int_literal_expression(right, i),
                ExpectedValue::Bool(b) => test_bool_literal_expression(right, b),
            };
        }
    }

    #[test]
    fn test_invalid_infix_expressions() {
        let tests = vec![
            InfixTest {
                input: "5 # 5".to_owned(),
                left_value: ExpectedValue::Int(5),
                operator: "#".to_owned(),
                right_value: ExpectedValue::Int(5),
            },
            InfixTest {
                input: "5 $ 5".to_owned(),
                left_value: ExpectedValue::Int(5),
                operator: "$".to_owned(),
                right_value: ExpectedValue::Int(5),
            },
            InfixTest {
                input: "5 @ 5".to_owned(),
                left_value: ExpectedValue::Int(5),
                operator: "@".to_owned(),
                right_value: ExpectedValue::Int(5),
            },
            InfixTest {
                input: "5 ~= 5".to_owned(),
                left_value: ExpectedValue::Int(5),
                operator: "~=".to_owned(),
                right_value: ExpectedValue::Int(5),
            },
        ];

        let expected_errors = vec![
            "unexpected token '#'",
            "unexpected token '$'",
            "unexpected token '@'",
            "unexpected token '~'",
        ];

        assert_eq!(tests.len(), expected_errors.len());

        let mut i = 0;
        for t in tests {
            let l = Lexer::new(t.input.into_bytes());
            let mut parser = Parser::new(l);
            let prog = parser.parse_program();

            assert_eq!(parser.errors.len(), 1);
            assert_eq!(prog.statements.len(), 0);
            assert_eq!(parser.errors[0].to_string(), expected_errors[i]);

            i += 1;
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
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
            Test {
                input: "1 + (2 + 3) + 4".to_owned(),
                expected: "((1 + (2 + 3)) + 4);".to_owned(),
            },
            Test {
                input: "(5 + 5) * 2".to_owned(),
                expected: "((5 + 5) * 2);".to_owned(),
            },
            Test {
                input: "2 / (5 + 5)".to_owned(),
                expected: "(2 / (5 + 5));".to_owned(),
            },
            Test {
                input: "-(5 + 5)".to_owned(),
                expected: "(-(5 + 5));".to_owned(),
            },
            Test {
                input: "true".to_owned(),
                expected: "true;".to_owned(),
            },
            Test {
                input: "false".to_owned(),
                expected: "false;".to_owned(),
            },
            Test {
                input: "3 > 5 == false".to_owned(),
                expected: "((3 > 5) == false);".to_owned(),
            },
            Test {
                input: "3 < 5 == true".to_owned(),
                expected: "((3 < 5) == true);".to_owned(),
            },
            Test {
                input: "!(true == true)".to_owned(),
                expected: "(!(true == true));".to_owned(),
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

    #[test]
    fn test_if_expression() {
        let input = "
        if (x < y) { x };
        "
        .as_bytes()
        .to_vec();

        let l = Lexer::new(input);
        let mut parser = Parser::new(l);
        let prog = parser.parse_program();

        assert_eq!(parser.errors.len(), 0);
        assert_eq!(prog.statements.len(), 1);

        let Statement::Expression { token, expression } = &prog.statements[0] else {
            panic!("expected Statement::Expression");
        };
        assert_eq!(token.kind, If);

        let Expression::If {
            token,
            condition,
            consequence,
            alternative,
        } = expression
        else {
            panic!("expected Expression::If");
        };
        assert_eq!(token.kind, If);

        let Expression::Infix {
            token,
            operator,
            left,
            right,
        } = &**condition
        else {
            panic!("expected Expression::Infix");
        };
        assert_eq!(token.kind, Lt);
        assert_eq!(operator.kind, Lt);

        let Expression::Identifier { token } = &**left else {
            panic!("expected Expression::Identifier");
        };
        assert_eq!(token.to_string(), "x");

        let Expression::Identifier { token } = &**right else {
            panic!("expected Expression::Identifier");
        };
        assert_eq!(token.to_string(), "y");

        assert_eq!(consequence.statements.len(), 1);
        let Statement::Expression { token, expression } = &consequence.statements[0] else {
            panic!("expected Statement::Expression");
        };
        assert_eq!(token.to_string(), "x");

        let Expression::Identifier { token } = expression else {
            panic!("expected Expression::Identifier");
        };
        assert_eq!(token.to_string(), "x");

        assert!(matches!(alternative, None));
    }

    #[test]
    fn test_if_else_expression() {
        let input = "
        if (x < y) { x } else { y }
        "
        .as_bytes()
        .to_vec();

        let l = Lexer::new(input);
        let mut parser = Parser::new(l);
        let prog = parser.parse_program();

        assert_eq!(parser.errors.len(), 0);
        assert_eq!(prog.statements.len(), 1);

        let Statement::Expression { token, expression } = &prog.statements[0] else {
            panic!("expected Statement::Expression");
        };
        assert_eq!(token.kind, If);

        let Expression::If {
            token,
            condition,
            consequence,
            alternative,
        } = expression
        else {
            panic!("expected Expression::If");
        };
        assert_eq!(token.kind, If);

        let Expression::Infix {
            token,
            operator,
            left,
            right,
        } = &**condition
        else {
            panic!("expected Expression::Infix");
        };
        assert_eq!(token.kind, Lt);
        assert_eq!(operator.kind, Lt);

        let Expression::Identifier { token } = &**left else {
            panic!("expected Expression::Identifier");
        };
        assert_eq!(token.to_string(), "x");

        let Expression::Identifier { token } = &**right else {
            panic!("expected Expression::Identifier");
        };
        assert_eq!(token.to_string(), "y");

        assert_eq!(consequence.statements.len(), 1);
        let Statement::Expression { token, expression } = &consequence.statements[0] else {
            panic!("expected Statement::Expression");
        };
        assert_eq!(token.to_string(), "x");

        let Expression::Identifier { token } = expression else {
            panic!("expected Expression::Identifier");
        };
        assert_eq!(token.to_string(), "x");

        let Some(alternative) = alternative else {
            panic!("expected Some(BlockStatement)");
        };
        let Statement::Expression { token, expression } = &alternative.statements[0] else {
            panic!("expected Statement::Expression");
        };
        assert_eq!(token.to_string(), "y");

        let Expression::Identifier { token } = expression else {
            panic!("expected Expression::Identifier");
        };
        assert_eq!(token.to_string(), "y");
    }

    fn test_int_literal_expression(expr: &Expression, expected_value: i64) {
        let Expression::IntLiteral { token, value } = expr else {
            panic!("expected Expression::IntLiteral");
        };
        assert_eq!(*value, expected_value);
        assert_eq!(token.to_string(), expected_value.to_string());
    }

    fn test_bool_literal_expression(expr: &Expression, expected_value: bool) {
        let Expression::BoolLiteral { token, value } = expr else {
            panic!("expected Expression::BoolLiteral");
        };
        assert_eq!(*value, expected_value);
        assert_eq!(token.to_string(), expected_value.to_string());
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
