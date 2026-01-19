use std::fmt;

const CHAR_EOF: char = '\0';
const CHAR_ASSIGN: char = '=';
const CHAR_PLUS: char = '+';
const CHAR_MINUS: char = '-';
const CHAR_BANG: char = '!';
const CHAR_ASTERISK: char = '*';
const CHAR_SLASH: char = '/';
const CHAR_LT: char = '<';
const CHAR_GT: char = '>';
const CHAR_COMMA: char = ',';
const CHAR_SEMICOLON: char = ';';
const CHAR_LEFTPAREN: char = '(';
const CHAR_RIGHTPAREN: char = ')';
const CHAR_LEFTBRACE: char = '{';
const CHAR_RIGHTBRACE: char = '}';

const KEYWORD_LET: &str = "let";
const KEYWORD_FN: &str = "fn";
const KEYWORD_TRUE: &str = "true";
const KEYWORD_FALSE: &str = "false";
const KEYWORD_IF: &str = "if";
const KEYWORD_ELSE: &str = "else";
const KEYWORD_RETURN: &str = "return";

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Illegal(String),
    EOF,
    Ident(String),
    Int(String),
    Assign,
    Eq,
    NotEq,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Comma,
    Semicolon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}
impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenKind::EOF => CHAR_EOF.to_string(),
                TokenKind::Ident(literal)
                | TokenKind::Int(literal)
                | TokenKind::Illegal(literal) => literal.clone(),
                TokenKind::Assign => CHAR_ASSIGN.to_string(),
                TokenKind::Eq => format!("{}{}", CHAR_ASSIGN, CHAR_ASSIGN),
                TokenKind::NotEq => format!("{}{}", CHAR_BANG, CHAR_ASSIGN),
                TokenKind::Plus => CHAR_PLUS.to_string(),
                TokenKind::Minus => CHAR_MINUS.to_string(),
                TokenKind::Bang => CHAR_BANG.to_string(),
                TokenKind::Asterisk => CHAR_ASTERISK.to_string(),
                TokenKind::Slash => CHAR_SLASH.to_string(),
                TokenKind::Lt => CHAR_LT.to_string(),
                TokenKind::Gt => CHAR_GT.to_string(),
                TokenKind::Comma => CHAR_COMMA.to_string(),
                TokenKind::Semicolon => CHAR_SEMICOLON.to_string(),
                TokenKind::LeftParen => CHAR_LEFTPAREN.to_string(),
                TokenKind::RightParen => CHAR_RIGHTPAREN.to_string(),
                TokenKind::LeftBrace => CHAR_LEFTBRACE.to_string(),
                TokenKind::RightBrace => CHAR_RIGHTBRACE.to_string(),
                TokenKind::Function => KEYWORD_FN.to_string(),
                TokenKind::Let => KEYWORD_LET.to_string(),
                TokenKind::True => KEYWORD_TRUE.to_string(),
                TokenKind::False => KEYWORD_FALSE.to_string(),
                TokenKind::If => KEYWORD_IF.to_string(),
                TokenKind::Else => KEYWORD_ELSE.to_string(),
                TokenKind::Return => KEYWORD_RETURN.to_string(),
            }
        )
    }
}

#[derive(PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    // TODO: add line and column numbers
}
impl Token {
    // Returns true if the token is the specified kind (ignoring any associated data), false otherwise
    pub fn is_kind(&self, kind: &TokenKind) -> bool {
        std::mem::discriminant(&self.kind) == std::mem::discriminant(&kind)
    }
}
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.kind.fmt(f)
    }
}

pub struct Lexer {
    input: Vec<u8>,
    c: char,
    cur: usize,
    next: usize,
}

impl Lexer {
    pub fn new(input: Vec<u8>) -> Self {
        Lexer {
            input,
            c: '\0',
            cur: 0,
            next: 0,
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.read_char();

        if self.c.is_whitespace() {
            self.consume_whitespace();
        }

        let kind = match self.c {
            CHAR_EOF => TokenKind::EOF,
            CHAR_ASSIGN => match self.lookahead() {
                CHAR_ASSIGN => {
                    self.read_char();
                    TokenKind::Eq
                }
                _ => TokenKind::Assign,
            },
            CHAR_PLUS => TokenKind::Plus,
            CHAR_MINUS => TokenKind::Minus,
            CHAR_BANG => match self.lookahead() {
                '=' => {
                    self.read_char();
                    TokenKind::NotEq
                }
                _ => TokenKind::Bang,
            },
            CHAR_ASTERISK => TokenKind::Asterisk,
            CHAR_SLASH => TokenKind::Slash,
            CHAR_LT => TokenKind::Lt,
            CHAR_GT => TokenKind::Gt,
            CHAR_COMMA => TokenKind::Comma,
            CHAR_SEMICOLON => TokenKind::Semicolon,
            CHAR_LEFTPAREN => TokenKind::LeftParen,
            CHAR_RIGHTPAREN => TokenKind::RightParen,
            CHAR_LEFTBRACE => TokenKind::LeftBrace,
            CHAR_RIGHTBRACE => TokenKind::RightBrace,
            _ => {
                return if self.c.is_alphabetic() || self.c == '_' {
                    self.read_ident()
                } else if self.c.is_numeric() {
                    self.read_num()
                } else {
                    Token {
                        kind: TokenKind::Illegal(self.c.to_string()),
                    }
                }
            }
        };
        Token { kind }
    }

    fn read_char(&mut self) {
        if self.next >= self.input.len() {
            self.c = '\0';
            return;
        }
        self.c = self.input[self.next] as char;
        if self.next > 0 {
            // only advance cur if we've already read the first char
            self.cur += 1;
        }
        self.next += 1;
    }

    pub fn lookahead(&self) -> char {
        if self.next >= self.input.len() {
            return '\0';
        }
        self.input[self.next] as char
    }

    fn consume_whitespace(&mut self) {
        while self.c.is_whitespace() {
            self.read_char();
        }
    }

    fn read_ident(&mut self) -> Token {
        let start = self.cur;
        while self.lookahead_is_ident() {
            self.read_char();
        }
        let ident = std::str::from_utf8(&self.input[start..self.next]).unwrap();
        let kind = match ident {
            KEYWORD_LET => TokenKind::Let,
            KEYWORD_FN => TokenKind::Function,
            KEYWORD_TRUE => TokenKind::True,
            KEYWORD_FALSE => TokenKind::False,
            KEYWORD_IF => TokenKind::If,
            KEYWORD_ELSE => TokenKind::Else,
            KEYWORD_RETURN => TokenKind::Return,
            _ => TokenKind::Ident(ident.to_string()),
        };
        Token { kind }
    }

    fn read_num(&mut self) -> Token {
        let start = self.cur;
        while self.lookahead().is_numeric() {
            self.read_char();
        }
        if self.lookahead_is_ident() {
            // bad news: identifier character following number
            // consume the rest of the illegal token
            self.read_ident();
            return Token {
                kind: TokenKind::Illegal(
                    std::str::from_utf8(&self.input[start..self.next])
                        .unwrap()
                        .to_owned(),
                ),
            };
        }
        let num = std::str::from_utf8(&self.input[start..self.next]).unwrap();
        Token {
            kind: TokenKind::Int(num.to_string()),
        }
    }

    fn lookahead_is_ident(&self) -> bool {
        let peek = self.lookahead();
        peek.is_alphanumeric() || peek == '_'
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let mut lexer = Lexer::new(
            "let five = 5;
            let ten = 10;

            let add = fn(x, y) {
                x + y;
            };

            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;"
                .as_bytes()
                .to_vec(),
        );
        let expected_kinds = vec![
            TokenKind::Let,
            TokenKind::Ident("five".to_owned()),
            TokenKind::Assign,
            TokenKind::Int("5".to_owned()),
            TokenKind::Semicolon,
            TokenKind::Let,
            TokenKind::Ident("ten".to_owned()),
            TokenKind::Assign,
            TokenKind::Int("10".to_owned()),
            TokenKind::Semicolon,
            TokenKind::Let,
            TokenKind::Ident("add".to_owned()),
            TokenKind::Assign,
            TokenKind::Function,
            TokenKind::LeftParen,
            TokenKind::Ident("x".to_owned()),
            TokenKind::Comma,
            TokenKind::Ident("y".to_owned()),
            TokenKind::RightParen,
            TokenKind::LeftBrace,
            TokenKind::Ident("x".to_owned()),
            TokenKind::Plus,
            TokenKind::Ident("y".to_owned()),
            TokenKind::Semicolon,
            TokenKind::RightBrace,
            TokenKind::Semicolon,
            TokenKind::Let,
            TokenKind::Ident("result".to_owned()),
            TokenKind::Assign,
            TokenKind::Ident("add".to_owned()),
            TokenKind::LeftParen,
            TokenKind::Ident("five".to_owned()),
            TokenKind::Comma,
            TokenKind::Ident("ten".to_owned()),
            TokenKind::RightParen,
            TokenKind::Semicolon,
            TokenKind::Bang,
            TokenKind::Minus,
            TokenKind::Slash,
            TokenKind::Asterisk,
            TokenKind::Int("5".to_owned()),
            TokenKind::Semicolon,
            TokenKind::Int("5".to_owned()),
            TokenKind::Lt,
            TokenKind::Int("10".to_owned()),
            TokenKind::Gt,
            TokenKind::Int("5".to_owned()),
            TokenKind::Semicolon,
            TokenKind::If,
            TokenKind::LeftParen,
            TokenKind::Int("5".to_owned()),
            TokenKind::Lt,
            TokenKind::Int("10".to_owned()),
            TokenKind::RightParen,
            TokenKind::LeftBrace,
            TokenKind::Return,
            TokenKind::True,
            TokenKind::Semicolon,
            TokenKind::RightBrace,
            TokenKind::Else,
            TokenKind::LeftBrace,
            TokenKind::Return,
            TokenKind::False,
            TokenKind::Semicolon,
            TokenKind::RightBrace,
            TokenKind::Int("10".to_owned()),
            TokenKind::Eq,
            TokenKind::Int("10".to_owned()),
            TokenKind::Semicolon,
            TokenKind::Int("10".to_owned()),
            TokenKind::NotEq,
            TokenKind::Int("9".to_owned()),
            TokenKind::Semicolon,
        ];

        let mut i = 0;
        let mut token = lexer.next_token();
        while !matches!(token.kind, TokenKind::EOF) {
            // Compare kinds, ignoring associated data (to test is_kind)
            assert!(token.is_kind(&expected_kinds[i]));

            // Compare kinds and associated data
            assert_eq!(token.kind, expected_kinds[i]);
            i += 1;
            token = lexer.next_token();
        }
    }
}
