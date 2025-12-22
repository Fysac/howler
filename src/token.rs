#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Illegal,
    EOF,
    Ident,
    Int,
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

impl TokenType {
    pub fn to_string(&self) -> String {
        match *self {
            TokenType::EOF => CHAR_EOF.to_string(),
            TokenType::Assign => CHAR_ASSIGN.to_string(),
            TokenType::Eq => format!("{}{}", CHAR_ASSIGN, CHAR_ASSIGN),
            TokenType::NotEq => format!("{}{}", CHAR_BANG, CHAR_ASSIGN),
            TokenType::Plus => CHAR_PLUS.to_string(),
            TokenType::Minus => CHAR_MINUS.to_string(),
            TokenType::Bang => CHAR_BANG.to_string(),
            TokenType::Asterisk => CHAR_ASTERISK.to_string(),
            TokenType::Slash => CHAR_SLASH.to_string(),
            TokenType::Lt => CHAR_LT.to_string(),
            TokenType::Gt => CHAR_GT.to_string(),
            TokenType::Comma => CHAR_COMMA.to_string(),
            TokenType::Semicolon => CHAR_SEMICOLON.to_string(),
            TokenType::LeftParen => CHAR_LEFTPAREN.to_string(),
            TokenType::RightParen => CHAR_RIGHTPAREN.to_string(),
            TokenType::LeftBrace => CHAR_LEFTBRACE.to_string(),
            TokenType::RightBrace => CHAR_RIGHTBRACE.to_string(),
            TokenType::Function => KEYWORD_FN.to_string(),
            TokenType::Let => KEYWORD_LET.to_string(),
            TokenType::True => KEYWORD_TRUE.to_string(),
            TokenType::False => KEYWORD_FALSE.to_string(),
            TokenType::If => KEYWORD_IF.to_string(),
            TokenType::Else => KEYWORD_ELSE.to_string(),
            TokenType::Return => KEYWORD_RETURN.to_string(),
            _ => panic!("no default string representation for {:?}", *self),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub type_: TokenType,
    pub literal: String,
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

        let cs = self.c.to_string();
        match self.c {
            CHAR_EOF => Token {
                type_: TokenType::EOF,
                literal: cs,
            },
            CHAR_ASSIGN => match self.lookahead() {
                CHAR_ASSIGN => {
                    self.read_char();
                    Token {
                        type_: TokenType::Eq,
                        literal: TokenType::Eq.to_string(),
                    }
                }
                _ => Token {
                    type_: TokenType::Assign,
                    literal: cs,
                },
            },
            CHAR_PLUS => Token {
                type_: TokenType::Plus,
                literal: cs,
            },
            CHAR_MINUS => Token {
                type_: TokenType::Minus,
                literal: cs,
            },
            CHAR_BANG => match self.lookahead() {
                '=' => {
                    self.read_char();
                    Token {
                        type_: TokenType::NotEq,
                        literal: TokenType::NotEq.to_string(),
                    }
                }
                _ => Token {
                    type_: TokenType::Bang,
                    literal: cs,
                },
            },
            CHAR_ASTERISK => Token {
                type_: TokenType::Asterisk,
                literal: cs,
            },
            CHAR_SLASH => Token {
                type_: TokenType::Slash,
                literal: cs,
            },
            CHAR_LT => Token {
                type_: TokenType::Lt,
                literal: cs,
            },
            CHAR_GT => Token {
                type_: TokenType::Gt,
                literal: cs,
            },
            CHAR_COMMA => Token {
                type_: TokenType::Comma,
                literal: cs,
            },
            CHAR_SEMICOLON => Token {
                type_: TokenType::Semicolon,
                literal: cs,
            },
            CHAR_LEFTPAREN => Token {
                type_: TokenType::LeftParen,
                literal: cs,
            },
            CHAR_RIGHTPAREN => Token {
                type_: TokenType::RightParen,
                literal: cs,
            },
            CHAR_LEFTBRACE => Token {
                type_: TokenType::LeftBrace,
                literal: cs,
            },
            CHAR_RIGHTBRACE => Token {
                type_: TokenType::RightBrace,
                literal: cs,
            },
            _ => {
                if self.c.is_alphabetic() || self.c == '_' {
                    self.read_ident()
                } else if self.c.is_numeric() {
                    self.read_num()
                } else {
                    Token {
                        type_: TokenType::Illegal,
                        literal: String::from("\0"),
                    }
                }
            }
        }
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
        Token {
            type_: match ident {
                KEYWORD_LET => TokenType::Let,
                KEYWORD_FN => TokenType::Function,
                KEYWORD_TRUE => TokenType::True,
                KEYWORD_FALSE => TokenType::False,
                KEYWORD_IF => TokenType::If,
                KEYWORD_ELSE => TokenType::Else,
                KEYWORD_RETURN => TokenType::Return,
                _ => TokenType::Ident,
            },
            literal: ident.to_string(),
        }
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
                type_: TokenType::Illegal,
                literal: "\0".to_string(),
            };
        }
        let num = std::str::from_utf8(&self.input[start..self.next]).unwrap();
        Token {
            type_: TokenType::Int,
            literal: num.to_string(),
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
        let expected_tokens = vec![
            Token {
                type_: TokenType::Let,
                literal: TokenType::Let.to_string(),
            },
            Token {
                type_: TokenType::Ident,
                literal: "five".to_string(),
            },
            Token {
                type_: TokenType::Assign,
                literal: TokenType::Assign.to_string(),
            },
            Token {
                type_: TokenType::Int,
                literal: "5".to_string(),
            },
            Token {
                type_: TokenType::Semicolon,
                literal: TokenType::Semicolon.to_string(),
            },
            Token {
                type_: TokenType::Let,
                literal: TokenType::Let.to_string(),
            },
            Token {
                type_: TokenType::Ident,
                literal: "ten".to_string(),
            },
            Token {
                type_: TokenType::Assign,
                literal: TokenType::Assign.to_string(),
            },
            Token {
                type_: TokenType::Int,
                literal: "10".to_string(),
            },
            Token {
                type_: TokenType::Semicolon,
                literal: TokenType::Semicolon.to_string(),
            },
            Token {
                type_: TokenType::Let,
                literal: TokenType::Let.to_string(),
            },
            Token {
                type_: TokenType::Ident,
                literal: "add".to_string(),
            },
            Token {
                type_: TokenType::Assign,
                literal: TokenType::Assign.to_string(),
            },
            Token {
                type_: TokenType::Function,
                literal: TokenType::Function.to_string(),
            },
            Token {
                type_: TokenType::LeftParen,
                literal: TokenType::LeftParen.to_string(),
            },
            Token {
                type_: TokenType::Ident,
                literal: "x".to_string(),
            },
            Token {
                type_: TokenType::Comma,
                literal: TokenType::Comma.to_string(),
            },
            Token {
                type_: TokenType::Ident,
                literal: "y".to_string(),
            },
            Token {
                type_: TokenType::RightParen,
                literal: TokenType::RightParen.to_string(),
            },
            Token {
                type_: TokenType::LeftBrace,
                literal: TokenType::LeftBrace.to_string(),
            },
            Token {
                type_: TokenType::Ident,
                literal: "x".to_string(),
            },
            Token {
                type_: TokenType::Plus,
                literal: TokenType::Plus.to_string(),
            },
            Token {
                type_: TokenType::Ident,
                literal: "y".to_string(),
            },
            Token {
                type_: TokenType::Semicolon,
                literal: TokenType::Semicolon.to_string(),
            },
            Token {
                type_: TokenType::RightBrace,
                literal: TokenType::RightBrace.to_string(),
            },
            Token {
                type_: TokenType::Semicolon,
                literal: TokenType::Semicolon.to_string(),
            },
            Token {
                type_: TokenType::Let,
                literal: TokenType::Let.to_string(),
            },
            Token {
                type_: TokenType::Ident,
                literal: "result".to_string(),
            },
            Token {
                type_: TokenType::Assign,
                literal: TokenType::Assign.to_string(),
            },
            Token {
                type_: TokenType::Ident,
                literal: "add".to_string(),
            },
            Token {
                type_: TokenType::LeftParen,
                literal: TokenType::LeftParen.to_string(),
            },
            Token {
                type_: TokenType::Ident,
                literal: "five".to_string(),
            },
            Token {
                type_: TokenType::Comma,
                literal: TokenType::Comma.to_string(),
            },
            Token {
                type_: TokenType::Ident,
                literal: "ten".to_string(),
            },
            Token {
                type_: TokenType::RightParen,
                literal: TokenType::RightParen.to_string(),
            },
            Token {
                type_: TokenType::Semicolon,
                literal: TokenType::Semicolon.to_string(),
            },
            Token {
                type_: TokenType::Bang,
                literal: TokenType::Bang.to_string(),
            },
            Token {
                type_: TokenType::Minus,
                literal: TokenType::Minus.to_string(),
            },
            Token {
                type_: TokenType::Slash,
                literal: TokenType::Slash.to_string(),
            },
            Token {
                type_: TokenType::Asterisk,
                literal: TokenType::Asterisk.to_string(),
            },
            Token {
                type_: TokenType::Int,
                literal: "5".to_string(),
            },
            Token {
                type_: TokenType::Semicolon,
                literal: TokenType::Semicolon.to_string(),
            },
            Token {
                type_: TokenType::Int,
                literal: "5".to_string(),
            },
            Token {
                type_: TokenType::Lt,
                literal: TokenType::Lt.to_string(),
            },
            Token {
                type_: TokenType::Int,
                literal: "10".to_string(),
            },
            Token {
                type_: TokenType::Gt,
                literal: TokenType::Gt.to_string(),
            },
            Token {
                type_: TokenType::Int,
                literal: "5".to_string(),
            },
            Token {
                type_: TokenType::Semicolon,
                literal: TokenType::Semicolon.to_string(),
            },
            Token {
                type_: TokenType::If,
                literal: TokenType::If.to_string(),
            },
            Token {
                type_: TokenType::LeftParen,
                literal: TokenType::LeftParen.to_string(),
            },
            Token {
                type_: TokenType::Int,
                literal: "5".to_string(),
            },
            Token {
                type_: TokenType::Lt,
                literal: TokenType::Lt.to_string(),
            },
            Token {
                type_: TokenType::Int,
                literal: "10".to_string(),
            },
            Token {
                type_: TokenType::RightParen,
                literal: TokenType::RightParen.to_string(),
            },
            Token {
                type_: TokenType::LeftBrace,
                literal: TokenType::LeftBrace.to_string(),
            },
            Token {
                type_: TokenType::Return,
                literal: TokenType::Return.to_string(),
            },
            Token {
                type_: TokenType::True,
                literal: TokenType::True.to_string(),
            },
            Token {
                type_: TokenType::Semicolon,
                literal: TokenType::Semicolon.to_string(),
            },
            Token {
                type_: TokenType::RightBrace,
                literal: TokenType::RightBrace.to_string(),
            },
            Token {
                type_: TokenType::Else,
                literal: TokenType::Else.to_string(),
            },
            Token {
                type_: TokenType::LeftBrace,
                literal: TokenType::LeftBrace.to_string(),
            },
            Token {
                type_: TokenType::Return,
                literal: TokenType::Return.to_string(),
            },
            Token {
                type_: TokenType::False,
                literal: TokenType::False.to_string(),
            },
            Token {
                type_: TokenType::Semicolon,
                literal: TokenType::Semicolon.to_string(),
            },
            Token {
                type_: TokenType::RightBrace,
                literal: TokenType::RightBrace.to_string(),
            },
            Token {
                type_: TokenType::Int,
                literal: "10".to_string(),
            },
            Token {
                type_: TokenType::Eq,
                literal: TokenType::Eq.to_string(),
            },
            Token {
                type_: TokenType::Int,
                literal: "10".to_string(),
            },
            Token {
                type_: TokenType::Semicolon,
                literal: TokenType::Semicolon.to_string(),
            },
            Token {
                type_: TokenType::Int,
                literal: "10".to_string(),
            },
            Token {
                type_: TokenType::NotEq,
                literal: TokenType::NotEq.to_string(),
            },
            Token {
                type_: TokenType::Int,
                literal: "9".to_string(),
            },
            Token {
                type_: TokenType::Semicolon,
                literal: TokenType::Semicolon.to_string(),
            },
        ];

        let mut i = 0;
        let mut token = lexer.next_token();
        while token.type_ != TokenType::EOF {
            assert_eq!(token, expected_tokens[i]);
            i += 1;
            token = lexer.next_token();
        }
    }
}
