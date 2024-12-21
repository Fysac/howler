#[derive(Debug, PartialEq)]
pub enum TokenType {
    Illegal,
    EOF,
    Ident,
    Int,
    Assign,
    Eq,
    Plus,
    Comma,
    Semicolon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Function,
    Let,
}

#[derive(Debug, PartialEq)]
pub struct Token<'a> {
    type_: TokenType,
    literal: &'a str,
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

        match self.c {
            '\0' => Token {
                type_: TokenType::EOF,
                literal: "\0",
            },
            '=' => match self.lookahead() {
                '=' => {
                    self.read_char();
                    Token {
                        type_: TokenType::Eq,
                        literal: "==",
                    }
                }
                _ => Token {
                    type_: TokenType::Assign,
                    literal: "=",
                },
            },
            '+' => Token {
                type_: TokenType::Plus,
                literal: "+",
            },
            ',' => Token {
                type_: TokenType::Comma,
                literal: ",",
            },
            ';' => Token {
                type_: TokenType::Semicolon,
                literal: ";",
            },
            '(' => Token {
                type_: TokenType::LeftParen,
                literal: "(",
            },
            ')' => Token {
                type_: TokenType::RightParen,
                literal: ")",
            },
            '{' => Token {
                type_: TokenType::LeftBrace,
                literal: "{",
            },
            '}' => Token {
                type_: TokenType::RightBrace,
                literal: "}",
            },
            _ => {
                if self.c.is_alphabetic() || self.c == '_' {
                    self.read_ident()
                } else if self.c.is_numeric() {
                    self.read_num()
                } else {
                    Token {
                        type_: TokenType::Illegal,
                        literal: "\0",
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

    fn lookahead(&self) -> char {
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
                "let" => TokenType::Let,
                "fn" => TokenType::Function,
                _ => TokenType::Ident,
            },
            literal: ident,
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
                literal: "\0",
            };
        }
        let num = std::str::from_utf8(&self.input[start..self.next]).unwrap();
        Token {
            type_: TokenType::Int,
            literal: num,
        }
    }

    fn lookahead_is_ident(&self) -> bool {
        let peek = self.lookahead();
        return peek.is_alphanumeric() || peek == '_';
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
            let result = add(five, ten);"
                .as_bytes()
                .to_vec(),
        );
        let expected_tokens = vec![
            Token {
                type_: TokenType::Let,
                literal: "let",
            },
            Token {
                type_: TokenType::Ident,
                literal: "five",
            },
            Token {
                type_: TokenType::Assign,
                literal: "=",
            },
            Token {
                type_: TokenType::Int,
                literal: "5",
            },
            Token {
                type_: TokenType::Semicolon,
                literal: ";",
            },
            Token {
                type_: TokenType::Let,
                literal: "let",
            },
            Token {
                type_: TokenType::Ident,
                literal: "ten",
            },
            Token {
                type_: TokenType::Assign,
                literal: "=",
            },
            Token {
                type_: TokenType::Int,
                literal: "10",
            },
            Token {
                type_: TokenType::Semicolon,
                literal: ";",
            },
            Token {
                type_: TokenType::Let,
                literal: "let",
            },
            Token {
                type_: TokenType::Ident,
                literal: "add",
            },
            Token {
                type_: TokenType::Assign,
                literal: "=",
            },
            Token {
                type_: TokenType::Function,
                literal: "fn",
            },
            Token {
                type_: TokenType::LeftParen,
                literal: "(",
            },
            Token {
                type_: TokenType::Ident,
                literal: "x",
            },
            Token {
                type_: TokenType::Comma,
                literal: ",",
            },
            Token {
                type_: TokenType::Ident,
                literal: "y",
            },
            Token {
                type_: TokenType::RightParen,
                literal: ")",
            },
            Token {
                type_: TokenType::LeftBrace,
                literal: "{",
            },
            Token {
                type_: TokenType::Ident,
                literal: "x",
            },
            Token {
                type_: TokenType::Plus,
                literal: "+",
            },
            Token {
                type_: TokenType::Ident,
                literal: "y",
            },
            Token {
                type_: TokenType::Semicolon,
                literal: ";",
            },
            Token {
                type_: TokenType::RightBrace,
                literal: "}",
            },
            Token {
                type_: TokenType::Semicolon,
                literal: ";",
            },
            Token {
                type_: TokenType::Let,
                literal: "let",
            },
            Token {
                type_: TokenType::Ident,
                literal: "result",
            },
            Token {
                type_: TokenType::Assign,
                literal: "=",
            },
            Token {
                type_: TokenType::Ident,
                literal: "add",
            },
            Token {
                type_: TokenType::LeftParen,
                literal: "(",
            },
            Token {
                type_: TokenType::Ident,
                literal: "five",
            },
            Token {
                type_: TokenType::Comma,
                literal: ",",
            },
            Token {
                type_: TokenType::Ident,
                literal: "ten",
            },
            Token {
                type_: TokenType::RightParen,
                literal: ")",
            },
            Token {
                type_: TokenType::Semicolon,
                literal: ";",
            },
            Token {
                type_: TokenType::EOF,
                literal: "\0",
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
