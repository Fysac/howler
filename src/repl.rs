use crate::token::{self, Lexer};
use std::io::{self, Write};

pub fn start() {
    loop {
        print!("\n>> ");
        io::stdout().flush().unwrap();

        let line = std::io::stdin().lines().next().unwrap().unwrap();
        let mut lexer = Lexer::new(line.into_bytes());

        let mut t = lexer.next_token();
        while t.type_ != token::TokenType::EOF {
            print!("{:?}\n", t);
            t = lexer.next_token();
        }
    }
}
