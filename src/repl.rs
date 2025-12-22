use crate::ast::*;
use crate::parser::Parser;
use crate::token::*;
use std::io::{self, Write};

pub fn start() {
    loop {
        print!("\n>> ");
        io::stdout().flush().unwrap();

        let line = std::io::stdin().lines().next().unwrap().unwrap();
        let lexer = Lexer::new(line.into_bytes());
        let mut parser = Parser::new(lexer);
        match parser.parse_program() {
            Ok(prog) => {
                for stmt in prog.statements {
                    print!("{}", stmt.token_literal())
                }
            }
            Err(err) => eprintln!("parse error: {:?}", err),
        }
    }
}
