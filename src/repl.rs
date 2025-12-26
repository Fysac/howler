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
        let prog = parser.parse_program();
        if parser.errors.len() > 0 {
            for e in parser.errors {
                eprintln!("{}", e)
            }
            continue;
        }
        for stmt in prog.statements {
            println!("{}", stmt.token_literal())
        }
    }
}
