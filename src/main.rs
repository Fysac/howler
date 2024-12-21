mod token;

fn main() {
    let mut lexer = token::Lexer::new("let x = 1;".as_bytes().to_vec());
    lexer.next_token();
}
