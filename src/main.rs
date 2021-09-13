use crate::lex::lex;
use std::fs;

mod ast;
mod cradle;
mod lex;
mod parse;
mod tok;
mod unify;

fn main() {
    let contents = fs::read_to_string("test.gl").expect("failed to open/read file");
    let mut lexed = lex(contents);
    let mut parsed = lexed.parse();
    parsed.annotate();

    println!("{:#?}", parsed);
}
