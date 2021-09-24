#![feature(box_patterns)]
#![feature(box_syntax)]

use crate::{
	lex::lex,
	anf::anfify_expr,
};
use std::fs;

mod ast;
mod cradle;
mod lex;
mod parse;
mod tok;
mod unify;
mod anf;

fn main() {
	let contents = fs::read_to_string("test.br").expect("failed to open/read file");
	let mut lexed = lex(contents);
	let mut parsed = lexed.parse();
	parsed.annotate();

	let parsed_anf = anfify_expr(parsed);
	println!("{:#?}", parsed_anf);
}
