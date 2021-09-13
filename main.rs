use std::fs;
use std::collections::VecDeque;

include!("lex.rs");
include!("parse.rs");
include!("unify.rs");

fn main() {
	let contents = fs::read_to_string("test.gl").expect("failed to open/read file");
	let mut lexed = lex(contents);
	let mut parsed = lexed.parse();
	parsed.annotate();

	println!("{:#?}", parsed);
}