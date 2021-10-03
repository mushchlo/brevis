#![feature(box_patterns)]
#![feature(box_syntax)]
#![feature(destructuring_assignment)]

use crate::{
	lex::lex,
	anf::anfify_expr,
	codegen::Compilation,
	core::{
		CORE_FNS_9,
		CORE_FNS_POSIX,
	},
};
use std::{
	fs,
	env,
};

mod ast;
mod cradle;
mod lex;
mod parse;
mod tok;
mod unify;
mod anf;
mod codegen;
mod core;

fn main() {
	let core_libs = if env::args().any(|s| s == "-9") {
		CORE_FNS_9
	} else {
		CORE_FNS_POSIX
	};

	let contents = fs::read_to_string("test.bv").expect("failed to open/read file");
	let mut lexed = lex(contents);
	let mut parsed = lexed.parse();
	parsed.annotate();

	let parsed_anf = anfify_expr(parsed);

	let mut compiler = Compilation::new();
	let compiled = compiler.compile_expr(parsed_anf);
	println!("{}\n{}\nvoid\nmain(void)\n{{\n{}\n{};\n}}", core_libs, compiler.global, compiler.fn_context.pop().unwrap(), compiled);
}
