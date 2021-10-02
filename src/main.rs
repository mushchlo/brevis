#![feature(box_patterns)]
#![feature(box_syntax)]
#![feature(destructuring_assignment)]

use crate::{
	lex::lex,
	anf::anfify_expr,
	optimize::reduce_blocks_expr,
	codegen::{
		Compilation,
		core_fns_9,
		core_fns_posix,
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
mod optimize;
mod codegen;

fn main() {
	let core_libs = if env::args().any(|s| s == "-9") {
		core_fns_9
	} else {
		core_fns_posix
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
