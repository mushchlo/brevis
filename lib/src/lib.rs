#![feature(box_patterns)]
#![feature(box_syntax)]
#![feature(destructuring_assignment)]

use crate::{
	lex::lex,
	anf::anfify_expr,
	codegen::{
		Compilation,
		compile_expr_js
	},
};
use wasm_bindgen::prelude::*;

extern crate lazy_static;
extern crate maplit;
extern crate peeking_take_while;
extern crate wasm_bindgen;
extern crate console_error_panic_hook;

mod ast;
mod cradle;
mod lex;
mod parse;
mod tok;
mod op;
mod unify;
mod anf;
mod codegen;
pub mod core;


#[wasm_bindgen]
pub fn compile_js(s: String, core_fns: &str) -> String {
    let mut lexed = lex(s);
	let mut parsed = lexed.parse();
	parsed.annotate();

	let parsed_anf = anfify_expr(parsed);
	format!("{}\n{}; buffered", core_fns, compile_expr_js(parsed_anf))
}

#[wasm_bindgen]
pub fn compile_c(s: String, core_fns: &str) -> String {
    console_error_panic_hook::set_once();
	let mut lexed = lex(s);
	let mut parsed = lexed.parse();
	parsed.annotate();

	let parsed_anf = anfify_expr(parsed);

    let mut compiler = Compilation::new();
    let compiled = compiler.compile_expr(parsed_anf);
	format!("{}\n{}\n{}\nvoid\nmain(void)\n{{\n{}\n{};\n}}", core_fns, compiler.global_defs, compiler.global, compiler.fn_context.pop().unwrap(), compiled)
}
