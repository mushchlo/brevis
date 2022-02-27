#![feature(box_patterns)]
#![feature(box_syntax)]
#![feature(destructuring_assignment)]
#![allow(non_upper_case_globals)]

use std::collections::HashMap;
use crate::{
	lex::lex,
	codegen::{
		Compilation,
		compile_js
	},
	transform::anfify,
	error::print_error,
};
use wasm_bindgen::prelude::*;

extern crate lazy_static;
extern crate maplit;
extern crate peeking_take_while;
extern crate wasm_bindgen;
extern crate console_error_panic_hook;

pub mod core;
mod codegen;
mod error;
mod lex;
mod namespace;
mod parse;
mod transform;
mod types;
mod util;
mod verify;

#[wasm_bindgen]
pub fn web_compile(src: String, core_fns: String, backend: String, err_fn: &js_sys::Function) -> String {
	console_error_panic_hook::set_once();
	compile(&src, &core_fns, &backend, |s|
		match err_fn.call1(&JsValue::null(), &JsValue::from(s)) {
			Ok(_) => {},
			Err(e) => panic!("{:?}", e),
		}
	)
}

pub fn compile<F>(src: &str, core_fns: &str, backend: &str, err_fn: F) -> String
where F: Fn(String) {
	let mut lexed = lex(src);
	coz::progress!("done lexing");
	let mut parsed = lexed.parse();
	coz::progress!("done parsing");
	let mut errors = verify::verify(&mut parsed);
	parsed.annotate_captures();
	coz::progress!("done annotating captures");
	errors.extend(parsed.annotate());
	coz::progress!("done inferring types!");

	let tmp = if !errors.is_empty() {
		let err_count = errors.len();
		for err in errors {
			print_error(src, err, &err_fn);
		}
		err_fn(format!("Ended compilation due to the previous {} errors", err_count));
		std::process::exit(1);
	} else {
		parsed.monomorphize(&mut Vec::new(), &mut HashMap::new());
		coz::progress!("monomorphization");
		let parsed_anf = anfify(parsed);
		coz::progress!("anfification");
		match backend {
			"c" => {
				let mut compiler = Compilation::new();
				let compiled = compiler.compile(parsed_anf);
				format!("{}\n{}\n{}\n{}\nvoid\nmain(void)\n{{\n{};\n}}", core_fns, compiler.global_defs, compiler.fn_context.pop().unwrap(), compiler.global, compiled)
			}
			"js" =>
				format!("{}\n{}; buffered", core_fns, compile_js(parsed_anf)),

			_ => "".to_string()
		}
	};

	coz::progress!("compilation");

	tmp
}
