#![feature(box_patterns)]
#![feature(box_syntax)]
#![feature(destructuring_assignment)]
#![allow(non_upper_case_globals)]

use std::collections::HashMap;
use crate::{
	lex::lex,
	codegen::{
		Compilation,
		compile_expr_js
	},
	transform::anfify_expr,
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
mod parse;
mod transform;
mod types;

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
	let mut parsed = lexed.parse();
	let errors = parsed.annotate();

	if !errors.is_empty() {
		let err_count = errors.len();
		for err in errors {
			print_error(src, err, &err_fn);
		}
		err_fn(format!("Ended compilation due to the previous {} errors", err_count));
		std::process::exit(1);
	} else {
println!("{:#?}", parsed);
		parsed.monomorphize(&mut Vec::new(), &mut HashMap::new());
println!("{:#?}", parsed);
		let parsed_anf = anfify_expr(parsed);
//println!("{:#?}", parsed_anf);
		match backend {
			"c" => {
				let mut compiler = Compilation::new();
				let compiled = compiler.compile_expr(parsed_anf);
				format!("{}\n{}\n{}\n{}\nvoid\nmain(void)\n{{\n{};\n}}", core_fns, compiler.global_defs, compiler.fn_context.pop().unwrap(), compiler.global, compiled)
			}
			"js" =>
				format!("{}\n{}; buffered", core_fns, compile_expr_js(parsed_anf)),

			_ => "".to_string()
		}
	}

}
/*
#[wasm_bindgen]
pub fn js_compile(s: String, core_fns: String, backend: String, err_fn: &js_sys::Function) -> String {
	console_error_panic_hook::set_once();
	let mut lexed = lex(&s);
	let mut parsed = lexed.parse();
	let errors = parsed.annotate();

	if !errors.is_empty() {
		let err_count = errors.len();
		for err in errors {
			print_error(&s, err, |e| {
				match err_fn.call1(&JsValue::null(), &JsValue::from(e)) {
					Ok(_) => {},
					Err(_) => panic!(),
				}
			});
		}
		match err_fn.call1(&JsValue::null(), &JsValue::from(format!("Ended compilation due to the previous {} errors", err_count))) {
			Ok(_) => {},
			Err(_) => unreachable!(),
		};
		"".to_string()
	} else {
		parsed.monomorphize(&mut Vec::new(), &mut HashMap::new());
		let mut parsed_anf = anfify_expr(parsed);
		match backend.as_str() {
			"c" => {
				let mut compiler = Compilation::new();
				let compiled = compiler.compile_expr(parsed_anf);
				format!("{}\n{}\n{}\n{}\nvoid\nmain(void)\n{{\n{};\n}}", core_fns, compiler.global_defs, compiler.fn_context.pop().unwrap(), compiler.global, compiled)
			}
			"js" =>
				format!("{}\n{}; buffered", core_fns, compile_expr_js(parsed_anf)),

			_ => "".to_string()
		}
	}
}
*/