extern crate maplit;
extern crate lazy_static;

use lazy_static::lazy_static;
use maplit::hashmap;
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::{
	anf::{
		*, ANFExprVal::*,
	},
	ast::{
		Type,
		Type::*,
	},
	lex::{
		TokenLiteral,
		TokenLiteral::*,
	},
	tok::{
		OpID,
		OpID::*,
	},
};

static LAMBDA_COUNTER: AtomicUsize = AtomicUsize::new(0);

lazy_static! {
	static ref C_OP: HashMap<OpID, &'static str> =
		hashmap!{
			Eq => "=",
			Add => "+",
			Sub => "-",
			Mul => "*",
			Div => "/",
			Gt => ">",
			Lt => "<",
			Gteq => ">=",
			Lteq => "<=",
			Doeq => "==",
			Noteq => "!=",
			And => "&&",
			Or => "||"
		};
}

pub const CORE_FNS_9: &str =
r#"#include <u.h>
#include <libc.h>

void _print(char* s){ print("%s", s); }

char*
_itoa(vlong val)
{
	smprint("%lld", val);
}"#;

pub const CORE_FNS_POSIX: &str =
r#"#include <stdio.h>
#include <stdlib.h>

void _print(char* s){ printf("%s", s); }

char*
_itoa(long long int val)
{
	static char buf[32] = {0};
	int i = 30;

    if(val == 0){
        strcpy(buf, "0");
        return buf;
    }
	for(; val && i; --i, val /= 10)
		buf[i] = "0123456789"[val % 10];

	return &buf[i+1];
}
"#;

fn lambda_name() -> String {
	format!("lambda_{}", LAMBDA_COUNTER.fetch_add(1, Ordering::SeqCst))
}

pub struct Compilation {
	pub global: String,
	pub fn_context: Vec<String>
}

impl Compilation {
	pub fn new() -> Self {
		Compilation {
			global: "".to_string(),
			fn_context: vec!["".to_string()]
		}
	}

	pub fn compile(&mut self, a: ANFAST) -> String {
		match a {
			ANFAST::LetNode(l) => {
				let c_type_name =
					match l.var.r#type {
						TypeConstructor(mut tc) if tc.name == "Function" => {
							let ret_t = tc.args.pop().unwrap();
							let args_t = tc.args.iter()
												.map(|t| compile_type(t.clone()))
												.reduce(|acc, next| acc + ", " + &next)
												.unwrap_or("".to_string());
							if let Some(box ANFExpr {
								val: LambdaNode(lambda),
								..
							}) = l.def {
								self.compile_lambda(lambda, mk_id(l.var.name));
								return "".to_string();
							}
							format!("{} (*{})({})",
								compile_type(ret_t),
								mk_id(l.var.name.clone()),
								args_t
							)
						}

						t => format!("{} {}", compile_type(t), mk_id(l.var.name.clone()))
					};
				let def_str =
					if let Some(box expr) = l.def {
						format!("{} = {}", mk_id(l.var.name), self.compile_expr(expr))
					} else {
						"".to_string()
					};
				let prev_context = self.fn_context.pop().unwrap();
				self.fn_context.push(
					format!("{}{};\n", prev_context, c_type_name)
				);
				format!("{}\n", def_str)
			}

			ANFAST::ExprNode(e) => 
				format!("({})", self.compile_expr(e))
		}
	}

	fn compile_lambda(&mut self, l: ANFLambda, mut name: String) -> String {
		
		let return_t = l.body.r#type.clone();
		let args_str = l.args.iter()
							.map(|v| compile_type(v.r#type.clone()) + " " + &mk_id(v.name.clone()))
							.reduce(|acc, next| acc + ", " + &next)
							.unwrap_or_else(|| "void".to_string());
		if name.is_empty() {
			 name = lambda_name();
		}
		self.fn_context.push("".to_string());
		let c_body = self.compile_expr(*l.body);
		let declarations = self.fn_context.pop();
		self.global +=
			&format!("{}\n{}({})\n{{\n{}{}{};\n}}\n",
				compile_type(return_t.clone()),
				name,
				args_str,
				declarations.unwrap(),

				if return_t != Void {
					"return "
				} else { "" },

				c_body
			);
		name
	}
		

	pub fn compile_expr(&mut self, e: ANFExpr) -> String {
		match e.val {
			LiteralNode(lit) =>
				compile_literal(lit),

			IdentNode(id) => mk_id(id),

			BlockNode(b) => format!("({})", b.iter()
												.filter_map(|box l| {
													let res = self.compile(l.clone());
													if res.clone().trim().is_empty() {
														None
													} else {
														Some(res)
													}
												})
												.reduce(|acc, next|
															acc.clone() + if !acc.is_empty() { ", " } else { "" } + &next)
												.unwrap()
							),

			LambdaNode(l) => self.compile_lambda(l, "".to_string()),

			IfNode(ifelse) =>
				format!("({}) ? ({}) : ({})",
						self.compile_expr(*ifelse.cond),
						self.compile_expr(*ifelse.then),
						if let Some(box expr) = ifelse.r#else {
							self.compile_expr(expr)
						} else {
							"0".to_string()
						}
				),

			UnaryNode(u) => {
				let c_op = match u.op {
					Not => "!",
					Minus => "-",
					_ => panic!("unary is not unary!")
				};

				format!("{}({})", c_op, self.compile_expr(*u.expr))
			}

			BinaryNode(b) => {
				let (c_left, c_right) = (
					self.compile_expr(*b.left),
					self.compile_expr(*b.right)
				);
				if e.r#type == Str {
					match b.op {
						Eq => format!("{} = {}", c_left, c_right),
						Doeq | Noteq =>
							format!("{}strcmp({}, {})",
								if b.op == Doeq { "!" } else { "" },
								c_left,
								c_right
							),
						_ => panic!("operator {:?} does not apply to strings", b.op)
					}
				} else if b.op == Xor {
					format!("!({}) != !({})", c_left, c_right)
				} else {
					format!("({}) {} ({})", c_left, C_OP[&b.op], c_right)
				}
			}

			CallNode(c) => {
				let args = c.args.iter()
									.map(|tr| compile_trivial(tr.clone()))
									.reduce(|acc, next| acc + ", " + &next)
									.unwrap_or_else(|| "".to_string());
				format!("({})({})", self.compile_expr(*c.func), args)
			}
		}
	}
}

fn compile_literal(lit: TokenLiteral) -> String {
	match lit {
		IntLit(i) => format!("{}", i),
		FltLit(f) => format!("{}", f),
		StrLit(s) => format!("\"{}\"", s),
		BoolLit(b) => (if b {"1"} else {"0"}).to_string()
	}
}

fn compile_trivial(tr: Trivial) -> String {
	match tr {
		Trivial::Literal(lit) => compile_literal(lit),
		Trivial::Ident(id) => mk_id(id)
	}
}

fn compile_type(t: Type) -> String {
	match t {
		Void => "void",
		Int => "long long int",
		Float => "long double",
		Str => "char*",
		Bool => "char",
		_ => panic!("aaaa i cant make type {:#?}", t)
	}.to_string()
}

fn mk_id(s: String) -> String {
// TODO: not all genlang names are valid C names! fix!
	format!("_{}", s)
}
