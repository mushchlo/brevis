use lazy_static::lazy_static;
use maplit::hashmap;

use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};

use anf::{
	*, ANFExprVal::*,
};
use ast::{
	Type,
	Type::*,
};
use lex::{
	TokenLiteral,
	TokenLiteral::*,
};
use tok::{
	OpID,
	OpID::*,
};

static LAMBDA_COUNTER: AtomicUsize = AtomicUsize::new(0);

lazy_static! {
	static ref C_OP: HashMap<OpID, &'static str> =
		hashmap!{
			Eq => "=",
			Add => "+",
			Sub => "-",
			Mul => "*",
			Mod => "%",
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
	static ref PY_OP: HashMap<OpID, &'static str> = {
			let mut tmp = C_OP.clone();
			tmp.insert(Eq, ":=");
			tmp.insert(And, "and");
			tmp.insert(Or, "or");
			tmp.insert(Concat, "+");
			tmp
		};
	static ref JS_OP: HashMap<OpID, &'static str> = {
			let mut tmp = C_OP.clone();
			tmp.insert(Concat, "+");
			tmp
		};
}

fn lambda_name() -> String {
	format!("lambda_{}", LAMBDA_COUNTER.fetch_add(1, Ordering::SeqCst))
}

pub fn compile_js(a: ANFAST) -> String {
	match a {
		ANFAST::LetNode(l) =>
			format!("({} = {})",
				mk_id(l.var.name),
				if let Some(box def) = l.def {
					compile_expr_js(def)
				} else {
					"false".to_string()
				}
			),
		ANFAST::ExprNode(e) =>
			compile_expr_js(e)
	}
}

pub fn compile_expr_js(e: ANFExpr) -> String {
	format!("({})", match e.val {
		ANFExprVal::LambdaNode(l) =>
			format!("(function ({}) {{ return {}; }})",
				l.args.iter()
					.map(|v| mk_id(v.name.clone()))
					.reduce(|acc, next| acc + ", " + &next)
					.unwrap_or_else(|| "".to_string()),
				compile_expr_js(*l.body)
			),
		ANFExprVal::LiteralNode(lit) =>
			compile_literal(lit),
		ANFExprVal::IdentNode(id) =>
			mk_id(id),
		ANFExprVal::BlockNode(b) =>
			format!("({})",
				b.iter()
					.map(|box line| compile_js(line.clone()))
					.reduce(|acc, next| acc + "," + &next)
					.unwrap_or_else(|| "".to_string()),
			),
		ANFExprVal::IfNode(ifelse) =>
			format!("({}) ? ({}) : ({})",
				compile_expr_js(*ifelse.cond),
				compile_expr_js(*ifelse.then),
				if let Some(box e) = ifelse.r#else {
					compile_expr_js(e)
				} else {
					"false".to_string()
				}
			),
		ANFExprVal::UnaryNode(u) =>
			format!("{}({})",
				match u.op {
					Not => "!",
					Minus => "-",
					_ => panic!("unary is not unary!")
				},
				compile_expr_js(*u.expr)
			),
		ANFExprVal::BinaryNode(b) =>
			if b.op == Xor {
				format!("!({}) != !({})",
					compile_expr_js(*b.left),
					compile_expr_js(*b.right)
				)
			} else {
				format!("({}) {} ({})",
					compile_expr_js(*b.left),
					if !JS_OP.contains_key(&b.op) {
						panic!("{:?} is not in c_op", b.op)
					} else { JS_OP[&b.op] },
					compile_expr_js(*b.right)
				)
			},
		ANFExprVal::CallNode(c) =>
			format!("({})({})",
				compile_expr_js(*c.func),
				c.args.iter()
					.map(|v| compile_trivial(v.clone()))
					.reduce(|acc, next| acc + ", " + &next)
					.unwrap_or_else(|| "".to_string())
			),
	})
}

pub fn compile_py(a: ANFAST) -> String {
	match a {
		ANFAST::LetNode(l) =>
			format!("({} := {})",
				mk_id(l.var.name),
				if let Some(box def) = l.def {
					compile_expr_py(def)
				} else {
					"None".to_string()
				}
			),
		ANFAST::ExprNode(e) =>
			compile_expr_py(e),
	}
}

pub fn compile_expr_py(e: ANFExpr) -> String {
	match e.val {
		ANFExprVal::LambdaNode(l) =>
			format!("(lambda {}: {})",
				l.args.iter()
					.map(|v| mk_id(v.name.clone()))
					.reduce(|acc, next| acc + ", " + &next)
					.unwrap_or_else(|| "".to_string()),
				compile_expr_py(*l.body)
			),
		ANFExprVal::LiteralNode(lit) =>
			compile_literal(lit),
		ANFExprVal::IdentNode(id) =>
			mk_id(id),
		ANFExprVal::BlockNode(b) =>
			format!("({}){}",
				b.iter()
					.map(|box line| compile_py(line.clone()))
					.reduce(|acc, next| acc + ",\n" + &next)
					.unwrap_or_else(|| "".to_string()),
				if e.r#type != Void && b.len() > 1 {
					"[-1]"
				} else {
					""
				}
			),
		ANFExprVal::IfNode(ifelse) =>
			format!("({} if {} else {})",
				compile_expr_py(*ifelse.then),
				compile_expr_py(*ifelse.cond),
				if let Some(box e) = ifelse.r#else {
					compile_expr_py(e)
				} else {
					"False".to_string()
				}
			),
		ANFExprVal::UnaryNode(u) =>
			format!("{}({})",
				match u.op {
					Not => "not ",
					Minus => "-",
					_ => panic!("unary is not unary!")
				},
				compile_expr_py(*u.expr)
			),
		ANFExprVal::BinaryNode(b) =>
			if b.op == Xor {
				format!("!({}) != !({})",
					compile_expr_py(*b.left),
					compile_expr_py(*b.right)
				)
			} else {
				format!("{} {} ({})",
					compile_expr_py(*b.left),
					if !PY_OP.contains_key(&b.op) {
						panic!("{:?} is not in py_op", b.op)
					} else { PY_OP[&b.op] },
					compile_expr_py(*b.right)
				)
			},
		ANFExprVal::CallNode(c) =>
			format!("{}({})",
				compile_expr_py(*c.func),
				c.args.iter()
					.map(|v| compile_trivial(v.clone()))
					.reduce(|acc, next| acc + ", " + &next)
					.unwrap_or_else(|| "".to_string())
			),
	}
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
												.map(|t| compile_type_name(t.clone(), None))
												.reduce(|acc, next| acc + ", " + &next)
												.unwrap_or_else(|| "".to_string());
							if let Some(box ANFExpr {
								val: LambdaNode(lambda),
								..
							}) = l.def {
								self.compile_lambda(lambda, mk_id(l.var.name));
								return "".to_string();
							}
							format!("{} (*{})({})",
								compile_type_name(ret_t, None),
								mk_id(l.var.name.clone()),
								args_t
							)
						}

						t => format!("{} {}", compile_type_name(t, None), mk_id(l.var.name.clone()))
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
							.map(|v| compile_type_name(v.r#type.clone(), Some(mk_id(v.name.clone()))))
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
				compile_type_name(return_t.clone(), None),
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
													if res.trim().is_empty() {
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
						Concat => format!("concat({}, {})", c_left, c_right),
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

fn compile_type_name(t: Type, name: Option<String>) -> String {
	format!("{}{}",
		match t {
			Void => "void",
			Int => "long long int",
			Float => "long double",
			Str => "char*",
			Bool => "char",
			TypeConstructor(mut tc) if tc.name == "Function" => {
				let ret_t = tc.args.pop().unwrap();
				return format!("{} (*{})({})",
					compile_type_name(ret_t, None),
					if name.is_some() { name.unwrap() } else { "".to_string() },
					tc.args.into_iter()
						.map(|t| compile_type_name(t, None))
						.reduce(|acc, next| acc + ", " + &next)
						.unwrap_or_else(|| "void".to_string())
				);
			}
			_ => panic!("aaaa i cant make type {:#?}", t)
		}.to_string(),
	
		if name.is_some() { " ".to_string() + &name.unwrap() } else { "".to_string() }
	)
}

fn mk_id(s: String) -> String {
// TODO: not all genlang names are valid C names! fix!
	format!("_{}", s)
}
