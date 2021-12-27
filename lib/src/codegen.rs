use lazy_static::lazy_static;
use maplit::hashmap;

use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};

use ast::{
	*,
	ExprVal::*,
	Type::*,
	Literal::*,
};
use tok::{
	TokenLiteral,
	TokenLiteral::*,
	OpID,
	OpID::*,
};

static LAMBDA_COUNTER: AtomicUsize = AtomicUsize::new(0);

lazy_static! {
	static ref C_OP: HashMap<OpID, &'static str> =
		hashmap!{
			Eq => "=",
			Member => ".",
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
	static ref JS_OP: HashMap<OpID, &'static str> = {
			let mut tmp = C_OP.clone();
			tmp.insert(Concat, "+");
			tmp
		};
}

fn lambda_name() -> String {
	format!("lambda_{}", LAMBDA_COUNTER.fetch_add(1, Ordering::SeqCst))
}

pub fn compile_js(a: AST) -> String {
	match a {
		AST::LetNode(l) =>
			format!("({} = {})",
				mk_id(l.var.name),
				if let Some(box def) = l.def {
					compile_expr_js(def)
				} else {
					"false".to_string()
				}
			),
		AST::ExprNode(e) =>
			compile_expr_js(e)
	}
}

pub fn compile_expr_js(e: Expr) -> String {
	format!("({})", match e.val {
		LambdaNode(l) =>
			format!("(function ({}) {{ return {}; }})",
				l.args.iter()
					.map(|v| mk_id(v.name.clone()))
					.reduce(|acc, next| acc + ", " + &next)
					.unwrap_or_else(|| "".to_string()),
				compile_expr_js(*l.body)
			),
		LiteralNode(AtomicLiteral(atom)) =>
			compile_atomic(atom),
		LiteralNode(StructLiteral(_s)) =>
			panic!("structs not yet implemented for js backend"),
		IdentNode(id) =>
			mk_id(id),
		BlockNode(b) =>
			format!("({})",
				b.iter()
					.map(|box line| compile_js(line.clone()))
					.reduce(|acc, next| acc + "," + &next)
					.unwrap_or_else(|| "".to_string()),
			),
		IfNode(ifelse) =>
			format!("({}) ? ({}) : ({})",
				compile_expr_js(*ifelse.cond),
				compile_expr_js(*ifelse.then),
				if let Some(box e) = ifelse.r#else {
					compile_expr_js(e)
				} else {
					"false".to_string()
				}
			),
		UnaryNode(u) =>
			format!("{}({})",
				match u.op {
					Not => "!",
					Minus => "-",
					_ => panic!("unary is not unary!")
				},
				compile_expr_js(*u.expr)
			),
		BinaryNode(b) =>
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
		CallNode(c) =>
			format!("({})({})",
				compile_expr_js(*c.func),
				c.args.iter()
					.map(|v| compile_trivial(v.clone()))
					.reduce(|acc, next| acc + ", " + &next)
					.unwrap_or_else(|| "".to_string())
			),
	})
}

pub struct Compilation {
	pub global_defs: String,
	pub global: String,
	pub type_map: HashMap<Type, String>,
	pub fn_context: Vec<String>
}

impl Compilation {
	pub fn new() -> Self {
		Compilation {
			global_defs: "".to_string(),
			global: "".to_string(),
			type_map: HashMap::new(),
			fn_context: vec!["".to_string()]
		}
	}

	pub fn compile(&mut self, a: AST) -> String {
		match a {
			AST::LetNode(l) => {
				let c_type_name =
					match l.var.r#type {
						TypeConstructor(mut tc) if tc.name == "Function" => {
							let ret_t = tc.args.pop().unwrap();
							let args_t = tc.args.iter()
												.map(|t| self.compile_type_name(t.clone(), None))
												.reduce(|acc, next| acc + ", " + &next)
												.unwrap_or_else(|| "".to_string());
							if let Some(box Expr {
								val: LambdaNode(lambda),
								..
							}) = l.def {
								self.compile_lambda(lambda, mk_id(l.var.name));
								return "".to_string();
							}
							format!("{} (*{})({})",
								self.compile_type_name(ret_t, None),
								mk_id(l.var.name.clone()),
								args_t
							)
						}

						t => format!("{} {}", self.compile_type_name(t, None), mk_id(l.var.name.clone()))
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

			AST::ExprNode(e) =>
				format!("({})", self.compile_expr(e))
		}
	}

	fn compile_lambda(&mut self, l: Lambda, mut name: String) -> String {
		let return_t = l.body.r#type.clone();
		let args_str = l.args.iter()
							.map(|v| self.compile_type_name(v.r#type.clone(), Some(v.name.clone())))
							.reduce(|acc, next| acc + ", " + &next)
							.unwrap_or_else(|| "void".to_string());
		if name.is_empty() {
			 name = lambda_name();
		}
		self.fn_context.push("".to_string());
		let c_body = self.compile_expr(*l.body);
		let declarations = self.fn_context.pop();
		let fn_declaration =
			&format!("{}\n{}({})\n{{\n{}{}{};\n}}\n",
				self.compile_type_name(return_t.clone(), None),
				name,
				args_str,
				declarations.unwrap(),

				if return_t != Void {
					"return "
				} else { "" },

				c_body
			);
		self.global += fn_declaration;
		name
	}


	pub fn compile_expr(&mut self, e: Expr) -> String {
		match e.val {
			LiteralNode(lit) =>
				self.compile_literal(lit),

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

			LambdaNode(l) =>
				self.compile_lambda(l, "".to_string()),

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
				} else if b.op == Member {
					format!("({}).{}", c_left, c_right)
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

	fn compile_type_name(&mut self, t: Type, name: Option<String>) -> String {
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
						self.compile_type_name(ret_t, None),
						if name.is_some() { name.unwrap() } else { "".to_string() },
						tc.args.into_iter()
							.map(|t| self.compile_type_name(t, None))
							.reduce(|acc, next| acc + ", " + &next)
							.unwrap_or_else(|| "void".to_string())
					);
				}

				Struct(mut s) => {
					s.sort();
					let name = s.iter().fold("".to_string(), |acc, a|
						format!("{}{}{}__{}",
							acc,
							if acc.is_empty() { "___" } else { "" },
							a.name,
							self.compile_type_name(a.r#type.clone(), None).replace(" ", "_")
						)
					);
					let member_declarations = s.iter().fold("".to_string(), |acc, a|
						format!("{}\n{};",
							acc,
							self.compile_type_name(a.r#type.clone(), Some(a.name.clone()))
						)
					);

					if !self.type_map.contains_key(&Struct(s.clone())) {
						self.global_defs += &format!("struct {} {{{}\n}};\n", name, member_declarations);
						self.type_map.insert(Struct(s.clone()), format!("struct {}", name));
					}

					&self.type_map[&Struct(s)]
				}

				_ => panic!("aaaa i cant make type {:#?}", t)
			}.to_string(),

			match name {
				Some(s) => format!(" {}", mk_id(s)),
				None => "".to_string()
			}
		)
	}

	fn compile_literal(&mut self, lit: Literal) -> String {
		match lit {
			AtomicLiteral(atomic) => compile_atomic(atomic),
			StructLiteral(s) => {
				let s_t = Struct(
					s.iter().map(|a|
						AggregateType {
							name: a.name.clone(),
							r#type: a.val.r#type.clone(),
						}
					).collect()
				);

				let s_vals =
					s.iter()
						.map(|a| a.val.clone())
						.fold("".to_string(), |acc, e|
							format!("{}{}{}",
								acc,
								if acc.is_empty() { "" } else { ", " },
								self.compile_expr(e)
							)
						);

				format!("({}) {{ {} }}",
					self.compile_type_name(s_t, None),
					s_vals
				)
			}
		}
	}
}

fn compile_atomic(lit: TokenLiteral) -> String {
	match lit {
		IntLit(i) => format!("{}", i),
		FltLit(f) => format!("{}", f),
		StrLit(s) => format!("\"{}\"", s),
		BoolLit(b) => (if b {"1"} else {"0"}).to_string(),
	}
}

fn compile_trivial(tr: Expr) -> String {
	match tr.val {
		ExprVal::LiteralNode(AtomicLiteral(atom)) =>
			compile_atomic(atom),
		ExprVal::IdentNode(id) => mk_id(id),
		_ => panic!("trivial is not trivial")
	}
}

fn mk_id(s: String) -> String {
// TODO: not all brevis names are valid C names! fix!
	format!("_{}", s)
}
