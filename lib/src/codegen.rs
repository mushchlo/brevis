use lazy_static::lazy_static;
use maplit::hashmap;

use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::iter;

use crate::{
	parse::ast::{
		Expr,
		ExprVal,
		Lambda,
		Literal,
		Literal::*,
	},
	types::{
		Type,
		Type::*,
		AggregateType,
	},
	lex::tok::{
		TokenLiteral,
		LiteralVal::*,
		OpID,
		OpID::*,
		UOpID::*,
	},
};

use core::core_vals;

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

pub fn compile_js(e: Expr) -> String {
	format!("({})", match e.val {
		ExprVal::LambdaNode(l) =>
			format!("(function ({}) {{ return {}; }})",
				l.args.iter()
					.map(|v| mk_id(&v.name))
					.reduce(|acc, next| acc + ", " + &next)
					.unwrap_or_else(|| "".to_string()),
				compile_js(*l.body)
			),
		ExprVal::LiteralNode(AtomicLiteral(atom)) =>
			compile_atomic(atom),
		ExprVal::LiteralNode(StructLiteral(_s)) =>
			panic!("structs not yet implemented for js backend"),
		ExprVal::LetNode(l) =>
			format!("({} = {})", mk_id(&l.declared.assert_assignee().name), compile_js(*l.def)),
		ExprVal::VarNode(v) =>
			mk_id(&v.name),
		ExprVal::BlockNode(b) =>
			format!("({})",
				b.iter()
					.map(|line| compile_js(line.clone()))
					.reduce(|acc, next| acc + "," + &next)
					.unwrap_or_else(|| "".to_string()),
			),
		ExprVal::IfNode(ifelse) =>
			format!("({}) ? ({}) : ({})",
				compile_js(*ifelse.cond),
				compile_js(*ifelse.then),
				if let Some(box e) = ifelse.r#else {
					compile_js(e)
				} else {
					"false".to_string()
				}
			),
		ExprVal::UnaryNode(u) =>
			format!("{}({})",
				match u.op {
					Not => "!",
					Neg => "-",
					_ => panic!("unary is not unary!")
				},
				compile_js(*u.expr)
			),
		ExprVal::BinaryNode(b) =>
			if b.op == Xor {
				format!("!({}) != !({})",
					compile_js(*b.left),
					compile_js(*b.right)
				)
			} else {
				format!("({}) {} ({})",
					compile_js(*b.left),
					if !JS_OP.contains_key(&b.op) {
						panic!("{:?} is not in c_op", b.op)
					} else { JS_OP[&b.op] },
					compile_js(*b.right)
				)
			},
		ExprVal::CallNode(c) =>
			format!("{}({})",
				compile_js(*c.func),
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

// type_name is used for functions that are named, and therefore can recurse,
// so that they can have their own name defined at the top of the function.
	fn compile_lambda(&mut self, l: Lambda, type_name: Option<&str>) -> String {
		if !l.captured.is_empty() {
			panic!("closures are not yet supported in C codegen.");
		}
		let args_t =
			l.args.iter()
				.map(|a| a.r#type.clone())
				.chain(iter::once(l.body.r#type.clone()))
				.collect::<Vec<_>>();
		let args_names =
			l.args.into_iter()
				.map(|a| a.name)
				.collect();
		let fn_name = lambda_name();
		let return_t = l.body.r#type.clone();

		self.fn_context.push("".to_string());
		let c_body = self.compile(*l.body);
		let declarations = self.fn_context.pop();
		let fn_declaration =
			format!("{}\n{{\n{}{}{}{};\n}}\n",
				self.compile_fn_type(args_t, fn_name.clone(), args_names),

				match type_name {
					Some(name) => format!("{} = {};\n", name, fn_name),
					None => "".to_string(),
				},

				declarations.unwrap(),

				if return_t != Void {
					"return "
				} else { "" },

				c_body
			);
		self.global += &fn_declaration;

		fn_name
	}


	pub fn compile(&mut self, e: Expr) -> String {
		match e.val {
			ExprVal::LiteralNode(lit) =>
				self.compile_literal(lit),

			ExprVal::VarNode(v) if core_vals.contains_key(&v.name) => v.name,
			ExprVal::VarNode(v) => mk_id(&v.name),

			ExprVal::BlockNode(b) =>
				format!("({})",
					b.iter()
						.filter_map(|l| {
							let res = self.compile(l.clone());
							if res.trim().is_empty() {
								None
							} else {
								Some(res)
							}
						})
						.reduce(|acc, next|
							acc.clone() + if !acc.is_empty() { ", " } else { "" } + &next
						)
						.unwrap()
				),

			ExprVal::LambdaNode(l) =>
				self.compile_lambda(l, None),

			ExprVal::IfNode(ifelse) =>
				format!("({}) ? ({}) : ({})",
						self.compile(*ifelse.cond),
						self.compile(*ifelse.then),
						if let Some(box expr) = ifelse.r#else {
							self.compile(expr)
						} else {
							"0".to_string()
						}
				),

			ExprVal::UnaryNode(u) => {
				let c_op = match u.op {
					Not => "!",
					Neg => "-",
					Ref(_) => "&",
					At => "*",
				};

				format!("{}({})", c_op, self.compile(*u.expr))
			}

			ExprVal::LetNode(l) => {
				let declared_var = l.declared.assert_assignee();
				let c_type_name = self.compile_type_name(declared_var.r#type.clone(), mk_id(&declared_var.name), true);
				let def_str =
					match l.def {
						box Expr {
							val: ExprVal::LambdaNode(lambda), ..
						} => {
							let c_fn = self.compile_lambda(lambda, Some(&c_type_name));
							format!("{} = {}", mk_id(&declared_var.name), c_fn)
						}

						box expr =>
							format!("{} = {}", mk_id(&l.declared.assert_assignee().name), self.compile(expr)),
					};
				let prev_context = self.fn_context.pop().unwrap();
				self.fn_context.push(
					format!("{}{};\n", prev_context, c_type_name)
				);
				def_str + "\n"
			}

			ExprVal::BinaryNode(b) => {
				let (c_left, c_right) = (
					self.compile(*b.left),
					self.compile(*b.right)
				);
			// TODO: A special case of strings is only made because we still use
			// C-strings! We should not use C-strings!!!!!!
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

			ExprVal::CallNode(c) => {
				let args =
					c.args.iter()
						.map(|tr| compile_trivial(tr.clone()))
						.reduce(|acc, next| acc + ", " + &next)
						.unwrap_or_else(|| "".to_string());
				format!("{}({})", self.compile(*c.func), args)
			}
		}
	}

	fn compile_type_name(
		&mut self,
		t: Type,
		name: String,
		fns_as_ptrs: bool
	) -> String {
		format!("{}{}{}",
			match t {
				Void => "void",
				Int => "long long int",
				Float => "long double",
				Str => "char*",
				Bool => "char",

				Pointer(box r, _) => {
					return self.compile_type_name(
						r,
						format!("*{}", name),
						fns_as_ptrs
					);
				}

				Func(args) => {
					return self.compile_fn_type(
						args,
						if fns_as_ptrs && !name.is_empty() {
							format!("(*{})", name)
						} else {
							name
						},
						vec![]
					);
				}

				Struct(mut s) => {
					s.sort();
					let member_declarations = s.iter().fold("".to_string(), |acc, a|
						format!("{}\n{};",
							acc,
							self.compile_type_name(a.r#type.clone(), mk_id(&a.name), true)
						)
					);
					let s_name = type_hash(Struct(s.clone()));

					if !self.type_map.contains_key(&Struct(s.clone())) {
						self.global_defs += &format!("struct {} {{{}\n}};\n", s_name, member_declarations);
						self.type_map.insert(Struct(s.clone()), format!("struct {}", s_name));
					}

					&self.type_map[&Struct(s)]
				}

				TypeVar(_) | Forall(_, _) => panic!("generic type {:#?} persisted into code generation, when it should have been removed by momonorphization", t)
			}.to_string(),

			if !name.is_empty() {
				" "
			} else {
				""
			},

			name
		)
	}

// Takes in the vector of the function's argument types,
// ending with the return type.
	fn compile_fn_type(&mut self,
		mut args_t: Vec<Type>,
		acc: String,
		args_name: Vec<String>
	) -> String {
		let ret_t = args_t.pop().unwrap();
		let args: Vec<(Type, String)> = if !args_name.is_empty() {
			args_t.into_iter()
				.zip(args_name.into_iter())
				.collect()
		} else {
			args_t.into_iter()
				.zip(iter::repeat(String::new()))
				.collect()
		};
		let compiled_args =
			args.into_iter()
				.map(|(t, name)|
					self.compile_type_name(t, mk_id(&name), false)
				)
				.reduce(|acc, next| acc + ", " + &next)
				.unwrap();
		match ret_t {
			Func(sub_args_t) => {
				let compiled = format!("{}({})",
					acc,
					compiled_args
				);
				self.compile_fn_type(sub_args_t, compiled, vec![])
			}
			_ => format!("{} {}({})",
				self.compile_type_name(ret_t, "".to_string(), true),
				acc,
				compiled_args
			)
		}
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
								self.compile(e)
							)
						);

				format!("({}) {{ {} }}",
					self.compile_type_name(s_t, "".to_string(), true),
					s_vals
				)
			}
		}
	}
}

fn compile_atomic(lit: TokenLiteral) -> String {
	match lit.val {
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
		ExprVal::VarNode(v) if core_vals.contains_key(&v.name) =>
			v.name,
		ExprVal::VarNode(v) =>
			mk_id(&v.name),
		_ => panic!("trivial is not trivial")
	}
}

fn mk_id(s: &str) -> String {
// TODO: not all brevis names are valid C names! fix!
	if s.is_empty() {
		"".to_string()
	} else {
		format!("_{}", s)
	}
}

// Expects a concrete type (all types should be monomorphized by now),
// generates a hashed name for a type, used for monomorphized functions
fn type_hash(t: Type) -> String {
	match t {
		Void => "v".to_string(),
		Int => "i".to_string(),
		Float => "f".to_string(),
		Str => "s".to_string(),
		Bool => "b".to_string(),
		Pointer(box r, _) =>
			format!("p{}", type_hash(r)),
		Struct(s) =>
			format!("struct__{}__",
				s.iter()
					.map(|agg| type_hash(agg.r#type.clone()) + "_" + &agg.name)
					.reduce(|acc, next| acc + "_" + &next)
					.unwrap_or_else(String::new)
			),
		Func(args_t) =>
			format!("fn__{}__",
				args_t.iter()
					.map(|t| type_hash(t.clone()))
					.reduce(|acc, next| acc + "_" + &next)
					.unwrap()
			),
		TypeVar(_) | Forall(_, _) => panic!(),
	}
}
