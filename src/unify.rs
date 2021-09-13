//! Currently, this file cannot be formatted (by rustfmt) because of the commented macros

use crate::{
	ast::{Expr, ExprVal, Let, TConstructor, Type, Type::*, AST},
	lex::TokenLiteral,
	parse::get_type_var,
	tok::OpID,
};
use lazy_static::lazy_static;
use std::{
	collections::{HashMap, VecDeque},
	sync::Mutex,
};

lazy_static! {
	static ref SUBSTITUTIONS: Mutex<HashMap<u16, Type>> = Mutex::new(HashMap::new());
	static ref TYPE_CONSTRAINTS: Mutex<Vec<(Type, Type)>> = Mutex::new(vec![]);
	static ref ENVIRONMENT: Mutex<Vec<HashMap<String, Type>>> = Mutex::new(vec![]);
}

macro_rules! substitution {
	() => {
		SUBSTITUTIONS.lock().unwrap()
	};
}

macro_rules! constraints {
	() => {
		TYPE_CONSTRAINTS.lock().unwrap().to_vec()
	};
}

macro_rules! environment {
	() => {
		ENVIRONMENT.lock().unwrap().to_vec()
	};
}

fn add_constraint(t1: Type, t2: Type) {
	TYPE_CONSTRAINTS.lock().unwrap().push((t1, t2));
}

fn env_find(key: String) -> Option<Type> {
	let environment = ENVIRONMENT.lock().unwrap().to_vec();
	for map in environment {
		if map.contains_key(&key) {
			return map.get(&key).cloned();
		}
	}

	None
}

fn env_insert(key: String, value: Type) {
	let mut environment = ENVIRONMENT.lock().unwrap();
	println!("inserting {}: {:#?}", key, value);
	println!(
		"insertion value is {:#?}",
		environment.last_mut().unwrap().insert(key, value)
	);
	println!("env looks like this after insert: {:#?}", environment);
}

fn env_new_stack() {
	let mut environment = ENVIRONMENT.lock().unwrap();
	environment.push(HashMap::new());
}

fn env_pop_stack() {
	let mut environment = ENVIRONMENT.lock().unwrap();
	println!("env looks like this before pop: {:#?}", environment);
	environment.pop();
	println!("env looks like this after pop: {:#?}", environment);
}

pub fn unify(t1: Type, t2: Type) {
	match (t1.clone(), t2.clone()) {
		(
			TypeConstructor(TConstructor {
				name: v1,
				args: args1,
			}),
			TypeConstructor(TConstructor {
				name: v2,
				args: args2,
			}),
		) => {
println!("unify1");
			if v1 != v2 {
				panic!("Type constructor {:#?} is not the same as type constructor {:#?}, but was attempted to be unified with it", v1, v2);
			}
			if args1.len() != args2.len() {
				panic!("type constructors with unequal lengths are attempting to be unified, {:#?} and {:#?}", t1, t2);
			}
			for (t3, t4) in args1.clone().into_iter().zip(args2.clone().into_iter()) {
				unify(t3, t4);
			}
println!("leaving unify1");
		}

		(TypeVar(i), TypeVar(j)) if i == j => {
println!("unify2");}
		(TypeVar(i), _) if substitution!().contains_key(&i) => {
println!("unify3");
			unify(substitution!()[&i].clone(), t2)
		}
		(_, TypeVar(j)) if substitution!().contains_key(&j) => {
println!("unify4");
			unify(t1, substitution!()[&j].clone())
		}

		(TypeVar(i), _) => {
println!("unify5");
			assert!(!occurs_in(i, t2.clone()));
println!("inserting into substitute");
			substitution!().insert(i, t2);
println!("leaving unify5");
		}
		(_, TypeVar(j)) => {
println!("unify6");
			assert!(!occurs_in(j, t1.clone()));
			substitution!().insert(j, t1);
println!("leaving unify6");
		}

		_ => {
println!("unify7");
		println!(
			"attempted a weird unification between {:#?} and {:#?}",
			t1, t2
		)
		}
	}
	println!("LEAVING UNIFY");
}

fn occurs_in(index: u16, t: Type) -> bool {
println!("in occurs_in, t is {:#?}", t);
	return match t {
		TypeVar(i) if substitution!().contains_key(&i) => {
			occurs_in(index, substitution!()[&i].clone())
		}
		TypeVar(i) => i == index,
		TypeConstructor(TConstructor { args: a, .. }) => {
			a.iter().any(|t1| occurs_in(index, (*t1).clone()))
		}
		Void | Int | Float | Str | Bool =>{
		println!("exiting"); false}
	};
}

fn infer_type(a: AST) -> Type {
	match a {
		AST::LetNode(l) => {
			env_insert(l.clone().var.name, l.clone().var.r#type);
			if let Some(def) = l.def {
				let def_t = infer_type(AST::ExprNode(*def));
				add_constraint(l.var.r#type, def_t);
			}
			Void
		},
		AST::ExprNode(expr) => match expr.val {
			ExprVal::LiteralNode(lit) =>
				match lit {
					TokenLiteral::StrLit(_) => Type::Str,
					TokenLiteral::IntLit(_) => Type::Int,
					TokenLiteral::FltLit(_) => Type::Float,
					TokenLiteral::BoolLit(_) => Type::Bool
				}, ExprVal::UnaryNode(u) => {
					let t = infer_type(AST::ExprNode(*u.expr));
				/*match u.op {
					OpID::Not => assert!(t == Bool),
					OpID::Minus => assert!(t == Int || t == Float),
					_ => panic!("operator {:?} was attempted to be unified as a unary expression, but is not a unary operator (should never happen)", u.op)
				}*/
					return t
				}, ExprVal::BinaryNode(b) => {
					let (t_left, t_right) = (infer_type(AST::ExprNode(*b.left)), infer_type(AST::ExprNode(*b.right)));
					// assert!(t_left == t_right);
					add_constraint(t_left.clone(), t_right);
					/* TODO: typechecking for binaries should be dependent on what the op is, FIX THIS */
					t_left
				},
				
	
			ExprVal::BlockNode(b) => {
				env_new_stack();
				println!("entering block, new stack");
				for boxed_line in b.range(..b.len() - 1) {
					println!("line is {:#?}", *boxed_line);
					infer_type((**boxed_line).clone());
				}
				let block_val_t = match b.back() {
					Some(line) => infer_type((**line).clone()),
					_ => Void
				};
				println!("popping stack bc block end");
				env_pop_stack();
				
				return block_val_t;
			},
	
			ExprVal::IfNode(i) => {
				println!("doing an if (and your mom)");
				// assert!(inferType(ExprNode(*i.cond)) == Bool);
				let if_t = infer_type(AST::ExprNode(*i.then));
				if let Some(expr) = i.r#else {
						#[allow(unused_variables)]
						let else_t = infer_type(AST::ExprNode(*expr));
						// println!("if_t is `{:#?}`, else_t is `{:#?}`", if_t, else_t);
						// assert!(if_t == else_t);
				}
				println!("finished doing if (but not your mom)");
	
				if_t
			},
	
			ExprVal::IdentNode(v) => env_find(v.name.clone()).unwrap_or_else(|| panic!("unable to find variable {:#?} in the environment when inferring types, env looks like {:#?}", v.name, environment!().to_vec())),
	
			ExprVal::LambdaNode(l) => {
				println!("entering fn, new stack");
				env_new_stack();
				for arg in l.args.iter() {
					env_insert(arg.name.clone(), arg.r#type.clone());
				}

				let fn_type = TypeConstructor(TConstructor {	
						name: format!("Function"), 
						args: {
								let mut tmp = l.args.iter()
												.map(|v| v.r#type.clone())
												.collect::<Vec<Type>>();
								tmp.push(if let AST::ExprNode(e) = *l.body {
											infer_type(AST::ExprNode(e))
										} else { Void });
								tmp
						}
					});
					println!("popping env bc fn end");

				env_pop_stack();

				fn_type
			},
	
			ExprVal::CallNode(c) => {
				println!("inferring type of function, env looks like this: {:#?}", environment!());
				let t1 = infer_type(AST::ExprNode(*c.clone().func));
				println!("inferring type of func {:#?} as {:#?}", *c.func, t1);
				
				let arg_types = c.args.iter()
									.map(|t| infer_type(AST::ExprNode((*t).clone())))
									.collect::<VecDeque<Type>>();
				let return_type = get_type_var();
	
				add_constraint(t1, TypeConstructor(TConstructor {
					name: format!("Function"),
					args: {
							let mut tmp = arg_types.into_iter()
								.collect::<Vec<Type>>();
							tmp.push(return_type.clone());
							tmp
						}
				}));
				return_type
			},
		},
	}
}

fn solve_constraints() {
println!("constraints are {:#?}", constraints!());
	for (t1, t2) in constraints!() {
		unify(t1, t2);
	}
	constraints!().clear();
println!("leaving solve_constraints");
}

fn substitute(t: Type) -> Type {
	match t {
		TypeVar(n) => substitution!()
			.get(&n)
			.unwrap_or_else(|| panic!("No substitution available for variable {}", n))
			.clone(),
		TypeConstructor(tc) => TypeConstructor(TConstructor {
			name: tc.name,
			args: tc
				.args
				.iter()
				.map(|t1| substitute(t1.clone()))
				.collect::<Vec<Type>>(),
		}),

		_ => t,
	}
}

impl Expr {
	fn annotate_helper(&mut self) {
		self.r#type = substitute(self.clone().r#type);
		match self.val {
			ExprVal::LiteralNode(_) | ExprVal::IdentNode(_) => {}
			ExprVal::BlockNode(ref mut b) => {
				for ref mut line in b {
					line.annotate_helper();
				}
			}
			ExprVal::LambdaNode(ref mut l) => {
				for ref mut var in l.clone().args {
					var.r#type = substitute(var.clone().r#type);
				}
				l.body.annotate_helper();
			}
			ExprVal::IfNode(ref mut i) => {
				i.clone().cond.annotate_helper();
				i.clone().then.annotate_helper();
				if let Some(mut else_branch) = i.clone().r#else {
					else_branch.annotate_helper();
				}
			}

			_ => panic!("annotating this thing is not yet supported"),
		}
	}

	pub fn annotate(&mut self) {
		println!("inferring types");
		self.r#type = infer_type(AST::ExprNode(self.clone()));
		println!("solving constraints");
		solve_constraints();
		println!("annotating types");
		self.annotate_helper();
	}
}

impl AST {
	fn annotate_helper(&mut self) {
		match self {
			AST::ExprNode(e) => e.annotate_helper(),
			AST::LetNode(Let {
				def: Some(expr), ..
			}) => expr.annotate_helper(),

			_ => {}
		}
	}
}
