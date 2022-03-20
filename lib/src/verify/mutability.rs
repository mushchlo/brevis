use std::{
	sync::Mutex,
	collections::{
		HashMap,
		HashSet,
	},
};

use crate::{
	push_err,
	error::ErrorMessage,
	lex::tok::{UOpID, OpID},
	parse::ast::{
		Expr,
		ExprVal,
		Parameter,
	},
	util::Env,
};

pub fn verify_mutability(to_verify: &Expr) -> HashSet<ErrorMessage> {
	let errs = Mutex::new(HashSet::new());
	let mutability_env: Mutex<Vec<HashMap<String, Parameter>>>
		= Mutex::new(Vec::new());

	let verify = |e: &Expr| {
		let mut errs = errs.lock().unwrap();
		let mutability_env = &mut *mutability_env.lock().unwrap();
		match &e.val {
			ExprVal::Block(_) => {
				mutability_env.new_stack();
			}

			ExprVal::Let(l) => {
				for declared in l.declared.assignees() {
					mutability_env.insert_in_env(declared.name.clone(), declared.clone());
				}
			}

			ExprVal::Lambda(l) => {
				mutability_env.new_stack();
				for arg in &l.args {
					mutability_env.insert_in_env(arg.name.clone(), arg.clone());
				}
			}

			ExprVal::Unary(u) if u.op == UOpID::Ref(true) => {
				assert_mutable(&u.expr, mutability_env, &mut errs);
			}

			ExprVal::Binary(b) if b.op == OpID::Eq => {
				assert_mutable(&b.left, mutability_env, &mut errs);
			}

			_ => {}
		}

		true
	};

	let manage_env = |e: &Expr| {
		let mut mutability_env = mutability_env.lock().unwrap();
		match &e.val {
			ExprVal::Block(_) | ExprVal::Lambda(_) => {
				mutability_env.pop();
			}

			_ => {}
		}
	};

	to_verify.visit(verify, manage_env);

	errs.into_inner().unwrap()
}

fn assert_mutable(
	ex: &Expr,
	mut_env: &impl Env<String, Parameter>,
	errs: &mut HashSet<ErrorMessage>
) {
	use self::ExprVal::*;

	let asserter = |e: &Expr| {
		match &e.val {
			Var(v) => {
				let var_declaration = mut_env.find(&v.name).unwrap();
				if !var_declaration.mutable {
					push_err!(
						errs,
						vec![ ex.loc, var_declaration.name_loc ],
						"the variable `{}` is mutated, as seen below, but is declared as immutable (consider making this variable mutable)",
						v.name
					);
				}

				true
			}

			Unary(u) if u.op == UOpID::At => false,

			_ => true,
		}
	};

	ex.visit(asserter, |_| {});
}