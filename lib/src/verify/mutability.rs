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
			ExprVal::Block { .. } => {
				mutability_env.new_stack();
			}

			ExprVal::Let { declared, .. } => {
				for assigned in declared.assignees() {
					mutability_env.insert_in_env(assigned.name.clone(), assigned.clone());
				}
			}

			ExprVal::Lambda { args, .. } => {
				mutability_env.new_stack();
				for arg in args {
					mutability_env.insert_in_env(arg.name.clone(), arg.clone());
				}
			}

			ExprVal::Unary { op: UOpID::Ref(true), expr, .. } => {
				assert_mutable(expr, mutability_env, &mut errs);
			}

			ExprVal::Binary { op: OpID::Eq, left, .. } => {
				assert_mutable(left, mutability_env, &mut errs);
			}

			_ => {}
		}

		true
	};

	let manage_env = |e: &Expr| {
		let mut mutability_env = mutability_env.lock().unwrap();
		match &e.val {
			ExprVal::Block { .. } | ExprVal::Lambda { .. } => {
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

			Unary { op: UOpID::At, .. } => false,

			_ => true,
		}
	};

	ex.visit(asserter, |_| {});
}