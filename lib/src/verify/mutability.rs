use std::{
	sync::Mutex,
	collections::{
		HashMap,
		HashSet,
	},
};

use crate::{
	error::ErrorMessage,
	lex::tok::OpID,
	parse::ast::{
		Expr,
		ExprVal,
		Parameter,
	},
	util::Env,
};

pub fn verify_mutability(to_verify: &mut Expr) -> HashSet<ErrorMessage> {
	let errs = Mutex::new(HashSet::new());
	let mutability_env: Mutex<Vec<HashMap<String, Parameter>>>
		= Mutex::new(Vec::new());

// TODO: Distinguish between mutable references, and immutable references.
// until that is done, we don't know if it's OK to use a pointer to an
// immutable value safely.
	let transifier = |e: &mut Expr| {
		let mut errs = errs.lock().unwrap();
		let mutability_env = &mut *mutability_env.lock().unwrap();
		match &e.val {
			ExprVal::BlockNode(_) => {
				mutability_env.new_stack();
			}

			ExprVal::LetNode(l) => {
				for declared in l.declared.assignees() {
					mutability_env.insert_in_env(declared.name.clone(), declared.clone());
				}
			}

			ExprVal::LambdaNode(l) => {
				mutability_env.new_stack();
				for arg in &l.args {
					mutability_env.insert_in_env(arg.name.clone(), arg.clone());
				}
			}

		// TODO: Assignment by pattern will break this! Just a dumb check for
		// var name assignment
			ExprVal::BinaryNode(b) if b.op == OpID::Eq => {
				if let ExprVal::VarNode(v) = &b.left.val {
					let var_declaration = mutability_env.find(&v.name).unwrap();
					if !var_declaration.mutable {
						errs.insert(ErrorMessage {
							msg: format!("the variable `{}` is reassigned, as seen below, but is declared as immutable (consider making this variable mutable)", v.name),
							origins: vec![ b.op_loc, var_declaration.name_loc ],
						});
					}
				}
			}

			_ => {}
		}

		true
	};

	let post_transifier = |e: &mut Expr| {
		let mut mutability_env = mutability_env.lock().unwrap();
		match &e.val {
			ExprVal::BlockNode(_) | ExprVal::LambdaNode(_) => {
				mutability_env.pop();
			}

			_ => {}
		}
	};

	to_verify.transform(transifier, post_transifier);

	errs.into_inner().unwrap()
}