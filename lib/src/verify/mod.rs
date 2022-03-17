mod mutability;
mod assignment;

use std::collections::HashSet;

use crate::{
	verify::{
		mutability::verify_mutability,
		assignment::verify_assignment,
	},
	parse::ast::Expr,
	error::ErrorMessage,
};

pub fn verify(e: &mut Expr) -> HashSet<ErrorMessage> {
	let mut errs = HashSet::new();
	for verify in [ verify_mutability, verify_assignment ] {
		errs.extend(verify(e));
	}

	errs
}