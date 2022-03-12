mod mutability;

use std::collections::HashSet;

use crate::{
	verify::mutability::verify_mutability,
	parse::ast::Expr,
	error::ErrorMessage,
};

pub fn verify(e: &mut Expr) -> HashSet<ErrorMessage> {
	let mut errs = HashSet::new();
	errs.extend(verify_mutability(e));

	errs
}