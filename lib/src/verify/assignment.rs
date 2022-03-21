use std::collections::HashSet;

use crate::{
	push_err,
	parse::ast::{Expr, ExprVal},
	lex::{
		cradle::SourceLoc,
		tok::{OpID, UOpID},
	},
	error::ErrorMessage,
};


pub fn verify_assignment(to_verify: &Expr) -> HashSet<ErrorMessage> {
	let mut errors = HashSet::new();
	let verify = |e: &Expr| {
		if let ExprVal::Binary { op: OpID::Eq, left, op_loc, .. } = &e.val {
			assert_assignable(left, *op_loc, &mut errors);
		}

		true
	};

	to_verify.visit(verify, |_| {});

	errors
}

fn assert_assignable(e: &Expr, op_loc: SourceLoc, errors: &mut HashSet<ErrorMessage>) {
	use self::ExprVal::*;
	let asserter = |e: &Expr| {
		match &e.val {
			Unary { op_loc, op, .. } if *op != UOpID::At => {
				push_err!(
					errors,
					vec![ e.loc, *op_loc ],
					"a unary expression was attempted to be assigned to",
				);

				false
			}

			Literal(_) | Let { .. } | Block { .. } | Lambda { .. }
				| If { .. } | Binary { .. } | Call { .. } => {
				push_err!(
					errors,
					vec![ e.loc, op_loc ],
					"{} was attempted to be assigned to",
					e.describe()
				);

				false
			}

			_ => true,
		}
	};

	e.visit(asserter, |_| {});
}