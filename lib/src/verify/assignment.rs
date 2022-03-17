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
		match &e.val {
			ExprVal::BinaryNode(b) if b.op == OpID::Eq => {
				assert_assignable(&b.left, b.op_loc, &mut errors);
			}

			_ => {}
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
			UnaryNode(u) if u.op != UOpID::At => {
				push_err!(
					errors,
					vec![ e.loc, op_loc ],
					"a unary expression was attempted to be assigned to",
				);

				false
			}

			LiteralNode(_) | LetNode(_) | BlockNode(_) | LambdaNode(_)
				| IfNode(_) | BinaryNode(_) | CallNode(_) => {
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