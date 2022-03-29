use std::collections::VecDeque;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::collections::HashMap;

use crate::{
	parse::ast::{
		self,
		Expr,
		ExprVal,
		Pattern,
		Literal,
	},
	lex::cradle::SourceLoc,
};

static COUNTER: AtomicUsize = AtomicUsize::new(0);

fn unique_name() -> String {
		format!("{}", COUNTER.fetch_add(1, Ordering::SeqCst))
}

/// Reorders an expression in administrative normal form
pub fn anfify(e: &mut Expr) {
	fn anfifier(ex: &mut Expr) -> bool {
		match &mut ex.val {
			ExprVal::Call { args, func } if args.iter().any(|a| !is_trivial(a)) => {
				let mut block = VecDeque::new();
				for arg in args.iter_mut() {
					let trivial_declaration = make_trivial(arg);
					if let Some(declaration) = trivial_declaration {
						block.push_back(declaration);
					}
				}

				let val = ExprVal::Call {
					args: args.clone(),
					func: func.clone(),
				};
				block.push_back(
					Expr {
						val,
						..ex.clone()
					}
				);
				ex.val = ExprVal::Block(block);
			}

			_ => {},
		}

		true
	}

	e.transform(anfifier, |_| {});
}

// Mutates e into an equivalent trivial expression,
// and returns an optional declaration for it
fn make_trivial(e: &mut Expr) -> Option<Expr> {
	if is_trivial(e) {
		return None;
	}

	let name = unique_name();
	let r#type = e.r#type.clone();
	let name_loc = SourceLoc::nonexistent();
	let type_loc = None;

	let mutable = false;
	let declared = Pattern::Assignee(
		ast::Parameter {
			name: name.clone(), r#type, name_loc, type_loc, mutable
		}
	);
	let def = box e.clone();
	let declaration_loc = name_loc;
	e.val = ExprVal::Var(ast::Variable {
		name, declaration_loc, generics: HashMap::new()
	});
	let val = ExprVal::Let { declared, def };

	Some(Expr { val, ..e.clone() })
}

fn is_trivial(e: &Expr) -> bool {
	matches!(&e.val, ExprVal::Literal(Literal::AtomicLiteral(_)) | ExprVal::Var(_))
}