use std::collections::VecDeque;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::collections::HashMap;

use crate::{
	parse::ast::{
		self,
		Expr,
		ExprVal,
		Pattern,
	},
	types::{
		Type,
	},
	lex::cradle::SourceLoc,
};

static COUNTER: AtomicUsize = AtomicUsize::new(0);

fn unique_name() -> String {
		format!("{}", COUNTER.fetch_add(1, Ordering::SeqCst))
}

/// Reorders an expression in administrative normal form
pub fn anfify(e: &mut Expr) {
	use self::ExprVal::*;

	let anfifier = |ex: &mut Expr| {
		if let Call { args, func } = &mut ex.val {
			let mut block = VecDeque::new();
			let mut trivial_args = VecDeque::new();
			for arg in args.iter_mut() {
				trivial_args.push_back(match &arg.val {
					Literal(_) | Var(_) => arg.clone(),

					_ => {
						let name = unique_name();
						let cloned_arg_t = arg.r#type.clone();
						let declared =
							Pattern::Assignee(ast::Parameter {
								name: name.clone(),
								mutable: false,
								// Zero values, as this variable doesn't exist in the source code.
								name_loc: SourceLoc::nonexistent(),
								type_loc: None,
								r#type: arg.r#type.clone()
							});
						anfify(arg);
						block.push_back(Expr {
							val: Let { declared, def: box arg.clone() },
							r#type: Type::Void,
							loc: SourceLoc::nonexistent(),
						});
						Expr {
							val: Var(ast::Variable {
									name,
									declaration_loc: SourceLoc::nonexistent(),
									generics: HashMap::new(),
							}),
							loc: SourceLoc::nonexistent(),
							r#type: cloned_arg_t,
						}
					}
				});
			}

			anfify(func);
			block.push_back(
				Expr {
					val: Call {
						args: trivial_args,
						func: func.clone(),
					},
					loc: SourceLoc::nonexistent(),
					r#type: ex.r#type.clone()
				}
			);
			ex.val = Block(block);
		}

		true
	};

	e.transform(anfifier, |_| {})
}
