use std::{
	iter,
	collections::{
		HashSet,
		VecDeque,
	},
};

use crate::{
	parse::ast::{
		Expr,
		ExprVal,
		Pattern,
	},
	types::Type,
};

pub fn desugar(e: &mut Expr) {
	desugar_assignment_patterns(e)
}

fn desugar_assignment_patterns(e: &mut Expr) {
	use self::Pattern::*;
	let desugarer = |ex: &mut Expr| {
		if let ExprVal::Let { def, declared } = &mut ex.val {
			match declared.clone() {
				Assignee(_) | Literal(_) | Empty(_) => {}

				Func { func, args } => {
					*declared = Assignee(func);
					**def = Expr {
						r#type: Type::Func(
							args.iter()
								.map(|arg| &arg.r#type)
								.chain(iter::once(&def.r#type))
								.cloned()
								.collect()
						),
						val: ExprVal::Lambda {
							args: VecDeque::from(args.clone()),
							captured: HashSet::new(),
							body: def.clone(),
						},
						loc: def.loc,
					};
				}

				Struct(_) => panic!("structure patterns not supported yet!"),
			}
		}

		true
	};

	e.transform(desugarer, |_| {});
}