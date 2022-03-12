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
		Lambda,
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
		if let ExprVal::LetNode(l) = &mut ex.val {
			match l.declared.clone() {
				Assignee(_) | Literal(_) | Empty(_) => {}

				Func { func, args } => {
					l.declared = Assignee(func);
					l.def = box Expr {
						r#type: Type::Func(
							args.iter().map(|arg| &arg.r#type).chain(iter::once(&l.def.r#type)).cloned().collect()
						),
						val: ExprVal::LambdaNode(Lambda {
							args: VecDeque::from(args.clone()),
							captured: HashSet::new(),
							body: l.def.clone(),
						}),
						loc: l.def.loc,
					};
				}

				Struct(_s) => panic!("structure patterns not supported yet!"),
			}
		}
		true
	};

	e.transform(desugarer, |_| {});
}