use std::collections::VecDeque;
use crate::{
	anf::{ANFAST, ANFExpr, ANFExprVal,ANFLambda},
};

fn separate_block(mut b: VecDeque<Box<ANFAST>>) -> (VecDeque<Box<ANFAST>>, ANFExpr) {
	let last =
		match *b.back().unwrap().clone() {
			ANFAST::ExprNode(e) => e,
			_ => panic!("last part of a block needs to be an expression")
		};
	b.pop_back();
	return (b, last);
}

pub fn reduce_blocks(a: ANFAST) -> ANFAST {
	match a {
		ANFAST::LetNode(mut l) => {
			l.def = l.def.map(|box d| box reduce_blocks_expr(reduce_blocks_expr(d)));
			if let Some(box ANFExpr { val: ANFExprVal::BlockNode(b), .. }) = l.def.clone() {
				let (mut pre_def, def) = separate_block(b);
				l.def = Some(box def);
				pre_def.push_back(box ANFAST::LetNode(l.clone()));
				ANFAST::ExprNode(ANFExpr {
					val: ANFExprVal::BlockNode(pre_def),
					r#type: l.var.r#type,
				})
			} else {
				ANFAST::LetNode(l)
			}
		}
		ANFAST::ExprNode(e) =>
			ANFAST::ExprNode(reduce_blocks_expr(e)),
	}
}

pub fn reduce_blocks_expr(e: ANFExpr) -> ANFExpr {
	let reduced_val = match e.val {
		ANFExprVal::LiteralNode(_) | ANFExprVal::IdentNode(_) | ANFExprVal::CallNode(_)
			=> e.val,
		ANFExprVal::LambdaNode(l) =>
			ANFExprVal::LambdaNode(ANFLambda {
				body: box reduce_blocks_expr(*l.body),
				args: l.args
			}),

		ANFExprVal::UnaryNode(mut u) => {
			*u.expr = reduce_blocks_expr(*u.expr);
			if let ANFExprVal::BlockNode(b) = u.expr.val {
				let mut tmp;
				(tmp, *u.expr) = separate_block(b);
				tmp.push_back(box ANFAST::ExprNode(ANFExpr {
					val: ANFExprVal::UnaryNode(u),
					r#type: e.r#type.clone()
				}));
				ANFExprVal::BlockNode(tmp)
			} else {
				ANFExprVal::UnaryNode(u)
			}
		}

		ANFExprVal::BinaryNode(mut bin) => {
			let mut ret = VecDeque::new();

			if let ANFExprVal::BlockNode(b) = bin.left.val {
				(ret, *bin.left) = separate_block(b);
			}
			if let ANFExprVal::BlockNode(b) = bin.right.val {
				let mut tmp;
				(tmp, *bin.right) = separate_block(b);
				ret.append(&mut tmp);
			}
			if ret.len() != 0 {
				ret.push_back(box ANFAST::ExprNode(ANFExpr {
					val: ANFExprVal::BinaryNode(bin),
					r#type: e.r#type.clone()
				}));

				ANFExprVal::BlockNode(ret)
			} else {
				ANFExprVal::BinaryNode(bin)
			}
		}

		ANFExprVal::IfNode(mut ifelse) => {
			ifelse.then = box reduce_blocks_expr(*ifelse.then);
			ifelse.r#else = ifelse.r#else.map(|box expr| box reduce_blocks_expr(expr));

			if let ANFExprVal::BlockNode(b) = ifelse.cond.val.clone() {
				let mut tmp;
				(tmp, *ifelse.cond) = separate_block(b);
				tmp.push_back(box ANFAST::ExprNode(ANFExpr {
					val: ANFExprVal::IfNode(ifelse),
					r#type: e.r#type.clone()
				}));

				ANFExprVal::BlockNode(tmp)
			} else {
				ANFExprVal::IfNode(ifelse)
			}
		}
		ANFExprVal::BlockNode(b) => {
			let mut ret: VecDeque<Box<ANFAST>> = VecDeque::new();
			for line in b.iter().map(|box l| reduce_blocks(l.clone())) {
				if let ANFAST::ExprNode(ANFExpr {
							val: ANFExprVal::BlockNode(sub_b),
							..
						}) = line.clone() {
					for sub_line in sub_b {
						ret.push_back(box reduce_blocks(*sub_line));
					}
				} else {
					ret.push_back(box line);
				}
			}
			ANFExprVal::BlockNode(ret)
		}
	};

	ANFExpr {
		val: reduced_val,
		r#type: e.r#type
	}
}