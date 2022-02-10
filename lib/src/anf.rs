use std::collections::VecDeque;
use std::sync::atomic::{AtomicUsize, Ordering};

use ast::{
	*,
	AST::*,
	ExprVal::*
};
use cradle::SourceLoc;

static COUNTER: AtomicUsize = AtomicUsize::new(0);

fn unique_name() -> String {
		format!("{}", COUNTER.fetch_add(1, Ordering::SeqCst))
}

pub fn anfify(a: AST) -> AST {
	match a {
		LetNode(l) =>
			LetNode(Let {
				var: l.var.clone(),
				def:
					if let Some(box def) = l.def {
						Some(box anfify_expr(def))
					}  else { None }
			}),
		ExprNode(e) =>
			ExprNode(anfify_expr(e)),
	}
}

/// Reorders an expression in administrative normal form
pub fn anfify_expr(e: Expr) -> Expr {
	Expr {
		r#type: e.r#type.clone(),
	// This is an expression wildly different from the source code,
	// so there's not really a source position to store.
		loc: e.loc,
		val:
			match e.val {
				LiteralNode(lit) => LiteralNode(lit),
				VarNode(v) =>
					VarNode(v),
				IfNode(i) =>
					IfNode(IfElse {
						cond: box anfify_expr(*i.cond),
						then: box anfify_expr(*i.then),
						r#else: i.r#else.map(|box e| box anfify_expr(e)),
					}),
				BlockNode(b) =>
					BlockNode(
						b.iter().map(|line| anfify(line.clone())).collect()
					),
				LambdaNode(l) =>
					LambdaNode(Lambda {
						args: l.args.clone(),
						generics: l.generics.clone(),
						body: Box::new(anfify_expr(*l.body))
					}),
				UnaryNode(u) =>
					UnaryNode(Unary {
						op: u.op,
						op_loc: u.op_loc,
						expr: Box::new(anfify_expr(*u.expr))
					}),
				BinaryNode(b) =>
					BinaryNode(Binary {
						op: b.op,
						op_loc: b.op_loc,
						left: Box::new(anfify_expr(*b.left.clone())),
						right: Box::new(anfify_expr(*b.right)),
					}),
				CallNode(c) => {
					let mut block = VecDeque::new();
					let mut trivial_args = VecDeque::new();
					for arg in c.args.iter() {
						trivial_args.push_back(match arg.val.clone() {
							LiteralNode(_) | VarNode(_) => arg.clone(),

							_ => {
								let name = unique_name();
								block.push_back(
									LetNode(Let {
										var:
											Parameter {
												name: name.clone(),
											// Zero values, as the variable doesn't exist in the source code.
												name_loc: SourceLoc::nonexistent(),
												type_loc: None,
												r#type: arg.r#type.clone()
											},
										def:
											Some(Box::new(
												anfify_expr(arg.clone())
											))
									})
								);
								Expr {
									val: VarNode(
										Variable {
											name,
											generics: vec![],
										}
									),
									loc: SourceLoc::nonexistent(),
									r#type: arg.r#type.clone()
								}
							}
						});
					}
					block.push_back(
						ExprNode(Expr {
							val: CallNode(Call {
								args: trivial_args,
								func: Box::new(anfify_expr(*c.func))
							}),
							loc: SourceLoc::nonexistent(),
							r#type: e.r#type
						})
					);

					BlockNode(block)
				}
			}
	}
}
