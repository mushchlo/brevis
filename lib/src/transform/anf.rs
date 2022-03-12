use std::collections::VecDeque;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::collections::HashMap;

use crate::{
	parse::ast::{
		self,
		Expr,
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
pub fn anfify(e: Expr) -> Expr {
	use parse::ast::ExprVal::*;
	Expr {
		r#type: e.r#type.clone(),
		loc: e.loc,
		val:
			match e.val {
				LiteralNode(lit) => LiteralNode(lit),
				VarNode(v) =>
					VarNode(v),
				LetNode(l) =>
					LetNode(ast::Let {
						declared: l.declared.clone(),
						def: box anfify(*l.def)
					}),
				IfNode(i) =>
					IfNode(ast::IfElse {
						cond: box anfify(*i.cond),
						then: box anfify(*i.then),
						r#else: i.r#else.map(|box e| box anfify(e)),
					}),
				BlockNode(b) =>
					BlockNode(
						b.iter().map(|line| anfify(line.clone())).collect()
					),
				LambdaNode(l) =>
					LambdaNode(ast::Lambda {
						args: l.args,
						captured: l.captured,
						body: Box::new(anfify(*l.body))
					}),
				UnaryNode(u) =>
					UnaryNode(ast::Unary {
						op: u.op,
						op_loc: u.op_loc,
						expr: Box::new(anfify(*u.expr))
					}),
				BinaryNode(b) =>
					BinaryNode(ast::Binary {
						op: b.op,
						op_loc: b.op_loc,
						left: box anfify(*b.left),
						right: box anfify(*b.right),
					}),
				CallNode(c) => {
					let mut block = VecDeque::new();
					let mut trivial_args = VecDeque::new();
					for arg in c.args.into_iter() {
						trivial_args.push_back(match &arg.val {
							LiteralNode(_) | VarNode(_) => arg,

							_ => {
								let name = unique_name();
								let cloned_arg_t = arg.r#type.clone();
								block.push_back(
									Expr {
										val:
											LetNode(ast::Let {
												declared: Pattern::Assignee(ast::Parameter {
													name: name.clone(),
													mutable: false,
													// Zero values, as this variable doesn't exist in the source code.
													name_loc: SourceLoc::nonexistent(),
													type_loc: None,
													r#type: arg.r#type.clone()
												}),
												def: box anfify(arg),
											}),
										r#type: Type::Void,
										loc: SourceLoc::nonexistent(),
									}
								);
								Expr {
									val: VarNode(
										ast::Variable {
											name,
											generics: HashMap::new(),
										}
									),
									loc: SourceLoc::nonexistent(),
									r#type: cloned_arg_t,
								}
							}
						});
					}
					block.push_back(
						Expr {
							val: CallNode(ast::Call {
								args: trivial_args,
								func: Box::new(anfify(*c.func))
							}),
							loc: SourceLoc::nonexistent(),
							r#type: e.r#type
						}
					);

					BlockNode(block)
				}
			}
	}
}
