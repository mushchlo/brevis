extern crate rand;

use std::collections::VecDeque;
use std::sync::atomic::{AtomicUsize, Ordering};
use crate::{
	lex::TokenLiteral,
	tok::OpID,
	ast::{
		AST,
		Type,
		Expr,
		ExprVal,
		Variable,
	},
};


#[derive(Clone, Debug)]
pub enum ANFAST {
	LetNode(ANFLet),
	ExprNode(ANFExpr),
}

#[derive(Clone, Debug)]
pub struct ANFExpr {
	val: ANFExprVal,
	r#type: Type,
}

#[derive(Clone, Debug)]
pub enum ANFExprVal {
	LiteralNode(TokenLiteral),

	IdentNode(String),
	BlockNode(VecDeque<Box<ANFAST>>),
	LambdaNode(ANFLambda),
	IfNode(ANFIfElse),

	UnaryNode(ANFUnary),
	BinaryNode(ANFBinary),
	CallNode(ANFCall),
}

#[derive(Clone, Debug)]
pub struct ANFLet {
	pub var: Variable,
	pub def: Option<Box<ANFExprVal>>,
}

#[derive(Clone, Debug)]
pub enum Trivial {
	Literal(TokenLiteral),
	Ident(String),
}

#[derive(Clone, Debug)]
pub struct ANFLambda {
	pub args: VecDeque<Variable>,
	pub body: Box<ANFExprVal>,
}

#[derive(Clone, Debug)]
pub struct ANFIfElse {
	pub cond: Box<ANFExprVal>,
	pub then: Box<ANFExprVal>,
	pub r#else: Option<Box<ANFExprVal>>,
}

#[derive(Clone, Debug)]
pub struct ANFUnary {
	pub op: OpID,
	pub expr: Box<ANFExprVal>,
}

#[derive(Clone, Debug)]
pub struct ANFBinary {
	pub op: OpID,
	pub left: Box<ANFExprVal>,
	pub right: Box<ANFExprVal>,
}

#[derive(Clone, Debug)]
pub struct ANFCall {
	pub func: Box<ANFExprVal>,
	pub args: VecDeque<Trivial>,
}

static COUNTER: AtomicUsize = AtomicUsize::new(0);

fn unique_name() -> String {
		format!("{}", COUNTER.fetch_add(1, Ordering::SeqCst))
}

pub fn anfify(a: AST) -> ANFAST {
	match a {
		AST::LetNode(l) =>
			ANFAST::LetNode(ANFLet {
				var: l.var.clone(),
				def:
					if let Some(box def) = l.def {
						Some(box anfify_expr(def))
					} else { None }
			}),
		AST::ExprNode(e) =>
			ANFAST::ExprNode(ANFExpr {
				r#type: e.r#type.clone(),
				val: anfify_expr(e),
			}),
	}
}

/// Reorders an AST in administrative normal form
pub fn anfify_expr(e: Expr) -> ANFExprVal {
	match e.val {
		ExprVal::LiteralNode(lit) => ANFExprVal::LiteralNode(lit),
		ExprVal::IdentNode(id) =>
			ANFExprVal::IdentNode(id),
		ExprVal::IfNode(i) =>
			ANFExprVal::IfNode(ANFIfElse {
				cond: Box::new(anfify_expr(*i.cond)),
				then: Box::new(anfify_expr(*i.then)),
				r#else: i.r#else.map(|box e| box anfify_expr(e)),
			}),
		ExprVal::BlockNode(b) =>
			ANFExprVal::BlockNode(
				b.iter().map(|box line| box anfify(line.clone())).collect()
			),
		ExprVal::LambdaNode(l) =>
			ANFExprVal::LambdaNode(ANFLambda {
				args: l.args.clone(),
				body: Box::new(anfify_expr(*l.body))
			}),
		ExprVal::UnaryNode(u) =>
			ANFExprVal::UnaryNode(ANFUnary {
				op: u.op,
				expr: Box::new(anfify_expr(*u.expr))
			}),
		ExprVal::BinaryNode(b) =>
			ANFExprVal::BinaryNode(ANFBinary {
				op: b.op,
				left: Box::new(anfify_expr(*b.left.clone())),
				right: Box::new(anfify_expr(*b.right)),
			}),
		ExprVal::CallNode(c) => {
			let mut block = VecDeque::new();
			let mut trivial_args = VecDeque::new();
			for arg in c.args.iter() {
				trivial_args.push_back(match arg.val.clone() {
					ExprVal::LiteralNode(lit) => Trivial::Literal(lit),
					ExprVal::IdentNode(id) => Trivial::Ident(id),

					_ => {
						let id = unique_name();
						block.push_back(Box::new(
							ANFAST::LetNode(ANFLet {
								var:
									Variable {
										name: id.clone(),
										r#type: arg.r#type.clone()
									},
								def:
									Some(Box::new(
										anfify_expr(arg.clone())
									))
							})
						));
						Trivial::Ident(id)
					}
				});
			}
			block.push_back(
				Box::new(ANFAST::ExprNode(ANFExpr {
					val: ANFExprVal::CallNode(ANFCall {
						args: trivial_args,
						func: Box::new(anfify_expr(*c.func))
					}),
					r#type: e.r#type.clone()
				}))
			);

			ANFExprVal::BlockNode(block)
		}
	}
}