extern crate rand;

use std::iter;
use std::collections::{HashMap, VecDeque};
use rand::{Rng, thread_rng, distributions::Alphanumeric};
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

pub struct ANFTransformer {
	env: Vec<HashMap<String, Type>>,
}

impl ANFTransformer {
	pub fn new() -> Self {
		ANFTransformer {
			env: vec![],
		}
	}

	fn type_of(&self, id: String) -> Option<Type> {
		for map in &self.env {
			if map.contains_key(&id) {
				return map.get(&id).cloned();
			}
		}

		None
	}

	fn unique_name(&self) -> String {
		let mut rng = thread_rng();
		let name = format!("tmp_{}", iter::repeat(())
										.map(|()| rng.sample(Alphanumeric))
										.map(char::from)
										.take(7)
										.collect::<String>());
		if self.type_of(name.clone()).is_some() {
			return self.unique_name();
		}
		name
	}

	pub fn anfify(&mut self, a: AST) -> ANFAST {
		match a {
			AST::LetNode(l) =>
				ANFAST::LetNode(ANFLet {
					var: l.var.clone(),
					def:
						if let Some(box def) = l.def {
							Some(Box::new(self.anfify_expr(def)))
						} else { None }
				}),
			AST::ExprNode(e) =>
				ANFAST::ExprNode(ANFExpr {
					r#type: e.r#type.clone(),
					val: self.anfify_expr(e),
				}),
		}
	}
	
	/// Reorders an AST in administrative normal form
	pub fn anfify_expr(&mut self, e: Expr) -> ANFExprVal {
		if matches!(e.val.clone(), ExprVal::BlockNode(_) | ExprVal::LambdaNode(_)) {
			self.env.push(HashMap::new());
		}
		match e.val {
			ExprVal::LiteralNode(lit) => ANFExprVal::LiteralNode(lit),
			ExprVal::IdentNode(id) => {
				self.env.last_mut().unwrap().insert(id.clone(), e.r#type);
				ANFExprVal::IdentNode(id)
			}
			ExprVal::IfNode(i) =>
				ANFExprVal::IfNode(ANFIfElse {
					cond: Box::new(self.anfify_expr(*i.cond)),
					then: Box::new(self.anfify_expr(*i.then)),
					r#else: i.r#else.map(|box e| box self.anfify_expr(e)),
				}),
			ExprVal::BlockNode(b) => {
				let ret = ANFExprVal::BlockNode(
					b.iter().map(|box line| Box::new(self.anfify(line.clone()))).collect()
				);
				self.env.pop();
				ret
			}
			ExprVal::LambdaNode(l) => {
				for v in &l.args {
					self.env.last_mut().unwrap().insert(v.name.clone(), v.r#type.clone());
				}
				let ret = ANFExprVal::LambdaNode(ANFLambda {
					args: l.args.clone(),
					body: Box::new(self.anfify_expr(*l.body))
				});
				self.env.pop();
				ret
			}
			ExprVal::UnaryNode(u) =>
				ANFExprVal::UnaryNode(ANFUnary {
					op: u.op,
					expr: Box::new(self.anfify_expr(*u.expr))
				}),
			ExprVal::BinaryNode(b) =>
				ANFExprVal::BinaryNode(ANFBinary {
					op: b.op,
					left: Box::new(self.anfify_expr(*b.left.clone())),
					right: Box::new(self.anfify_expr(*b.right)),
				}),
			ExprVal::CallNode(c) => {
				let mut block = VecDeque::new();
				let mut trivial_args = VecDeque::new();
				for arg in c.args.iter() {
					trivial_args.push_back(match arg.val.clone() {
						ExprVal::LiteralNode(lit) => Trivial::Literal(lit),
						ExprVal::IdentNode(id) => Trivial::Ident(id),
	
						_ => {
							let id = self.unique_name();
							block.push_back(Box::new(
								ANFAST::LetNode(ANFLet {
									var:
										Variable {
											name: id.clone(),
											r#type: arg.r#type.clone()
										},
									def:
										Some(Box::new(
											self.anfify_expr(arg.clone())
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
							func: Box::new(self.anfify_expr(*c.func))
						}),
						r#type: e.r#type.clone()
					}))
				);
	
				ANFExprVal::BlockNode(block)
			}
		}
	}
}