use std::collections::{
	VecDeque,
	HashSet,
};

use crate::lex::{
	tok::{
		TokenLiteral,
		OpID,
		UOpID,
	},
	cradle::SourceLoc,
};

#[derive(Clone, Debug)]
pub struct Expr {
	pub val: ExprVal,
	pub loc: SourceLoc,
	pub r#type: Type,
}

#[derive(Clone, Debug)]
pub enum ExprVal {
	LiteralNode(Literal),

	LetNode(Let),
	VarNode(Variable),
	BlockNode(VecDeque<Expr>),
	LambdaNode(Lambda),
	IfNode(IfElse),

	UnaryNode(Unary),
	BinaryNode(Binary),
	CallNode(Call),
}

#[derive(Clone, Debug)]
pub enum Literal {
	StructLiteral(Vec<Aggregate>),
	AtomicLiteral(TokenLiteral),
}

#[derive(Clone, Debug)]
pub struct Aggregate {
	pub name: String,
	pub val: Expr,
}

#[derive(Clone, Debug)]
pub struct Variable {
	pub name: String,
	pub generics: Vec<Type>,
}


// TODO: make member accesses NOT binary structures, but
// their own thing.
#[derive(Clone, Debug)]
pub struct Binary {
	pub op: OpID,
	pub op_loc: SourceLoc,
	pub left: Box<Expr>,
	pub right: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct IfElse {
	pub cond: Box<Expr>,
	pub then: Box<Expr>,
	pub r#else: Option<Box<Expr>>,
}

#[derive(Clone, Debug)]
pub struct Unary {
	pub op: UOpID,
	pub op_loc: SourceLoc,
	pub expr: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct Lambda {
	pub args: VecDeque<Parameter>,
	pub captured: HashSet<Parameter>,
	pub body: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq, std::cmp::Eq, Hash)]
pub struct Parameter {
	pub name: String,
	pub r#type: Type,
	pub mutable: bool,
	pub name_loc: SourceLoc,
	pub type_loc: Option<SourceLoc>,
}

#[derive(Clone, Debug)]
pub struct Call {
	pub func: Box<Expr>,
	pub args: VecDeque<Expr>,
}

#[derive(Clone, Debug)]
pub struct Let {
	pub var: Parameter,
	pub def: Option<Box<Expr>>,
}

pub type TypeVarId = usize;

#[derive(Clone, Debug, PartialEq, std::cmp::Eq, Hash, PartialOrd, Ord)]
pub enum Type {
	Void,
	Int,
	Float,
	Str,
	Bool,

	// Union(Vec<Aggregate>),
	Struct(Vec<AggregateType>),
	Pointer(Box<Type>),
// The argument types, ending with the return type
	Func(Vec<Type>),

// A type generic over these arguments
	Forall(Vec<TypeVarId>, Box<Type>),

	TypeVar(TypeVarId),
}

#[derive(Clone, Debug, PartialEq, std::cmp::Eq, Hash, PartialOrd, Ord)]
pub struct AggregateType {
	pub name: String,
	pub r#type: Type,
}

impl Expr {
// Transforms an expression recursively, with given functions.
// A nice helper for functions that really just need to make small
// modifications to the AST, without removing anything.
// Trans returns a boolean, indicating if transform
// should recurse further into this node, and if so, post_trans
// is also run after the recursion.
	pub fn transform<E, T>(&mut self, mut trans: E, mut post_trans: T)
	where E: for<'r> FnMut(&'r mut Expr) -> bool,
		T: for<'r> FnMut(&'r mut Expr)
	{
		use self::ExprVal::*;

	// the boolean marks "are we exiting?," i.e. do we run post_trans?
		let mut call_stack: Vec<(&mut Expr, bool)> = vec![ (self, false) ];

		while let Some((current, exiting)) = call_stack.pop() {
			if exiting {
				post_trans(current);
			} else if trans(current) {
				unsafe {
					call_stack.push((&mut *(current as *mut Expr), true));
					match &mut current.val {
					// Dead ends, all done!
						LiteralNode(Literal::AtomicLiteral(_)) | VarNode(_) => {},

						LiteralNode(Literal::StructLiteral(ref mut s)) => {
							for agg in s {
								call_stack.push((&mut agg.val, false));
							}
						}

						BlockNode(ref mut b) => {
							for line in b.iter_mut().rev() {
								call_stack.push((line, false));
							}
						}

						LetNode(ref mut l) => {
							if let Some(box def) = &mut l.def {
								call_stack.push((def, false));
							}
						}

						LambdaNode(ref mut lam) => {
							call_stack.push((&mut lam.body, false));
						}

						IfNode(ref mut ifelse) => {
							call_stack.push((&mut ifelse.cond, false));
							call_stack.push((&mut ifelse.then, false));
							if let Some(ref mut e) = ifelse.r#else {
								call_stack.push((e, false));
							}
						}

						UnaryNode(ref mut u) => {
							call_stack.push((&mut u.expr, false));
						}

						BinaryNode(ref mut b) => {
							call_stack.push((&mut b.right, false));
							call_stack.push((&mut b.left, false));
						}

						CallNode(ref mut c) => {
							call_stack.push((&mut *c.func, false));
							for arg in &mut c.args {
								call_stack.push((arg, false));
							}
						}
					}
				}
			}
		}
	}
}