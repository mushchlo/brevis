use std::collections::{
	VecDeque,
	HashSet,
	HashMap,
};

use crate::{
	error::ErrorMessage,
	types::{
		Type,
		TypeVarId,
		AggregateType,
		Constraint,
		solve_constraints,
		substitute,
		generalize
	},
	parse::get_type_var,
	lex::{
		tok::{
			TokenLiteral,
			LiteralVal,
			OpID,
			UOpID,
		},
		cradle::SourceLoc,
	},
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
	pub generics: HashMap<TypeVarId, Type>,
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
	pub declared: Pattern,
	pub def: Box<Expr>,
}

#[derive(Clone, Debug)]
pub enum Pattern {
	Empty(SourceLoc),
	Assignee(Parameter),
	Func {
		func: Parameter,
		args: Vec<Parameter>,
	},
	Struct(Vec<(String, Pattern)>),
	Literal(TokenLiteral),
}

impl Pattern {
	pub fn assignees(&self) -> Vec<&Parameter> {
		use self::Pattern::*;
		match self {
			Assignee(p) => vec![p],
			Func { func, .. } => vec![func],
			Struct(s) => {
				s.iter()
					.map(|(_, p)| p.assignees())
					.reduce(|mut acc, mut next| {
						acc.append(&mut next);
						acc
					})
					.unwrap_or_else(Vec::new)
			}
			Literal(_) | Empty(_) => vec![],
		}
	}

	pub fn assert_assignee(&self) -> &Parameter {
		if let Pattern::Assignee(p) = self {
			p
		} else {
			panic!("pattern {:#?} was asserted to be an assignee, but wasn't", self)
		}
	}

	pub fn generalize(&mut self) {
		use self::Pattern::*;
		match self {
			Empty(_) | Literal(_) => {},

			Assignee(p) => {
				p.r#type = generalize(p.r#type.clone());
			}

			Func { func, args } => {
				func.r#type = generalize(func.r#type.clone());
				for arg in args {
					arg.r#type = generalize(arg.r#type.clone());
				}
			}

			Struct(s) => {
				for (_, pattern) in s {
					pattern.generalize();
				}
			}
		}
	}

	pub fn infer_as(&mut self, t: Type, loc: SourceLoc, errors: &mut HashSet<ErrorMessage>) {
		let constraints = [self.constrain_as(t, loc)];
		let substitutions = solve_constraints(&constraints, errors);
		self.substitute_with(&substitutions);
	}

	pub fn constrain_as(&self, t: Type, loc: SourceLoc) -> Constraint {
		Constraint::Equal(
			(self.r#type(), self.loc()),
			(t, loc)
		)
	}

	fn substitute_with(&mut self, substitutions: &HashMap<TypeVarId, Type>) {
		use self::Pattern::*;
		match self {
			Empty(_) | Literal(_) => {}
			Assignee(p) => {
				p.r#type = substitute(substitutions, &p.r#type);
			}
			Func { func, args } => {
				func.r#type = substitute(substitutions, &func.r#type);
				for arg in args {
					arg.r#type = substitute(substitutions, &arg.r#type);
				}
			}
			Struct(s) => {
				for (_, pattern) in s {
					pattern.substitute_with(substitutions)
				}
			}
		}
	}

	fn r#type(&self) -> Type {
		use self::Pattern::*;
		match self {
			Assignee(p) => p.r#type.clone(),
			Func { .. } =>
				get_type_var(),
			Struct(s) =>
				Type::Struct(s.iter().map(|(name, p)|
					AggregateType {
						name: name.clone(),
						r#type: p.r#type()
					}
				).collect()),
			Literal(lit) =>
				match lit.val {
					LiteralVal::StrLit(_) => Type::Str,
					LiteralVal::IntLit(_) => Type::Int,
					LiteralVal::FltLit(_) => Type::Float,
					LiteralVal::BoolLit(_) => Type::Bool,
				},
			Empty(_) => get_type_var(),
		}
	}

	fn loc(&self) -> SourceLoc {
		use self::Pattern::*;
		match self {
			Assignee(p) => p.name_loc,
			Func { func, args } =>
				func.name_loc.join(args.last().map(|p| p.name_loc).unwrap_or_else(SourceLoc::nonexistent)),
			Struct(s) =>
				s.first().map(|(_, p)| p.loc()).unwrap()
					.join(s.last().map(|(_, p)| p.loc()).unwrap_or_else(SourceLoc::nonexistent)),
			Literal(lit) => lit.loc,
			Empty(loc) => *loc,
		}
	}
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
							call_stack.push((&mut *l.def, false));
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