use std::{
	collections::{
		VecDeque,
		HashSet,
		HashMap,
	},
	iter,
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
// modifications to the AST.
// Trans returns a boolean, indicating if transform
// should recurse further into this node, and if so, post_trans
// is also run after the recursion.
	pub fn transform<E, T>(&mut self, mut trans: E, mut post_trans: T)
	where E: Copy + FnMut(&mut Self) -> bool,
		T: Copy + FnMut(&mut Self)
	{
		use self::ExprVal::*;

		macro_rules! recurse {
			($to_trans:expr) => {
				$to_trans.transform(trans, post_trans)
			}
		}

		if trans(self) {
			match &mut self.val {
			// Dead ends, all done!
				LiteralNode(Literal::AtomicLiteral(_)) | VarNode(_) => {},

				LiteralNode(Literal::StructLiteral(s)) => {
					for agg in s {
						recurse!(&mut agg.val);
					}
				}

				BlockNode(b) => {
					for line in b.iter_mut() {
						recurse!(line);
					}
				}

				LetNode(l) => {
					recurse!(&mut l.def);
				}

				LambdaNode(lam) => {
					recurse!(&mut lam.body);
				}

				IfNode(ifelse) => {
					recurse!(&mut ifelse.cond);
					recurse!(&mut ifelse.then);
					if let Some(box e) = &mut ifelse.r#else {
						recurse!(e);
					}
				}

				UnaryNode(u) => {
					recurse!(&mut u.expr);
				}

				BinaryNode(b) => {
					recurse!(&mut b.right);
					recurse!(&mut b.left);
				}

				CallNode(c) => {
					recurse!(&mut c.func);
					for arg in &mut c.args {
						recurse!(arg);
					}
				}
			}
		}

		post_trans(self);
	}

	pub fn visit<A, B>(&self, mut pre_visit: B, mut post_visit: A)
	where B: FnMut(&Self) -> bool,
		A: FnMut(&Self)
	{
		let mut to_visit = vec![ self ];
		let mut visited: HashSet<*const Expr> = HashSet::new();
		while let Some(ex) = to_visit.pop() {
			if visited.contains(&(ex as *const Expr)) {
				post_visit(ex);
			} else if pre_visit(ex) {
				to_visit.push(ex);
				to_visit.append(&mut ex.descendents());
				visited.insert(ex as *const Expr);
			}
		}
	}

	// Used in error messages like "Attempted to assign to _"
	pub fn describe(&self) -> &str {
		use self::ExprVal::*;
		use self::Literal::*;
		use lex::tok::LiteralVal::*;

		match &self.val {
			LiteralNode(AtomicLiteral(atom)) =>
				match &atom.val {
					IntLit(_) => "an integer literal",
					StrLit(_) => "a string literal",
					FltLit(_) => "a floating-point literal",
					BoolLit(_) => "a boolean literal",
				},
			LiteralNode(StructLiteral(_)) => "a structure literal",
			LetNode(_) => "a declaration",
			VarNode(_) => "a variable",
			BlockNode(_) => "a block",
			LambdaNode(_) => "a function",
			IfNode(ifelse) if ifelse.r#else.is_some() => "an if-else-expression",
			IfNode(_) => "an if-expression",
			UnaryNode(_) => "a unary expression",
			BinaryNode(_) => "a binary expression",
			CallNode(_) => "a function call",
		}
	}

// Returns references to the shallowest amount of descendents,
// in visiting order when popping from the stack.
	fn descendents(&self) -> Vec<&Self> {
		use self::ExprVal::*;

		match &self.val {
			// Dead ends, all done!
			LiteralNode(Literal::AtomicLiteral(_)) | VarNode(_) => Vec::new(),

			LiteralNode(Literal::StructLiteral(s)) => s.iter().map(|agg| &agg.val).collect(),

			BlockNode(b) => b.iter().rev().collect(),

			LetNode(l) => vec![ &l.def ],

			LambdaNode(lam) => vec![ &lam.body ],

			IfNode(ifelse) =>
				IntoIterator::into_iter([ ifelse.cond.as_ref(), ifelse.then.as_ref() ])
					.chain(ifelse.r#else.as_ref().iter().map(|&e| e.as_ref()))
					.collect(),

			UnaryNode(u) => vec![ &u.expr ],

			BinaryNode(b) => vec![ &b.right, &b.left ],

			CallNode(c) =>
				iter::once(&*c.func)
					.chain(c.args.iter())
					.collect(),
		}
	}
}
