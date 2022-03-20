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
	Literal(Literal),

	Let(Let),
	Var(Variable),
	Block(VecDeque<Expr>),
	Lambda(Lambda),
	If(IfElse),

	Unary(Unary),
	Binary(Binary),
	Call(Call),
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
					LiteralVal::Str(_) => Type::Str,
					LiteralVal::Int(_) => Type::Int,
					LiteralVal::Flt(_) => Type::Float,
					LiteralVal::Bool(_) => Type::Bool,
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
		macro_rules! recurse {
			($to_trans:expr) => {
				$to_trans.transform(trans, post_trans)
			}
		}

		if trans(self) {
			match &mut self.val {
			// Dead ends, all done!
				ExprVal::Literal(Literal::AtomicLiteral(_)) | ExprVal::Var(_) => {},

				ExprVal::Literal(Literal::StructLiteral(s)) => {
					for agg in s {
						recurse!(&mut agg.val);
					}
				}

				ExprVal::Block(b) => {
					for line in b.iter_mut() {
						recurse!(line);
					}
				}

				ExprVal::Let(l) => {
					recurse!(&mut l.def);
				}

				ExprVal::Lambda(lam) => {
					recurse!(&mut lam.body);
				}

				ExprVal::If(ifelse) => {
					recurse!(&mut ifelse.cond);
					recurse!(&mut ifelse.then);
					if let Some(box e) = &mut ifelse.r#else {
						recurse!(e);
					}
				}

				ExprVal::Unary(u) => {
					recurse!(&mut u.expr);
				}

				ExprVal::Binary(b) => {
					recurse!(&mut b.right);
					recurse!(&mut b.left);
				}

				ExprVal::Call(c) => {
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

		match &self.val {
			Literal(AtomicLiteral(atom)) =>
				match &atom.val {
					LiteralVal::Int(_) => "an integer literal",
					LiteralVal::Str(_) => "a string literal",
					LiteralVal::Flt(_) => "a floating-point literal",
					LiteralVal::Bool(_) => "a boolean literal",
				},
			Literal(StructLiteral(_)) => "a structure literal",
			Let(_) => "a declaration",
			Var(_) => "a variable",
			Block(_) => "a block",
			Lambda(_) => "a function",
			If(ifelse) if ifelse.r#else.is_some() => "an if-else-expression",
			If(_) => "an if-expression",
			Unary(_) => "a unary expression",
			Binary(_) => "a binary expression",
			Call(_) => "a function call",
		}
	}

// Returns references to the shallowest amount of descendents,
// in visiting order when popping from the stack.
	fn descendents(&self) -> Vec<&Self> {
		match &self.val {
			// Dead ends, all done!
			ExprVal::Literal(Literal::AtomicLiteral(_)) | ExprVal::Var(_) => Vec::new(),

			ExprVal::Literal(Literal::StructLiteral(s)) => s.iter().map(|agg| &agg.val).collect(),

			ExprVal::Block(b) => b.iter().rev().collect(),

			ExprVal::Let(l) => vec![ &l.def ],

			ExprVal::Lambda(lam) => vec![ &lam.body ],

			ExprVal::If(ifelse) =>
				IntoIterator::into_iter([ ifelse.cond.as_ref(), ifelse.then.as_ref() ])
					.chain(ifelse.r#else.as_ref().iter().map(|&e| e.as_ref()))
					.collect(),

			ExprVal::Unary(u) => vec![ &u.expr ],

			ExprVal::Binary(b) => vec![ &b.right, &b.left ],

			ExprVal::Call(c) =>
				iter::once(&*c.func)
					.chain(c.args.iter())
					.collect(),
		}
	}
}
