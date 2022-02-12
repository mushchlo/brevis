use std::collections::{
	VecDeque,
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
pub enum AST {
	ExprNode(Expr),
	LetNode(Let),
}

#[derive(Clone, Debug)]
pub struct Expr {
	pub val: ExprVal,
	pub loc: SourceLoc,
	pub r#type: Type,
}

#[derive(Clone, Debug)]
pub enum ExprVal {
	LiteralNode(Literal),

	VarNode(Variable),
	BlockNode(VecDeque<AST>),
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
	pub generics: Vec<u16>,
	pub captured: Vec<Variable>,
	pub body: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq, std::cmp::Eq, Hash)]
pub struct Parameter {
	pub name: String,
	pub r#type: Type,
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

#[derive(Clone, Debug, PartialEq, std::cmp::Eq, Hash, PartialOrd, Ord)]
pub struct GenericType {
	pub generics: Vec<u16>, // list of typevars
	pub uninstantiated: Type, // a type that may contain those typevars
}

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
	TypeConstructor(TConstructor),

	TypeVar(u16),
}

#[derive(Clone, Debug, PartialEq, std::cmp::Eq, Hash, PartialOrd, Ord)]
pub struct AggregateType {
	pub name: String,
	pub r#type: Type,
}

#[derive(Clone, Debug, PartialEq, std::cmp::Eq, Hash, PartialOrd, Ord)]
pub struct TConstructor {
	pub name: String,
	pub args: Vec<Type>,
}


impl GenericType {
	pub fn new(t: Type) -> Self {
		GenericType {
			uninstantiated: t,
			generics: Vec::new(),
		}
	}
}
