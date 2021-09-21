use std::collections::VecDeque;

use crate::{lex::TokenLiteral, tok::OpID};

#[derive(Clone, Debug)]
pub enum AST {
	ExprNode(Expr),
	LetNode(Let),
}

#[derive(Clone, Debug)]
pub struct Expr {
	pub val: ExprVal,
	pub r#type: Type,
}

#[derive(Clone, Debug)]
pub enum ExprVal {
	LiteralNode(TokenLiteral),

	IdentNode(String),
	BlockNode(VecDeque<Box<AST>>),
	LambdaNode(Lambda),
	IfNode(IfElse),

	UnaryNode(Unary),
	BinaryNode(Binary),
	CallNode(Call),
}

#[derive(Clone, Debug)]
pub struct Binary {
	pub op: OpID,
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
	pub op: OpID,
	pub expr: Box<Expr>,
}

#[derive(Clone, Debug)]
pub struct Lambda {
	pub args: VecDeque<Variable>,
	pub body: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq, std::cmp::Eq, Hash)]
pub struct Variable {
	pub name: String,
	pub r#type: Type,
}

#[derive(Clone, Debug)]
pub struct Call {
	pub func: Box<Expr>,
	pub args: VecDeque<Expr>,
}

#[derive(Clone, Debug)]
pub struct Let {
	pub var: Variable,
	pub def: Option<Box<Expr>>,
}

#[derive(Clone, Debug, PartialEq, std::cmp::Eq, Hash)]
pub enum Type {
	Void,
	Int,
	Float,
	Str,
	Bool,

	// Union(Vec<Aggregate>),
	// Struct(Vec<Aggregate>),
	TypeConstructor(TConstructor),

	TypeVar(u16),
}

#[derive(Clone, Debug, PartialEq, std::cmp::Eq, Hash)]
pub struct TConstructor {
	pub name: String,
	pub args: Vec<Type>,
}

#[derive(Clone, Debug)]
pub struct FuncType {
	args: Vec<Type>,
	returned: Box<Type>,
}
