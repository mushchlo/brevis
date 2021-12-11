#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum OpID {
	// assignment
	Eq,

	// string concatenation
	Concat,

	// the only unaries (hate these)
	Not,
	Minus,

	// mathematical operators
	Add, // Int, Int => Int / Flt, Flt => Flt
	Sub,
	Mul,
	Div,
	Mod,

	// truthiness operators
	Gt,
	Lt,
	Gteq,
	Lteq,
	Doeq,
	Noteq,
	And,
	Or,
	Xor,

	InfixFn,
}
use self::OpID::*;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum KeyWord {
	Let,
	If,
	Else,
	Int,
	Float,
	String,
	Union,
	Struct,
	#[allow(non_camel_case_types)]
	λ,
}

pub const KEYWORD_DICT: &[(KeyWord, &str)] = &[
	(KeyWord::Let, "let"),
	(KeyWord::If, "if"),
	(KeyWord::Else, "else"),
	(KeyWord::Int, "int"),
	(KeyWord::Float, "float"),
	(KeyWord::String, "string"),
	(KeyWord::Union, "union"),
	(KeyWord::Struct, "struct"),
	(KeyWord::λ, "λ"),
	(KeyWord::λ, "fn"),
];

pub const BINARY_OP_DICT: &[(OpID, &str)] = &[
	(Add, "+"),
	(Sub, "-"),
	(Mul, "*"),
	(Div, "/"),
	(Mod, "%"),
	(Concat, "~"),
	(Gt, ">"),
	(Lt, "<"),
	(Gteq, ">="),
	(Lteq, "<="),
	(Doeq, "=="),
	(Noteq, "!="),
	(InfixFn, "`"),
];

use crate::ast::Type;

pub fn binary_op_result_type(op: OpID, t_left: Type, t_right: Type) -> Type {
	match op {
		Eq | Add | Sub | Mul | Div | Mod | Concat => t_left,
		Doeq | Noteq | Gt | Lt | Gteq | Lteq | And | Or | Xor => Type::Bool,

		_ => panic!("Operator {:?} does not apply to types {:#?} and {:#?}", op, t_left, t_right)
	}
}
