use crate::lex::cradle::SourceLoc;

#[derive(Debug, Clone)]
pub struct Token {
	pub val: TokenValue,
	pub loc: SourceLoc,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenValue {
	UnaryOp(UOpID),
	BinaryOp(OpID),
	AssignOp(OpID),
	KeyWord(KeyWord),
	Ident(String),
	Punc(char),
	Literal(TokenLiteral),
}

#[derive(Debug, PartialEq, Clone)]
pub struct TokenLiteral {
	pub val: LiteralVal,
	pub loc: SourceLoc,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralVal {
	Str(String),
	Int(i64),
	Flt(f64),
	Bool(bool),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum UOpID {
	Not, // !
	Neg, // -
	At,  // @
	Ref(bool), // & and &mut
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum OpID {
	// assignment
	Eq,

	// string concatenation
	Concat,

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
	Mut,
	If,
	Else,
// TODO: eventually, the unit type will be empty struct. it is dumb to have two unit values.
	Void,
	Bool,
	Int,
	Float,
	String,
	Union,
	Struct,
	Arrow,
	Dot,
	#[allow(non_camel_case_types)]
	λ,
}

pub const KEYWORD_DICT: &[(KeyWord, &str)] = &[
	(KeyWord::Let, "let"),
	(KeyWord::Mut, "mut"),
	(KeyWord::If, "if"),
	(KeyWord::Else, "else"),
	(KeyWord::Void, "void"),
	(KeyWord::Bool, "bool"),
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
