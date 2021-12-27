use cradle::SourcePos;

#[derive(Debug, Clone)]
pub struct Token {
	pub val: TokenValue,
	pub start: SourcePos,
	pub end: SourcePos,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenValue {
	UnaryOp(OpID),
	BinaryOp(OpID),
	AssignOp(OpID),
	KeyWord(KeyWord),
	Ident(String),
	Punc(char),
	Literal(TokenLiteral),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenLiteral {
	StrLit(String),
	IntLit(i64),
	FltLit(f64),
	BoolLit(bool),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum OpID {
	// assignment
	Eq,

	// lookup member of structure
	Member,

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
	(Member, "."),
];
