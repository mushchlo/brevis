#[derive(Debug, Copy, Clone, PartialEq)]
enum OpID {
/* assignment */
	Eq,

/* the only unaries (hate these) */
	Not,
	Minus,

/* mathematical operators */
	Add,
	Sub,
	Mul,
	Div,

/* truthiness operators*/
	Gt,
	Lt,
	Gteq,
	Lteq,
	Doeq,
	Noteq,
	And,
	Or,
	Xor
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum KeyWord {
	Return,
	Let,
	If,
	Else,
	Int,
	Float,
	String,
	Union,
	Struct,
#[allow(non_camel_case_types)]
	λ
}

const KEYWORD_DICT: &[(KeyWord, &str)] = &[
	(KeyWord::Return,	"return"),
	(KeyWord::Let,		"let"),
	(KeyWord::If,		"if"),
	(KeyWord::Else,		"else"),
	(KeyWord::Int,		"int"),
	(KeyWord::Float,	"float"),
	(KeyWord::String,	"string"),
	(KeyWord::Union,	"union"),
	(KeyWord::Struct,	"struct"),
	(KeyWord::λ,		"λ"),
	(KeyWord::λ,		"fn")
];

const BINARY_OP_DICT: &[(OpID, &'static str)] = &[
	(Add, "+"),
	(Sub, "-"),
	(Mul, "*"),
	(Div, "/"),

	(Gt, ">"),
	(Lt, "<"),
	(Gteq, ">="),
	(Lteq, "<="),
	(Doeq, "=="),
	(Noteq, "!=")
];