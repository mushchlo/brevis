#[derive(Debug, Copy, Clone, PartialEq)]
pub enum OpID {
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
    Xor,
}
use OpID::*;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum KeyWord {
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
    λ,
}

pub const KEYWORD_DICT: &[(KeyWord, &str)] = &[
    (KeyWord::Return, "return"),
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
    (Gt, ">"),
    (Lt, "<"),
    (Gteq, ">="),
    (Lteq, "<="),
    (Doeq, "=="),
    (Noteq, "!="),
];
