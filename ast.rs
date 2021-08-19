#[derive(Clone, Debug)]
enum AST {
	ExprNode(Expr),
	LetNode(Let)
}

#[derive(Clone, Debug)]
struct Expr {
	val: ExprVal,
	r#type: Type
}

#[derive(Clone, Debug)]
enum ExprVal {
	LiteralNode(TokenLiteral),

	IdentNode(Variable),
	BlockNode(VecDeque<Box<AST>>),
	LambdaNode(Lambda),
	IfNode(IfElse),

	UnaryNode(Unary),
	BinaryNode(Binary),
	CallNode(Call)
}

#[derive(Clone, Debug)]
struct Binary {
	op: OpID,
	left: Box<AST>,
	right: Box<AST>
}

#[derive(Clone, Debug)]
struct IfElse {
	cond: Box<AST>,
	then: Box<AST>,
	r#else: Option<Box<AST>>
}

#[derive(Clone, Debug)]
struct Unary {
	op: OpID,
	expr: Box<AST>
}

#[derive(Clone, Debug)]
struct Lambda {
	args: VecDeque<Variable>,
	body: Box<AST>
}

#[derive(Clone, Debug)]
struct Variable {
	name: String,
	r#type: Type
}

#[derive(Clone, Debug)]
struct Call {
	func: Box<AST>,
	args: VecDeque<AST>
}

#[derive(Clone, Debug)]
struct Let {
	var: Variable,
	def: Option<Box<AST>>
}

#[derive(Clone, Debug)]
enum Type {
	Void,
	Int,
	Float,
	Str,

/*
	Union(Vec<Aggregate>),
	Struct(Vec<Aggregate>),*/

	TypeConstructor((TConstructorVariant, Vec<Type>)),

	TypeVar(u16)
}

#[derive(Clone, Debug)]
struct TConstructor {
	name: TConstructorVariant,
	args: Vec<Type>
}

#[derive(Clone, Debug)]
enum TConstructorVariant {
	Fn
}

/*
#[derive(Clone, Debug)]
struct Aggregate {
	tag: String,
	valtype: Box<Type>
}*/

#[derive(Clone, Debug)]
struct FuncType {
	args: Vec<Type>,
	returned: Box<Type>
}