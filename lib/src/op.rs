use unify::Constraint;
use ast::{
	Unary,
	Binary,
	ExprVal,
	Type,
	Type::*,
	AggregateType,
};
use tok::{
	OpID,
	OpID::*,
	UOpID,
	UOpID::*,
};
use parse::get_type_var;


impl Binary {
	pub fn associations(&self) -> Vec<Constraint> {
		let (left_t, right_t) = (self.left.r#type.clone(), self.right.r#type.clone());
		match self.op {
			Eq | Doeq | Noteq => vec![ Constraint::Equal(left_t, right_t) ],
			Gt | Lt | Gteq | Lteq => vec![
				Constraint::Equal(left_t, right_t),
			],

			And | Or | Xor => vec![
				Constraint::Equal(left_t, right_t),
			],

			Add | Sub | Mul | Div => vec![
				Constraint::Equal(left_t, right_t),
			],

			Mod => vec![
				Constraint::Equal(left_t, right_t.clone()),
				Constraint::Equal(right_t, Int)
			],

			Concat => vec![
				Constraint::Equal(left_t.clone(), right_t),
				Constraint::Equal(left_t, Str)
			],

			Member => {
				let name = match &self.right.val {
					ExprVal::VarNode(s) => s.name.clone(),
					_ => panic!("unreachable")
				};

				vec![
					Constraint::HasMember(
						left_t,
						AggregateType {
							name,
							r#type: right_t,
						}
					)
				]
			}

			InfixFn => panic!("no"),
		}
	}
}

impl OpID {
	pub fn result(&self, left: Type, right: Type) -> Type {
		match *self {
			Eq => Type::Void,

			Gt | Lt | Gteq | Lteq | Doeq | Noteq | And | Or | Xor => Type::Bool,

			Add | Sub | Mul | Div => left,
			Mod => Type::Int,

			Concat => Type::Str,

			Member => right,

			_ => panic!("Unary operator had a requested binary result type")
		}
	}
}

impl Unary {
	pub fn associations(&self, expr_t: Type) -> Vec<Constraint> {
		let operand_t = self.expr.r#type.clone();
		match self.op {
			Not =>
				vec![
					Constraint::Equal(operand_t, Type::Bool),
				],
			Neg =>
				vec![],
			At =>
				vec![
					Constraint::Equal(
						Type::Pointer(box expr_t),
						operand_t
					),
				],
			Ref => vec![],
		}
	}
}

impl UOpID {
	pub fn result(&self, operand_t: Type) -> Type {
		match *self {
			Not => Type::Bool,
			Neg => operand_t,
			At => get_type_var(),
			Ref => Type::Pointer(box operand_t),
		}
	}
}

