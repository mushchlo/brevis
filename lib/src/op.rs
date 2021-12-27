use unify::Constraint;
use ast::{
	Binary,
	ExprVal,
	Type,
	AggregateType,
};
use tok::{
	OpID,
	OpID::*
};


impl Binary {
	pub fn associations(&self) -> Vec<Constraint> {
		let (left_t, right_t) = (self.left.r#type.clone(), self.right.r#type.clone());
		match self.op {
			Eq | Doeq | Noteq => vec![ Constraint::Equal(left_t, right_t) ],
			Gt | Lt | Gteq | Lteq => vec![
				Constraint::Equal(left_t, right_t),
//				Constraint::In(left_t.clone(), vec![ Type::Int, Type::Float ])
			],

			And | Or | Xor => vec![
				Constraint::Equal(left_t.clone(), right_t),
				Constraint::Equal(left_t, Type::Bool)
			],

			Add | Sub | Mul | Div => vec![
				Constraint::Equal(left_t, right_t),
//				Constraint::In(left_t.clone(), vec![ Type::Int, Type::Float ])
			],

			Mod => vec![
				Constraint::Equal(left_t, right_t.clone()),
				Constraint::Equal(right_t, Type::Int)
			],

			Concat => vec![
				Constraint::Equal(left_t.clone(), right_t),
				Constraint::Equal(left_t, Type::Str)
			],

			Member => {
				let name = match &self.right.val {
					ExprVal::IdentNode(s) => s.clone(),
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

			_ => panic!("Unary operator had a requested binary constraint")
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