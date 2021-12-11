use unify::Constraint;
use ast::Type;
use tok::{
	OpID,
	OpID::*
};


impl OpID {
	pub fn associations(&self, left: Type, right: Type) -> Vec<Constraint> {
		match *self {
			Eq | Doeq | Noteq => vec![ Constraint::Equals(left.clone(), right.clone()) ],
			Gt | Lt | Gteq | Lteq => vec![ Constraint::Equals(left.clone(), right.clone()), Constraint::In(left.clone(), vec![ Type::Int, Type::Float ]) ],

			And | Or | Xor => vec![ Constraint::Equals(left.clone(), right.clone()), Constraint::Equals(left.clone(), Type::Bool) ],

			Add | Sub | Mul | Div => vec![ Constraint::Equals(left.clone(), right.clone()), Constraint::In(left, vec![ Type::Int, Type::Float ]) ],
			Mod => vec![ Constraint::Equals(left.clone(), right.clone()), Constraint::Equals(left.clone(), Type::Int) ],

			Concat => vec![ Constraint::Equals(left.clone(), right.clone()), Constraint::Equals(left.clone(), Type::Str) ],

			_ => panic!("Unary operator had a requested binary constraint")
		}
	}

	pub fn result(&self, left: Type, _right: Type) -> Type {
		match *self {
			Eq => Type::Void,

			Gt | Lt | Gteq | Lteq | Doeq | Noteq | And | Or | Xor => Type::Bool,

			Add | Sub | Mul | Div => left.clone(),
			Mod => Type::Int,

			Concat => Type::Str,

			_ => panic!("Unary operator had a requested binary result type")
		}
	}
}