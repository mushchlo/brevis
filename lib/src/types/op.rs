use crate::{
	types::{
		Type,
		Type::*,
		AggregateType,
		unify::Constraint,
	},
	lex::tok::{
		OpID::*,
		UOpID::*,
	},
	parse::{
		get_type_var,
		ast::{
			Unary,
			Binary,
			Expr,
			ExprVal,
		},
	},
};


impl Binary {
	pub fn associations(&self, result: &Expr) -> Vec<Constraint> {
		let (left, right) = (
			(self.left.r#type.clone(), self.left.loc),
			(self.right.r#type.clone(), self.right.loc)
		);
		let mut ret = match self.op {
			Eq | Doeq | Noteq => vec![ Constraint::Equal(left, right) ],
			Gt | Lt | Gteq | Lteq => vec![
				Constraint::Equal(left, right),
			],

			And | Or | Xor => vec![
				Constraint::Equal(left, right),
			],

			Add | Sub | Mul | Div => vec![
				Constraint::Equal(left, right),
			],

			Mod => vec![
				Constraint::Equal(left, right.clone()),
				Constraint::Equal(right, (Int, self.op_loc))
			],

			Concat => vec![
				Constraint::Equal(left.clone(), right),
				Constraint::Equal(left, (Str, self.op_loc))
			],

			Member => {
				let new_right = match &self.right.val {
					ExprVal::VarNode(s) => (
						AggregateType {
							name: s.name.clone(),
							r#type: right.0,
						},
						right.1
					),
					_ => panic!("unreachable")
				};

				vec![
					Constraint::HasMember(left, new_right)
				]
			}

			InfixFn => panic!("no"),
		};
		ret.push(self.result_constraint(result));

		ret
	}

	pub fn result_constraint(&self, result: &Expr) -> Constraint {
		let result_t = match self.op {
			Eq => Type::Void,

			Gt | Lt | Gteq | Lteq | Doeq | Noteq | And | Or | Xor => Type::Bool,
			Add | Sub | Mul | Div => self.left.r#type.clone(),
			Mod => Type::Int,
			Concat => Type::Str,
			Member => self.right.r#type.clone(),

			InfixFn => panic!("no")
		};

		Constraint::Equal(
			(result_t, result.loc),
			(result.r#type.clone(), self.op_loc),
		)
	}
}

impl Unary {
	pub fn associations(&self, result: &Expr) -> Vec<Constraint> {
		let (operand_t, operand_loc) = (self.expr.r#type.clone(), self.expr.loc);
		let mut ret = match self.op {
			Not =>
				vec![
					Constraint::Equal(
						(operand_t, operand_loc),
						(Type::Bool, self.op_loc)
					),
				],
			Neg =>
				vec![],
			At =>
				vec![
					Constraint::Equal(
						(operand_t, operand_loc),
						(Type::Pointer(box result.r#type.clone(), false), self.op_loc)
					),
				],
			Ref(_) => vec![],
		};

		ret.push(self.result_constraint(result));

		ret
	}

	pub fn result_constraint(&self, result: &Expr) -> Constraint {
		let result_t = match self.op {
			Not => Type::Bool,
			Neg => self.expr.r#type.clone(),
			At => get_type_var(),
			Ref(mutable) => Type::Pointer(box self.expr.r#type.clone(), mutable),
		};

		Constraint::Equal(
			(result_t, self.op_loc),
			(result.r#type.clone(), result.loc)
		)
	}
}

