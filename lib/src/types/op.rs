use crate::{
	types::{
		Type,
		Type::*,
		AggregateType,
		unify::Constraint,
		Mutability,
		get_type_var,
		get_type_var_id,
	},
	lex::{
		tok::{OpID, OpID::*, UOpID, UOpID::*},
		cradle::SourceLoc,
	},
	parse::ast::{Expr, ExprVal},
};


impl OpID {
	pub fn associations(
		&self,
		op_loc: SourceLoc,
		left_ex: &Expr,
		right_ex: &Expr,
		result: &Expr
	) -> Vec<Constraint> {
		let (left, right) = ((left_ex.r#type.clone(), left_ex.loc), (right_ex.r#type.clone(), right_ex.loc));
		let mut ret = match self {
			Eq =>
				match &left_ex.val {
					ExprVal::Unary { op: At, expr, .. } =>
						vec![
							Constraint::Equal(left, right.clone()),
							Constraint::Equal(
								(expr.r#type.clone(), expr.loc),
								(Pointer(box right.0, Mutability::Mutable), right.1)
							)
						],
					_ => vec![ Constraint::Equal(left, right) ],
				},
			Doeq | Noteq => vec![ Constraint::Equal(left, right) ],
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
				Constraint::Equal(right, (Int, op_loc))
			],

			Concat => vec![
				Constraint::Equal(left.clone(), right),
				Constraint::Equal(left, (Str, op_loc))
			],

			Member => {
				let new_right = match &right_ex.val {
					ExprVal::Var(s) => (
						AggregateType {
							name: s.name.clone(),
							r#type: right.0.clone(),
						},
						right.1
					),
					_ => panic!("unreachable")
				};

				vec![
					Constraint::HasMember((left_ex.r#type.clone(), left_ex.loc), new_right)
				]
			}

			InfixFn => panic!("no"),
		};
		ret.push(self.result_constraint(op_loc, &left_ex.r#type, &right_ex.r#type, result));

		ret
	}

	pub fn result_constraint(&self, op_loc: SourceLoc, left_t: &Type, right_t: &Type, result: &Expr) -> Constraint {
		let result_t = match self {
			Eq => Type::Void,

			Gt | Lt | Gteq | Lteq | Doeq | Noteq | And | Or | Xor => Type::Bool,
			Add | Sub | Mul | Div => left_t.clone(),
			Mod => Type::Int,
			Concat => Type::Str,
			Member => right_t.clone(),

			InfixFn => panic!("no")
		};

		Constraint::Equal(
			(result.r#type.clone(), result.loc),
			(result_t, op_loc),
		)
	}
}

impl UOpID {
	pub fn associations(&self, op_loc: SourceLoc, operand: &Expr, result: &Expr) -> Vec<Constraint> {
		let (operand_t, operand_loc) = (operand.r#type.clone(), operand.loc);
		let mut ret = match self {
			Not =>
				vec![
					Constraint::Equal(
						(operand_t, operand_loc),
						(Type::Bool, op_loc)
					),
				],
			Neg =>
				vec![],
			At =>
				vec![
					Constraint::Equal(
						(operand_t, operand_loc),
						(Type::Pointer(box result.r#type.clone(), Mutability::Unknown(get_type_var_id())), op_loc)
					),
				],
			Ref(_) => vec![],
		};

		ret.push(self.result_constraint(op_loc, operand, result));

		ret
	}

	pub fn result_constraint(&self, op_loc: SourceLoc, operand: &Expr, result: &Expr) -> Constraint {
		let result_t = match self {
			Not => Type::Bool,
			Neg => operand.r#type.clone(),
			At => get_type_var(),
			Ref(true) =>
				Type::Pointer(box operand.r#type.clone(), Mutability::Mutable),
			Ref(false) =>
				Type::Pointer(box operand.r#type.clone(), Mutability::Immutable),
		};

		Constraint::Equal(
			(result_t, op_loc),
			(result.r#type.clone(), result.loc)
		)
	}
}

