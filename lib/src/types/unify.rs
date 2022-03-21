use std::{
	iter,
	collections::{
		HashMap,
		HashSet,
	},
	sync::{
		Mutex,
	},
};

use crate::{
	push_err,
	parse::ast::{
		Expr,
		ExprVal,
		Variable,
		Aggregate,
		Literal::*,
	},
	types::{
		Type,
		Type::*,
		TypeVarId,
		AggregateType,
		Mutability,
		generalize,
		substitute,
		instantiate,
		substitute_mutability,
		occurs_in,
		get_type_var,
	},
	lex::tok::{
		LiteralVal,
	},
	core::core_vals,
	lex::cradle::SourceLoc,
	error::ErrorMessage,
	types::typeprint::name_of,
	util::Env,
};


struct Inference {
	env: Vec<HashMap<String, Type>>,
	substitutions: HashMap<TypeVarId, Type>,
	errors: HashSet<ErrorMessage>,
}

#[derive(Clone, Debug)]
pub enum Constraint {
	Equal((Type, SourceLoc), (Type, SourceLoc)),
	HasMember((Type, SourceLoc), (AggregateType, SourceLoc)),
}

impl Inference {
	fn new() -> Self {
		Inference {
			env: vec![
				core_vals.iter()
					.map(|(s,t)| (
						s.clone(),
						generalize(t.clone())
					))
					.collect()
			],
			substitutions: HashMap::new(),
			errors: HashSet::new(),
		}
	}

	fn infer(&mut self, expr: Expr, constraints: &mut Vec<Constraint>) -> Expr {
		let new_expr_val = match expr.val.clone() {
			ExprVal::Literal(StructLiteral(s)) => {
				let inferred_struct =
					s.iter()
						.map(|a|
							Aggregate {
								val: self.infer(a.val.clone(), constraints),
								name: a.name.clone(),
							}
						)
						.collect::<Vec<_>>();
				let new_struct_t =
					Struct(
						inferred_struct.iter()
							.map(|a|
								AggregateType {
									name: a.name.clone(),
									r#type: a.val.r#type.clone(),
								}
							)
							.collect()
					);
				let new_struct = Expr {
					val: ExprVal::Literal(StructLiteral(inferred_struct)),
					loc: expr.loc,
					r#type: new_struct_t,
				};
				constraints.push(mk_eq(&expr, &new_struct));

				new_struct.val
			}

			ExprVal::Literal(AtomicLiteral(lit)) => {
				let mut new_lit = expr.clone();
				new_lit.r#type = match lit.val {
					LiteralVal::Str(_) => Str,
					LiteralVal::Int(_) => Int,
					LiteralVal::Flt(_) => Float,
					LiteralVal::Bool(_) => Bool,
				};
				constraints.push(mk_eq(&new_lit, &expr));

				ExprVal::Literal(AtomicLiteral(lit))
			}

			ExprVal::MemberAccess { left, member, member_loc, dot_loc } => {
				let new_left = self.infer(*left.clone(), constraints);
				constraints.push(mk_eq(&new_left, &left));
				constraints.push(Constraint::HasMember(
					(new_left.r#type.clone(), new_left.loc),
					(member.clone(), dot_loc)
				));
				constraints.push(Constraint::Equal(
					(member.r#type.clone(), member_loc),
					(expr.r#type.clone(), expr.loc)
				));

				ExprVal::MemberAccess { left: box new_left, member, member_loc, dot_loc }
			}

			ExprVal::Unary { op, op_loc, expr: operand } => {
				let mut op_constraints = op.associations(op_loc, &operand, &expr);
				let new_operand = self.infer(*operand.clone(), constraints);
				constraints.push(mk_eq(&new_operand, &operand));
				constraints.append(&mut op_constraints);

				ExprVal::Unary {
					expr: box new_operand,
					op,
					op_loc,
				}
			}

			ExprVal::Binary { left, right, op, op_loc } => {
				let new_left = self.infer(*left.clone(), constraints);
				constraints.push(mk_eq(&new_left, &left));
				let new_right = self.infer(*right.clone(), constraints);
				constraints.push(mk_eq(&new_right, &right));

				let mut op_constraints = op.associations(op_loc, &new_left, &new_right, &expr);
				constraints.append(&mut op_constraints);

				ExprVal::Binary { left: box new_left, right: box new_right, op, op_loc }
			}

			ExprVal::Block(b) => {
				self.env.new_stack();

				let mut new_block = b;
			// We first infer each line, adding constraints to the pile, leaving the
			// generalization and substitution of definitions until after. This properly
			// infers variables that are used in declarations before they are constrained.
				for line in &mut new_block {
					if let ExprVal::Let { declared, def } = &line.val {
						for declared in declared.assignees() {
							self.env.insert_in_env(declared.name.clone(), declared.r#type.clone());
						}
						constraints.push(declared.constrain_as(def.r#type.clone(), def.loc));
					}
					*line = self.infer(line.clone(), constraints);
				}

				self.env.pop();

				if let Some(last_line) = new_block.back() {
					constraints.push(mk_eq(last_line, &expr));
				}

				ExprVal::Block(new_block)
			}

			ExprVal::Let { mut declared, mut def } => {
			// The declared variables need to be placed prematurely in the environment, in case
			// the definition is a recursive function. After inferring the definition, the declared
			// variable is reinserted into the environment, with its inferred type.
				for declared in declared.assignees() {
					self.env.insert_in_env(declared.name.clone(), declared.r#type.clone());
				}
				def = {
					let mut new_def = self.infer(*def, constraints);
					let local_substitutions = solve_constraints(constraints, &mut self.errors);
					new_def.r#type = generalize(
						substitute(&local_substitutions, &new_def.r#type)
					);

					// Reinsert the declared variables, with their new generalized types.
					declared.infer_as(
						new_def.r#type.clone(),
						new_def.loc,
						&mut self.errors
					);
					for declared in declared.assignees() {
						self.env.insert_in_env(declared.name.clone(), declared.r#type.clone());
					}

					box new_def
				};

				let new_expr = Expr { val: ExprVal::Let { declared, def }, r#type: Void, loc: expr.loc };
				constraints.push(mk_eq(
					&expr,
					&new_expr
				));
				new_expr.val
			}

			ExprVal::If { cond, then, r#else } => {
				let new_then = self.infer(*then.clone(), constraints);
				let new_cond = self.infer(*cond.clone(), constraints);
				let new_else = r#else.map(|box else_expr| {
					let tmp = self.infer(else_expr, constraints);
					constraints.push(mk_eq(&new_then, &tmp));
					box tmp
				});
				if new_else.is_none() {
					constraints.push(
						Constraint::Equal(
							(new_then.r#type.clone(), new_then.loc),
							(Void, SourceLoc::new(expr.loc.start, new_cond.loc.start.index))
						)
					)
				}
				constraints.push(mk_eq(&new_then, &then));
				constraints.push(mk_eq(&new_then, &expr));
				constraints.push(mk_eq(&new_cond, &cond));
				constraints.push(
					Constraint::Equal(
						(new_cond.r#type.clone(), new_cond.loc),
						(Bool, SourceLoc::new(expr.loc.start, new_cond.loc.start.index))
					)
				);

				ExprVal::If {
					then: box new_then,
					cond: box new_cond,
					r#else: new_else,
				}
			}

			ExprVal::Var(v) => {
				let var_t = self.env.find(&v.name).cloned()
					.unwrap_or_else(||
						panic!("unable to find variable {:#?} in the environment when inferring types, env looks like {:#?}",
							v,
							self.env.to_vec()
						)
					);
				let (r#type, generics) = instantiate(&var_t, &self.substitutions);
				let val = ExprVal::Var(Variable {
					name: v.name,
					declaration_loc: v.declaration_loc,
					generics,
				});
				let loc = expr.loc;
				let new_expr = Expr { val, loc, r#type };

				constraints.push(mk_eq(
					&Expr {
						val: expr.val,
						loc: v.declaration_loc,
						r#type: expr.r#type.clone(),
					},
					&new_expr
				));

				new_expr.val
			}

			ExprVal::Lambda { args, body, captured }  => {
				self.env.new_stack();
				for arg in args.iter() {
					self.env.insert_in_env(arg.name.clone(), arg.r#type.clone());
				}

				let new_body = self.infer(Expr {
					val: body.val.clone(),
					loc: body.loc,
					r#type: get_type_var(),
				}, constraints);

				let new_fn_t = Func(
					args.iter()
						.map(|arg| arg.r#type.clone())
						.chain(iter::once(new_body.r#type.clone()))
						.collect(),
			 	);

				let new_lambda = Expr {
					val: ExprVal::Lambda { body: box new_body, args, captured },
					r#type: new_fn_t,
					loc: expr.loc,
				};

				self.env.pop();

				constraints.push(mk_eq(
					&expr,
					&new_lambda
				));

				new_lambda.val
			}

			ExprVal::Call { args, func } => {
				let args_t = args.clone().into_iter().map(|arg| arg.r#type);
				let fn_t = Func(
					args_t.clone().chain(iter::once(expr.r#type.clone())).collect()
				);
				let func = box self.infer(
					Expr {
						val: func.val.clone(),
						loc: func.loc,
						r#type: fn_t,
					},
					constraints
				);
				let args = args.into_iter()
					.zip(args_t)
					.map(|(arg, t)| self.infer(
						Expr {
							val: arg.val,
							loc: arg.loc,
							r#type: t,
						},
						constraints
					))
					.collect();

				ExprVal::Call { func, args }
			}
		};

		Expr {
			val: new_expr_val,
			loc: expr.loc,
			r#type: expr.r#type,
		}
	}
}

pub fn solve_constraints(constraints: &[Constraint], errors: &mut HashSet<ErrorMessage>) -> HashMap<TypeVarId, Type> {
	let mut substitutions = HashMap::new();
	let mut mutability_substitutions = HashMap::new();
	let mut member_map = HashMap::new();
	for constraint in constraints.iter() {
		match constraint {
			Constraint::Equal((t1, l1), (t2, l2)) => {
				unify(&mut substitutions, &mut mutability_substitutions, errors, t1, t2, (*l1, *l2));
			}

			Constraint::HasMember((t, l1), (m, l2)) => {
				member_map.entry(t)
					.or_insert_with(Vec::new)
					.push((m.clone(), (*l1, *l2)));
			}
		}
	}

	for (t, members_locations) in member_map.into_iter() {
		// TODO: this is where row polymorphism will be added
		// TODO: members_positions is thrown away, but it should be
		// passed into unify() to track where each member constraint
		// came from. Probably just refactor so that members are a
		// proper trait.
		let locs = members_locations.iter().map(|(_, locs)| *locs).next().unwrap();
		let members = members_locations.into_iter().map(|(types, _)| types).collect();
		unify(&mut substitutions, &mut mutability_substitutions, errors, t, &Struct(members), locs);
	}

	for t in substitutions.values_mut() {
		*t = substitute_mutability(&mutability_substitutions, t);
	}
	substitutions
}

fn mk_eq(e1: &Expr, e2: &Expr) -> Constraint {
	Constraint::Equal(
		(e1.r#type.clone(), e1.loc),
		(e2.r#type.clone(), e2.loc)
	)
}

// Unifies two types as equal, with origins being the locations that the types came from.
fn unify(
	substitutions: &mut HashMap<TypeVarId, Type>,
	mutability_substitutions: &mut HashMap<TypeVarId, Mutability>,
	errors: &mut HashSet<ErrorMessage>,
	t1: &Type,
	t2: &Type,
	origins: (SourceLoc, SourceLoc)
) {
	use self::Mutability::*;

	macro_rules! recurse {
		($t1:expr, $t2:expr, $orig:expr) => {
			unify(substitutions, mutability_substitutions, errors, $t1, $t2, $orig)
		}
	}

	match (&t1, &t2) {
		(Func(args1), Func(args2)) if args1.len() != args2.len() => {
			push_err!(
				errors,
				vec![ origins.0, origins.1 ],
				"functions with inequal lengths are attempting to be unified, {} and {}, due to the following lines",
				substitute(substitutions, t1),
				substitute(substitutions, t2),
			);
		}

		(Func(args1), Func(args2)) => {
			for (t3, t4) in args1.iter().zip(args2.iter()) {
				recurse!(t3, t4, origins);
			}
		}

		(Struct(s1), Struct(s2)) => {
			assert_eq!(s1.len(), s2.len());

			let (mut s1, mut s2) = (s1.clone(), s2.clone());
			s1.sort();
			s2.sort();
			for (a1, a2) in s1.iter().zip(s2.iter()) {
				assert_eq!(a1.name, a2.name);
				recurse!(&a1.r#type, &a2.r#type, origins);
			}
		}

		(Pointer(box r1, Unknown(m1)), Pointer(box r2, Unknown(m2))) if m1 == m2 => {
			recurse!(r1, r2, origins);
		}
		(Pointer(box r1, Unknown(u)), Pointer(box r2, m2)) |
		 (Pointer(box r2, m2), Pointer(box r1, Unknown(u)))
		 if mutability_substitutions.contains_key(u) => {
			recurse!(&Pointer(box r1.clone(), mutability_substitutions[u]), &Pointer(box r2.clone(), *m2), origins);
		}
		(Pointer(box r1, Unknown(u)), Pointer(box r2, m)) |
		 (Pointer(box r2, m), Pointer(box r1, Unknown(u))) => {
			mutability_substitutions.insert(*u, *m);
			recurse!(r1, r2, origins);
		}
		(Pointer(box r1, m1), Pointer(box r2, m2)) => {
			if m1 != m2 {
				push_err!(
					errors,
					vec![ origins.0, origins.1 ],
					"I expected the mutability of this {} and this {} to be the same, due to the following lines, but they were not ({} vs {})",
					substitute(substitutions, &Pointer(box r1.clone(), *m1)),
					substitute(substitutions, &Pointer(box r2.clone(), *m2)),
					m1, m2
				);
			} else {
				recurse!(r1, r2, origins);
			}
		}

		(Forall(args1, generic_t1), Forall(args2, generic_t2)) => {
			if args1.len() != args2.len() {
				push_err!(
					errors,
					vec![ origins.0, origins.1 ],
					"there was a type mismatch between a `{}` and a `{}`, expected to be the same due to the following lines:",
					Forall(args1.to_vec(), box substitute(substitutions, generic_t1)),
					Forall(args2.to_vec(), box substitute(substitutions, generic_t2)),
				);
				return;
			}
			recurse!(generic_t1, generic_t2, origins);
		}

		(Free(i), Free(j)) if i == j => {}
		(Free(i), t) | (t, Free(i)) if substitutions.contains_key(i) => {
			let substituted = substitutions[i].clone();
			recurse!(t, &substituted, origins);
		}

		(Free(i), t) | (t, Free(i)) => {
			if occurs_in(substitutions, *i, t) {
				push_err!(
					errors,
					vec![ origins.0, origins.1 ],
					"types `{}` and `{}`, expected to be the same due to the following lines, are recursive.",
					substitute(substitutions, t),
					Free(*i)
				);
			} else {
				substitutions.insert(*i, (*t).clone());
			}
		}

		_ if t1 != t2 => {
			push_err!(
				errors,
				vec![ origins.0, origins.1 ],
				"there was a type mismatch between a{} `{}`, and a{} `{}`, expected to be of the same type due to the following lines:",
				name_of(t1), substitute(substitutions, t1), name_of(t2), substitute(substitutions, t2)
			);
		}
		_ => {},
	}
}

impl Expr {
	pub fn annotate(&mut self) -> HashSet<ErrorMessage> {
		let mut inference = Inference::new();
		let mut constraints = Vec::new();
		*self = inference.infer(self.clone(), &mut constraints);
		let substitutions = solve_constraints(&constraints, &mut inference.errors);

	// Annotate expressions
		let typevar_env = Mutex::new(vec![ Vec::new() ]);
		let error_messages = Mutex::new(inference.errors);
		let trans_expr = annotate_helper(
			&substitutions,
			Some(&typevar_env),
			Some(&error_messages)
		);
		let post_trans = |e: &mut Expr| {
			if let ExprVal::Block(_) = e.val {
				typevar_env.lock().unwrap().pop();
			}
		};
		self.transform(trans_expr, post_trans);

		error_messages.into_inner().unwrap()
	}
}

// Typevar_env keeps track of the free variables in the environment if supplied, and pushes
// to generics_errors when it finds unconstrained typevars outside of a generic function.
pub fn annotate_helper<'a>(
	substitutions: &'a HashMap<TypeVarId, Type>,
	typevar_env: Option<&'a Mutex<Vec<Vec<TypeVarId>>>>,
	generics_errors: Option<&'a Mutex<HashSet<ErrorMessage>>>
) -> impl 'a + Copy + Fn(&mut Expr) -> bool {
	move |e: &mut Expr| {
		e.r#type = substitute(substitutions, &e.r#type);
		if let (Forall(generics, _), Some(errors), Some(env))
			= (&e.r#type, generics_errors.as_ref(), typevar_env.as_ref())
		{
			let env = env.lock().unwrap();
			let errors = &mut errors.lock().unwrap();
			for g in generics {
				if !env.iter().any(|stack| stack.iter().any(|t| t == g)) {
					errors.insert(ErrorMessage {
						msg: format!("the generic type {} was underconstrained, consider using a type annotation", e.r#type),
						origins: vec![ e.loc ],
					});
				}
			}
		}
		match &mut e.val {
			ExprVal::Let { declared, def } => {
				if let Forall(generics, _) = &def.r#type {
					if let Some(env) = &typevar_env {
						let mut env = env.lock().unwrap();
						for generic in generics {
							env.last_mut().unwrap().push(*generic);
						}
					}
				}

				declared.infer_as(
					generalize(substitute(
						substitutions,
						&def.r#type
					)),
					def.loc,
					&mut HashSet::new()
				);
			}

			ExprVal::Block(_) => {
				if let Some(env) = &typevar_env {
					let mut env = env.lock().unwrap();
					env.push(Vec::new());
				}
			}

			ExprVal::Var(v) => {
				for generic_t in v.generics.values_mut() {
					*generic_t = substitute(substitutions, generic_t);
				}
			}

			ExprVal::Lambda { captured, args, .. }  => {
				*captured = captured.drain()
					.map(|mut c| {
						c.r#type = generalize(substitute(substitutions, &c.r#type));
						c
					})
					.collect();
				for arg in args.iter_mut() {
					arg.r#type = substitute(substitutions, &arg.r#type);
				}
			}

			_ => {}
		}

		true
	}
}