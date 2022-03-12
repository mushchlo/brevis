use std::{
	iter::{
		self,
		FromIterator,
	},
	collections::{
		HashMap,
		HashSet,
	},
	sync::{
		Mutex,
	},
};

use crate::{
	parse::ast::{
		Expr,
		ExprVal,
		Let,
		Lambda,
		IfElse,
		Call,
		Binary,
		Unary,
		Variable,
		Aggregate,
		Literal::*,
	},
	types::{
		Type,
		Type::*,
		TypeVarId,
		AggregateType,
	},
	lex::tok::{
		LiteralVal::*,
		OpID,
	},
	parse::get_type_var,
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
			ExprVal::LiteralNode(StructLiteral(s)) => {
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
					val: ExprVal::LiteralNode(StructLiteral(inferred_struct)),
					loc: expr.loc,
					r#type: new_struct_t,
				};
				constraints.push(mk_eq(&expr, &new_struct));

				new_struct.val
			}

			ExprVal::LiteralNode(AtomicLiteral(lit)) => {
				let mut new_lit = expr.clone();
				new_lit.r#type = match lit.val {
					StrLit(_) => Str,
					IntLit(_) => Int,
					FltLit(_) => Float,
					BoolLit(_) => Bool,
				};
				constraints.push(mk_eq(&new_lit, &expr));

				ExprVal::LiteralNode(AtomicLiteral(lit))
			}

			ExprVal::UnaryNode(u) => {
				let mut op_constraints = u.associations(&expr);
				let new_operand = self.infer(*u.expr.clone(), constraints);
				constraints.push(mk_eq(&new_operand, &u.expr));
				constraints.append(&mut op_constraints);

				ExprVal::UnaryNode(
					Unary {
						expr: box new_operand,
						op: u.op,
						op_loc: u.op_loc,
					}
				)
			}

			ExprVal::BinaryNode(b) if b.op == OpID::Member => {
				let new_left = self.infer(*b.left.clone(), constraints);

				constraints.push(mk_eq(&new_left, &b.left));

				let mut op_constraints = b.associations(&expr);
				constraints.append(&mut op_constraints);

				ExprVal::BinaryNode(
					Binary {
						op: b.op,
						op_loc: b.op_loc,
						left: box new_left,
						right: b.right,
					}
				)
			}

			ExprVal::BinaryNode(b) => {
				let (new_left, new_right) = (
					self.infer(*b.left.clone(), constraints),
					self.infer(*b.right.clone(), constraints)
				);

				constraints.push(mk_eq(&new_left, &b.left));
				constraints.push(mk_eq(&new_right, &b.right));

				let mut op_constraints = b.associations(&expr);
				constraints.append(&mut op_constraints);

				ExprVal::BinaryNode(
					Binary {
						op: b.op,
						op_loc: b.op_loc,
						left: box new_left,
						right: box new_right,
					}
				)
			}

			ExprVal::BlockNode(b) => {
				self.env.new_stack();

				let mut new_block = b;
			// We first infer each line, adding constraints to the pile, leaving the
			// generalization and substitution of definitions until after. This properly
			// infers variables that are used in declarations before they are constrained.
				for line in &mut new_block {
					if let ExprVal::LetNode(l) = &line.val {
						for declared in l.declared.assignees() {
							self.env.insert_in_env(declared.name.clone(), declared.r#type.clone());
						}
						constraints.push(l.declared.constrain_as(l.def.r#type.clone(), l.def.loc));
					}
					*line = self.infer(line.clone(), constraints);
				}

				self.env.pop();

				if let Some(last_line) = new_block.back() {
					constraints.push(mk_eq(last_line, &expr));
				}

				ExprVal::BlockNode(new_block)
			}

			ExprVal::LetNode(mut l) => {
			// The declared variables need to be placed prematurely in the environment, in case
			// the definition is a recursive function. After inferring the definition, the declared
			// variable is reinserted into the environment, with its inferred type.
				for declared in l.declared.assignees() {
					self.env.insert_in_env(declared.name.clone(), declared.r#type.clone());
				}
				l.def = {
					let mut new_def = self.infer(*l.def, constraints);
					let local_substitutions = solve_constraints(constraints, &mut self.errors);
					new_def.r#type = generalize(
						substitute(&local_substitutions, &new_def.r#type)
					);

					// Reinsert the declared variables, with their new generalized types.
					l.declared.infer_as(
						new_def.r#type.clone(),
						new_def.loc,
						&mut self.errors
					);
					for declared in l.declared.assignees() {
						self.env.insert_in_env(declared.name.clone(), declared.r#type.clone());
					}

					box new_def
				};
				constraints.push(mk_eq(
					&expr,
					&Expr {
						val: ExprVal::LetNode(l.clone()),
						r#type: Void,
						loc: expr.loc,
					}
				));
				ExprVal::LetNode(l)
			}

			ExprVal::IfNode(i) => {
				let new_then = self.infer(*i.then.clone(), constraints);
				let new_cond = self.infer(*i.cond.clone(), constraints);
				let new_else = i.r#else.map(|box else_expr| {
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
				constraints.push(mk_eq(&new_then, &i.then));
				constraints.push(mk_eq(&new_then, &expr));
				constraints.push(mk_eq(&new_cond, &i.cond));
				constraints.push(
					Constraint::Equal(
						(new_cond.r#type.clone(), new_cond.loc),
						(Bool, SourceLoc::new(expr.loc.start, new_cond.loc.start.index))
					)
				);

				ExprVal::IfNode(
					IfElse {
						then: box new_then,
						cond: box new_cond,
						r#else: new_else,
					}
				)
			}

			ExprVal::VarNode(v) => {
				let var_t = self.env.find(&v.name).cloned()
					.unwrap_or_else(||
						panic!("unable to find variable {:#?} in the environment when inferring types, env looks like {:#?}",
							v,
							self.env.to_vec()
						)
					);
				let (r#type, generics) = instantiate(&var_t, &self.substitutions);
				let val = ExprVal::VarNode(Variable {
					name: v.name,
					generics,
				});
				let loc = expr.loc;
				let new_expr = Expr { val, loc, r#type };

				constraints.push(mk_eq(&expr, &new_expr));

				new_expr.val
			}

			ExprVal::LambdaNode(l) => {
				self.env.new_stack();
				for arg in l.args.iter() {
					self.env.insert_in_env(arg.name.clone(), arg.r#type.clone());
				}

				let new_body = self.infer(Expr {
					val: l.body.val.clone(),
					loc: l.body.loc,
					r#type: get_type_var(),
				}, constraints);

				let new_fn_t = Func(
					l.args.iter()
						.map(|arg| arg.r#type.clone())
						.chain(iter::once(new_body.r#type.clone()))
						.collect(),
			 	);

				let new_lambda = Lambda {
					args: l.args,
					body: box new_body,
					captured: l.captured,
				};

				self.env.pop();

				constraints.push(mk_eq(
					&expr,
					&Expr {
						val: ExprVal::LambdaNode(new_lambda.clone()),
						r#type: new_fn_t,
						loc: expr.loc,
					}
				));

				ExprVal::LambdaNode(new_lambda)
			}

			ExprVal::CallNode(c) => {
				let args_t = c.args.clone().into_iter().map(|arg| arg.r#type);
				let fn_t = Func(
					args_t.clone().chain(iter::once(expr.r#type.clone())).collect()
				);
				let func = box self.infer(
					Expr {
						val: c.func.val.clone(),
						loc: c.func.loc,
						r#type: fn_t,
					},
					constraints
				);
				let args = c.args.into_iter()
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

				ExprVal::CallNode(Call { func, args })
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
	let mut member_map = HashMap::new();
	for constraint in constraints.iter() {
		match constraint {
			Constraint::Equal((t1, l1), (t2, l2)) => {
				unify(&mut substitutions, errors, t1.clone(), t2.clone(), (*l1, *l2));
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
		unify(&mut substitutions, errors, t.clone(), Struct(members), locs);
	}

	substitutions
}

fn mk_eq(e1: &Expr, e2: &Expr) -> Constraint {
	Constraint::Equal(
		(e1.r#type.clone(), e1.loc),
		(e2.r#type.clone(), e2.loc)
	)
}

// Flattens a Forall-quantified type, returning a type with free
// typevars, and a hashmap of generic types to typevars.
pub fn instantiate(t: &Type, substitutions: &HashMap<TypeVarId, Type>) -> (Type, HashMap<TypeVarId, Type>) {/*
	match t {
		TypeVar(v) if substitutions.contains(v) => substitutions[v],
		Forall(type_vars, generic_type) => {
			let map = type_vars.iter().map(|tv| (tv, get_type_var())).collect();

*/
	let instantiation = generics_in_type(substitutions, t).into_iter()
		.zip(iter::repeat_with(get_type_var))
		.collect();
	(substitute(&instantiation, t), instantiation)
}

pub fn substitute(substitutions: &HashMap<TypeVarId, Type>, t: &Type) -> Type {
	match t {
		Void => Void,
		Int => Int,
		Float => Float,
		Str => Str,
		Bool => Bool,
		Pointer(box r) =>
			Pointer(box substitute(substitutions, r)),

		TypeVar(v) if substitutions.contains_key(v) =>
			substitute(
				substitutions,
				&substitutions
					.get(v)
					.unwrap_or_else(|| unreachable!())
			),

		TypeVar(v) => TypeVar(*v),
		Func(args_t) => Func(
			args_t.iter()
				.map(|t1| substitute(substitutions, t1))
				.collect::<Vec<Type>>(),
		),
		Struct(s) => Struct(
			s.iter().map(|a|
				AggregateType {
					name: a.name.clone(),
					r#type: substitute(substitutions, &a.r#type),
				}
			).collect()
		),
		Forall(type_vars, box generic_t) => {
			let new_generic_t = substitute(substitutions, generic_t);
			let free = free_in_type(substitutions, &new_generic_t);
			let new_type_vars = type_vars.iter().copied().filter(|tv| free.contains(tv)).collect::<Vec<_>>();

			if new_type_vars.len() > 0 {
				Forall(new_type_vars, box new_generic_t)
			} else {
				new_generic_t
			}
		}
	}
}

// Unifies two types as equal, with origin being the locations that the types came from.
fn unify(
	substitutions: &mut HashMap<TypeVarId, Type>,
	errors: &mut HashSet<ErrorMessage>,
	t1: Type,
	t2: Type,
	origins: (SourceLoc, SourceLoc)
) {
	// TODO: Remove unecessary up-front clone
	match (t1.clone(), t2.clone()) {
		(Func(args1), Func(args2)) if args1.len() != args2.len() => {
			errors.insert(ErrorMessage {
				msg: format!("functions with unequal lengths are attempting to be unified, {} and {}, due to the following lines", t1, t2),
				origins: vec![ origins.0, origins.1 ],
			});
		}

		(Func(args1), Func(args2)) => {
			for (t3, t4) in args1.into_iter().zip(args2.into_iter()) {
				unify(substitutions, errors, t3, t4, origins);
			}
		}

		(Struct(mut s1), Struct(mut s2)) => {
			if s1.len() != s2.len() {
				panic!("no");
			}

			s1.sort();
			s2.sort();
			for (a1, a2) in s1.into_iter().zip(s2.into_iter()) {
				if a1.name != a2.name {
					panic!("bad");
				}
				unify(substitutions, errors, a1.r#type, a2.r#type, origins);
			}
		}

		(Pointer(box r1), Pointer(box r2)) =>
			unify(substitutions, errors, r1, r2, origins),

		(Forall(args1, box generic_t1), Forall(args2, box generic_t2)) => {
			if args1.len() == args2.len() {
				errors.insert(ErrorMessage {
					msg: format!("there was a type mismatch between a `{}` and a `{}`, expected to be the same due to the following lines:",
						Forall(args1, box generic_t1),
						Forall(args2, box generic_t2),
					),
					origins: vec![ origins.0, origins.1 ],
				});
				return;
			}
			unify(substitutions, errors, generic_t1, generic_t2, origins);
		}

		(TypeVar(i), TypeVar(j)) if i == j => {}
		(TypeVar(i), t) | (t, TypeVar(i)) if substitutions.contains_key(&i) => {
			unify(substitutions, errors, t, substitutions[&i].clone(), origins)
		}

		(TypeVar(i), t) | (t, TypeVar(i)) => {
			if occurs_in(substitutions, i, t.clone()) {
				errors.insert(ErrorMessage {
					msg: format!("types `{}` and `{}`, expected to be the same due to the following lines, are recursive.",
						substitute(substitutions, &t),
						TypeVar(i)
					),
					origins: vec![ origins.0, origins.1 ],
				});
				return;
			}
			substitutions.insert(i, t);
		}

		_ if t1 != t2 => {
			errors.insert(ErrorMessage {
				msg: format!("there was a type mismatch between a{} `{}`, and a{} `{}`, expected to be of the same type due to the following lines:",
					name_of(&t1),
					substitute(substitutions, &t1),
					name_of(&t2),
					substitute(substitutions, &t2),
				),
				origins: vec![ origins.0, origins.1 ],
			});
		}
		_ => {},
	}
}

fn occurs_in(substitutions: &HashMap<TypeVarId, Type>, index: TypeVarId, t: Type) -> bool {
	match t {
		Void | Int | Float | Str | Bool => false,

		Pointer(box p) =>
			occurs_in(substitutions, index, p),

		TypeVar(i) if substitutions.contains_key(&i) => {
			occurs_in(substitutions, index, substitutions[&i].clone())
		}

		TypeVar(i) => i == index,

		Struct(s) => s.into_iter().any(|a| occurs_in(substitutions, index, a.r#type)),

		Func(args_t) => {
			args_t.into_iter()
				.any(|t1| occurs_in(substitutions, index, t1))
		}

		Forall(_, box generic_t) =>
			occurs_in(substitutions, index, generic_t),
	}
}

pub fn generalize(t: Type) -> Type {
	let free_vars = free_in_type(&HashMap::new(), &t);
	if !free_vars.is_empty() {
		Forall(free_vars.into_iter().collect(), box t)
	} else {
		t
	}
}

pub fn free_in_type(substitutions: &HashMap<TypeVarId, Type>, t: &Type) -> HashSet<TypeVarId> {
	match t {
		Void | Int | Float | Str | Bool =>
			HashSet::new(),
		TypeVar(i) if substitutions.contains_key(i) =>
			free_in_type(substitutions, &substitutions[i]),
		TypeVar(i) =>
			HashSet::from([*i]),
		Pointer(box r) =>
			free_in_type(substitutions, r),
		Func(args_t) =>
			args_t.iter()
				.map(|arg| free_in_type(substitutions, arg))
				.reduce(|mut acc, next| {
					acc.extend(next.into_iter());
					acc
				})
				.unwrap_or_else(HashSet::new),
		Struct(s) =>
			s.iter()
				.map(|agg| free_in_type(substitutions, &agg.r#type))
				.reduce(|mut acc, next| {
					acc.extend(next.into_iter());
					acc
				})
				.unwrap_or_else(HashSet::new),
		Forall(_, box generic_t) =>
			free_in_type(substitutions, generic_t),
	}
}

pub fn generics_in_type(substitutions: &HashMap<TypeVarId, Type>, t: &Type) -> HashSet<TypeVarId> {
	match t {
		Void | Int | Float | Str | Bool =>
			HashSet::new(),
		TypeVar(i) if substitutions.contains_key(i) =>
			generics_in_type(substitutions, &substitutions[i]),
		TypeVar(_) =>
			HashSet::new(),
		Pointer(box r) =>
			generics_in_type(substitutions, r),
		Func(args_t) =>
			args_t.iter()
				.map(|arg| generics_in_type(substitutions, arg))
				.reduce(|mut acc, next| {
					acc.extend(next.into_iter());
					acc
				})
				.unwrap_or_else(HashSet::new),
		Struct(s) =>
			s.iter()
				.map(|agg| generics_in_type(substitutions, &agg.r#type))
				.reduce(|mut acc, next| {
					acc.extend(next.into_iter());
					acc
				})
				.unwrap_or_else(HashSet::new),
		Forall(type_vars, box generic_t) => {
			let mut ret = HashSet::from_iter(type_vars.iter().cloned());
			ret.extend(generics_in_type(substitutions, generic_t).into_iter());
			ret
		}
	}
}

impl Expr {
	pub fn annotate(&mut self) -> HashSet<ErrorMessage> {
		let mut inference = Inference::new();
		let mut constraints = Vec::new();
		*self = inference.infer(self.clone(), &mut constraints);
		inference.substitutions = solve_constraints(&constraints, &mut inference.errors);

	// Annotate expressions
		let typevar_env = Mutex::new(vec![ Vec::new() ]);
		let error_messages = Mutex::new(inference.errors);
		let trans_expr = annotate_helper(
			&mut inference.substitutions,
			Some(&typevar_env),
			Some(&error_messages)
		);
		let post_trans = |e: &mut Expr| {
			if let ExprVal::BlockNode(_) = e.val {
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
	substitutions: &'a mut HashMap<TypeVarId, Type>,
	typevar_env: Option<&'a Mutex<Vec<Vec<TypeVarId>>>>,
	generics_errors: Option<&'a Mutex<HashSet<ErrorMessage>>>
) -> impl 'a + Fn(&mut Expr) -> bool {
	move |e| {
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
			ExprVal::LetNode(Let {
				ref mut declared,
				def: ref mut expr
			}) => {
				if let Forall(generics, _) = &expr.r#type {
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
						&expr.r#type
					)),
					expr.loc,
					&mut HashSet::new()
				);
			}

			ExprVal::BlockNode(_) => {
				if let Some(env) = &typevar_env {
					let mut env = env.lock().unwrap();
					env.push(Vec::new());
				}
			}

			ExprVal::VarNode(ref mut v) => {
				for (_, generic_t) in &mut v.generics {
					*generic_t = substitute(substitutions, generic_t);
				}
			}

			ExprVal::LambdaNode(ref mut lam) => {
				lam.captured = lam.captured.drain().map(|mut captured| {
					captured.r#type = generalize(substitute(substitutions, &captured.r#type));
					captured
				}).collect();
				for arg in &mut lam.args {
					arg.r#type = substitute(substitutions, &arg.r#type);
				}
			}

			_ => {}
		}

		true
	}
}