use std::{
	iter,
	collections::{
		HashMap,
		HashSet,
		VecDeque,
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
		Parameter,
		GenericType,
		Type,
		Type::*,
		Aggregate,
		AggregateType,
		Literal::*,
		AST,
	},
	lex::tok::{
		TokenLiteral::*,
		OpID,
	},
	parse::get_type_var,
	core::core_vals,
	lex::cradle::SourceLoc,
	error::ErrorMessage,
	types::typeprint::name_of,
};


struct Inference {
	env: Vec<HashMap<String, GenericType>>,
	substitutions: HashMap<u16, Type>,
	errors: Vec<ErrorMessage>,
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
						GenericType {
							uninstantiated: t.clone(),
							generics:
								free_in_type(&HashMap::new(), t.clone()).into_iter().collect(),
						}
					))
					.collect()
			],
			substitutions: HashMap::new(),
			errors: Vec::new(),
		}
	}

	fn env_insert(&mut self, key: String, value: GenericType) {
		self.env.last_mut().unwrap().insert(key, value);
	}

	fn env_find(&self, key: String) -> Option<GenericType> {
		for map in self.env.clone() {
			if map.contains_key(&key) {
				return map.get(&key).cloned();
			}
		}

		None
	}

	fn infer_ast(&mut self, a: AST, constraints: &mut Vec<Constraint>) -> AST {
		match a {
			AST::LetNode(mut l) => {
				let generics =
					match l.def.clone() {
						Some(e) => match e.val {
							ExprVal::LambdaNode(l) => l.generics,
							_ => vec![],
						}
						_ => vec![],
					};
				self.env_insert(l.clone().var.name,
					GenericType {
						uninstantiated: l.var.r#type.clone(),
						generics
					}
				);
				l.def =
					l.def.clone().map(|def| {
						let new_def = self.infer(*def.clone(), constraints);
						constraints.push(mk_eq(&def, &new_def));
						l.var.r#type = new_def.r#type.clone();
						box new_def
					});
				AST::LetNode(l)
			}
			AST::ExprNode(e) =>
				AST::ExprNode(self.infer(e, constraints)),
		}
	}

	fn infer(&mut self, mut expr: Expr, constraints: &mut Vec<Constraint>) -> Expr {
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
				new_lit.r#type = match lit {
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
				self.env.push(HashMap::new());
				let new_block: VecDeque<_> =
					b.iter()
						.map(|line|
							self.infer_ast(line.clone(), constraints)
						)
						.collect();
				self.env.pop();

				if let Some(AST::ExprNode(last_line)) = new_block.back() {
					constraints.push(mk_eq(last_line, &expr));
				}

				ExprVal::BlockNode(new_block)
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
				let generic_t = self.env_find(v.name.clone())
					.unwrap_or_else(||
						panic!("unable to find variable {:#?} in the environment when inferring types, env looks like {:#?}",
							v,
							self.env.to_vec()
						)
					);
				let tmp = generic_t.generics.clone();
				let new_generics = tmp.iter().map(|_| get_type_var()).collect::<Vec<_>>();
				let instantiation =
					generic_t.generics.into_iter()
						.zip(new_generics.clone())
						.collect();

				if !v.generics.is_empty() {
					assert!(v.generics.len() == new_generics.len());
					for (g1, g2) in v.generics.iter().zip(new_generics.iter()) {
						constraints.push(Constraint::Equal(
							(g1.clone(), expr.loc),
							(g2.clone(), expr.loc)
						));
					}
				}

				let new_var = Expr {
					val: ExprVal::VarNode(Variable {
						name: v.name.clone(),
						generics: new_generics.clone(),
					}),
					loc: expr.loc,
					r#type: instantiate(
						&self.substitutions,
						&instantiation,
						generic_t.uninstantiated
					),
				};
				constraints.push(mk_eq(&new_var, &expr));

				ExprVal::VarNode(
					Variable {
						name: v.name,
						generics: new_generics,
					}
				)
			}

			ExprVal::LambdaNode(l) => {
				self.env.push(HashMap::new());
				for arg in l.args.iter() {
					self.env_insert(arg.name.clone(), GenericType::new(arg.r#type.clone()));
				}

				let mut new_body = self.infer(Expr {
					val: l.body.val.clone(),
					loc: l.body.loc,
					r#type: get_type_var(),
				}, constraints);

				let lambda_substitutions = self.solve_constraints(constraints);
				let new_return_t = substitute(&lambda_substitutions, new_body.r#type.clone());

				let new_fn_t = Func(
					l.args.iter()
						.map(|v|
							substitute(&lambda_substitutions, v.r#type.clone())
						)
						.chain(iter::once(new_return_t))
						.collect(),
			 	);
				let new_args = l.args.into_iter()
					.map(|a|
						Parameter {
							name: a.name,
							name_loc: a.name_loc,
							type_loc: a.type_loc,
							r#type: substitute(&lambda_substitutions, a.r#type),
						}
					)
					.collect();

				new_body.annotate_helper(&lambda_substitutions, false);
				let new_lambda = generalize(
					&lambda_substitutions,
					&self.env,
					new_fn_t.clone(),
					Lambda {
						args: new_args,
						generics: vec![],
						captured: l.captured,
						body: box new_body,
					}
				);

				self.env.pop();

				expr.r#type = new_fn_t;

				ExprVal::LambdaNode(new_lambda)
			}

			ExprVal::CallNode(c) => {
				let args_t =
					c.args.iter()
						.map(|_| get_type_var())
						.collect::<Vec<_>>();
				let new_fn_t = Func(
					args_t.clone()
						.into_iter()
						.chain(iter::once(expr.r#type.clone()))
						.collect()
				);

				let new_fn = self.infer(
					Expr {
						val: c.func.val,
						loc: c.func.loc,
						r#type: new_fn_t,
					}, constraints
				);

				let new_args =
					c.args.iter()
						.zip(args_t)
						.map(|(arg, t)| self.infer(
								Expr {
									val: arg.val.clone(),
									loc: arg.loc,
									r#type: t,
								}, constraints
							)
						)
						.collect::<VecDeque<Expr>>();

				ExprVal::CallNode(
					Call {
						func: box new_fn,
						args: new_args,
					}
				)
			}
		};

		Expr {
			val: new_expr_val,
			loc: expr.loc,
			r#type: expr.r#type,
		}
	}

	fn solve_constraints(&mut self, constraints: &[Constraint]) -> HashMap<u16, Type> {
		let mut substitutions = HashMap::new();
		let mut member_map = HashMap::new();
		for constraint in constraints.iter() {
			match constraint {
				Constraint::Equal((t1, l1), (t2, l2)) => {
					unify(&mut substitutions, &mut self.errors, t1.clone(), t2.clone(), (*l1, *l2));
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
			// TODO: member_positions is thrown away, but it should be
			// passed into unify() to track where each member constraint
			// came from. Probably just refactor so that members are a
			// proper trait.
			let locs = members_locations.iter().map(|(_, locs)| *locs).next().unwrap();
			let members = members_locations.into_iter().map(|(types, _)| types).collect();
			unify(&mut substitutions, &mut self.errors, t.clone(), Struct(members), locs);
		}

		substitutions
	}
}

fn mk_eq(e1: &Expr, e2: &Expr) -> Constraint {
	Constraint::Equal(
		(
			e1.r#type.clone(),
			e1.loc
		),
		(
			e2.r#type.clone(),
			e2.loc
		)
	)
}

pub fn instantiate(
	substitutions: &HashMap<u16, Type>,
	instantiation: &HashMap<u16, Type>,
	t: Type
) -> Type {
	match t {
		_ if instantiation.is_empty() =>
			t,
		TypeVar(v) if substitutions.contains_key(&v) =>
			instantiate(substitutions, instantiation, substitutions[&v].clone()),
		TypeVar(v) if instantiation.contains_key(&v) =>
			instantiation[&v].clone(),
		Void | Int | Float | Str | Bool | TypeVar(_) =>
			t,
		Pointer(box r) =>
			Pointer(box instantiate(substitutions, instantiation, r)),
		Func(args_t) =>
			Func(
				args_t.into_iter()
					.map(|t| instantiate(substitutions, instantiation, t))
					.collect()
			),
		Struct(s) =>
			Struct(
				s
					.into_iter()
					.map(|a|
						AggregateType {
							name: a.name,
							r#type: instantiate(substitutions, instantiation, a.r#type),
						}
					)
					.collect()
			),

	}
}

fn substitute(substitutions: &HashMap<u16, Type>, t: Type) -> Type {
	match t {
		Void | Int | Float | Str | Bool => t,
		Pointer(box r) =>
			Pointer(box substitute(substitutions, r)),

		TypeVar(n) if substitutions.contains_key(&n) =>
			substitute(
				substitutions,
				substitutions
					.get(&n)
					.unwrap_or_else(|| panic!("No substitution available for variable {}, substitutions is {:#?}", n, substitutions))
					.clone()
			),

		TypeVar(_) => t,
		Func(args_t) => Func(
			args_t.iter()
				.map(|t1| substitute(substitutions, t1.clone()))
				.collect::<Vec<Type>>(),
		),
		Struct(s) => Struct(
			s.iter().map(|a|
				AggregateType {
					name: a.name.clone(),
					r#type: substitute(substitutions, a.r#type.clone()),
				}
			).collect()
		),
	}
}

// Unifies two types as equal, with origin being the locations that the types came from.
fn unify(
	substitutions: &mut HashMap<u16, Type>,
	errors: &mut Vec<ErrorMessage>,
	t1: Type,
	t2: Type,
	origin: (SourceLoc, SourceLoc)
) {
	// TODO: Remove unecessary up-front clone
	match (t1.clone(), t2.clone()) {
		(Func(args1), Func(args2)) => {
			if args1.len() != args2.len() {
				panic!("functions with unequal lengths are attempting to be unified, {:#?} and {:#?}", t1, t2);
			}
			for (t3, t4) in args1.into_iter().zip(args2.into_iter()) {
				unify(substitutions, errors, t3, t4, origin);
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
				unify(substitutions, errors, a1.r#type, a2.r#type, origin);
			}
		}

		(Pointer(box r1), Pointer(box r2)) =>
			unify(substitutions, errors, r1, r2, origin),

		(TypeVar(i), TypeVar(j)) if i == j => {}
		(TypeVar(i), _) if substitutions.contains_key(&i) => {
			unify(substitutions, errors, substitutions[&i].clone(), t2, origin)
		}
		(_, TypeVar(j)) if substitutions.contains_key(&j) => {
			unify(substitutions, errors, t1, substitutions[&j].clone(), origin)
		}

		(TypeVar(i), _) => {
			assert!(!occurs_in(substitutions, i, t2.clone()));
			substitutions.insert(i, t2);
		}
		(_, TypeVar(j)) => {
			assert!(!occurs_in(substitutions, j, t1.clone()));
			substitutions.insert(j, t1);
		}

		_ if t1 != t2 =>
			errors.push(ErrorMessage {
				msg: format!("the underlined expression is a{} `{}`, but it was expected to be a{} `{}` due to the following line:",
					name_of(&t1),
					t1,
					name_of(&t2),
					t2,
				),
				origin: origin.0
			}),
		_ => {},
	}
}

fn occurs_in(substitutions: &HashMap<u16, Type>, index: u16, t: Type) -> bool {
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
	}
}

fn generalize(
		substitutions: &HashMap<u16, Type>,
		env: &[HashMap<String, GenericType>],
		t: Type,
		l: Lambda
	) -> Lambda {
	let mut outer_env = env.to_vec();
	outer_env.pop();
print!("{:#?} is outer_env", outer_env);
	let free_in_env = free_in_env(substitutions, outer_env);
print!(", {:#?} is substitutions", substitutions);
	let generic_type_vars =
		free_in_type(substitutions, t).into_iter()
			.filter(|free_t| !free_in_env.contains(free_t))
			.collect();
println!(", for fn with args {:#?} env {:#?} generics {:#?}", l.args, free_in_env, generic_type_vars);

	Lambda {
		body: l.body,
		args: l.args,
		captured: l.captured,
		generics: generic_type_vars,
	}
}

pub fn free_in_type(substitutions: &HashMap<u16, Type>, t: Type) -> HashSet<u16> {
	match t {
		Void | Int | Float | Str | Bool =>
			HashSet::new(),
		TypeVar(i) if substitutions.contains_key(&i) =>
			free_in_type(substitutions, substitutions[&i].clone()),
		TypeVar(i) =>
			HashSet::from([i]),
		Pointer(box r) =>
			free_in_type(substitutions, r),
		Func(args_t) =>
			args_t.iter()
				.map(|arg| free_in_type(substitutions, arg.clone()))
				.reduce(|acc, next|
					acc.union(&next).copied().collect()
				)
				.unwrap_or_else(HashSet::new),
		Struct(s) =>
			s.iter()
				.map(|agg| free_in_type(substitutions, agg.r#type.clone()))
				.reduce(|acc, next|
					acc.union(&next).copied().collect()
				)
				.unwrap_or_else(HashSet::new),
	}
}

fn free_in_env(
		substitutions: &HashMap<u16, Type>,
		env: Vec<HashMap<String, GenericType>>
	) -> HashSet<u16> {
	let mut ret = HashSet::new();
	for stack in env {
		for (_, generic_t) in stack {
			ret = ret
				.union(
					&free_in_type(
						substitutions,
						generic_t.uninstantiated
					)
				)
				.copied()
				.collect();
		}
	}

	ret
}

impl Expr {
	pub fn annotate(&mut self) -> Vec<ErrorMessage> {
		let mut inference = Inference::new();
		let mut constraints = Vec::new();
		*self = inference.infer(self.clone(), &mut constraints);
		inference.substitutions = inference.solve_constraints(&constraints);

		self.annotate_helper(&inference.substitutions, false);

		inference.errors
	}

	pub fn annotate_helper(&mut self, substitutions: &HashMap<u16, Type>, annotate_lambdas: bool) {
		self.r#type = substitute(substitutions, self.r#type.clone());
		match self.val {
			ExprVal::LiteralNode(AtomicLiteral(_)) => {},

			ExprVal::LiteralNode(StructLiteral(ref mut s)) => {
				for member in s.iter_mut() {
					member.val.annotate_helper(substitutions, annotate_lambdas);
				}
			}

			ExprVal::VarNode(ref mut v) => {
				for generic in &mut v.generics {
					*generic = substitute(substitutions, generic.clone());
				}
			}

			ExprVal::BlockNode(ref mut b) => {
				for line in b.iter_mut() {
					line.annotate_helper(substitutions, annotate_lambdas);
				}
			}

		// lambdas constrain and substitute themselves in infer()
		// in order to properly infer generics, so we only substitute
		// if we're told to.
			ExprVal::LambdaNode(ref mut lam) => {
				if annotate_lambdas {
					for arg in &mut lam.args {
						arg.r#type = substitute(substitutions, arg.r#type.clone());
					}

					lam.generics = lam.generics.iter().copied().filter(|&t|
						!free_in_type(substitutions, TypeVar(t)).is_empty()
					).collect();
					lam.body.annotate_helper(substitutions, annotate_lambdas);
				}
			}

			ExprVal::IfNode(ref mut i) => {
				i.cond.annotate_helper(substitutions, annotate_lambdas);
				i.then.annotate_helper(substitutions, annotate_lambdas);
				if let Some(ref mut else_branch) = i.r#else {
					else_branch.annotate_helper(substitutions, annotate_lambdas);
				}
			}

			ExprVal::BinaryNode(ref mut b) => {
				b.left.annotate_helper(substitutions, annotate_lambdas);
				b.right.annotate_helper(substitutions, annotate_lambdas);
			}

			ExprVal::CallNode(ref mut c) => {
				c.func.annotate_helper(substitutions, annotate_lambdas);
				for arg in &mut c.args {
					arg.annotate_helper(substitutions, annotate_lambdas);
				}
			}

			ExprVal::UnaryNode(ref mut u) => {
				u.expr.annotate_helper(substitutions, annotate_lambdas);
			}
		}
	}
}

impl AST {
	fn annotate_helper(&mut self, substitutions: &HashMap<u16, Type>, annotate_lambdas: bool) {
		match self {
			AST::ExprNode(e) => e.annotate_helper(substitutions, annotate_lambdas),
			AST::LetNode(Let {
				var: v,
				def: Some(expr)
			}) => {
				expr.annotate_helper(substitutions, annotate_lambdas);
				v.r#type = expr.r#type.clone();
			}
			_ => {}
		}
	}
}
