use std::{
	iter,
	collections::{
		HashMap,
		HashSet,
		VecDeque,
	},
};

use crate::{
	ast::{
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
		TConstructor,
		GenericType,
		Type,
		Type::*,
		Aggregate,
		AggregateType,
		Literal::*,
		AST,
	},
	tok::{
		TokenLiteral::*,
		OpID,
	},
	parse::get_type_var,
	core::core_vals,
};

struct Inference {
	env: Vec<HashMap<String, GenericType>>,
	substitutions: HashMap<u16, Type>
}

#[derive(Clone, Debug)]
pub enum Constraint {
	Equal(Type, Type),
	HasMember(Type, AggregateType),
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
			substitutions: HashMap::new()
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
				l.def =
					l.def.clone().map(|def| {
						let new_def = self.infer(*def.clone(), constraints);
						constraints.push(Constraint::Equal(def.r#type.clone(), new_def.r#type.clone()));
						l.var.r#type = new_def.r#type.clone();
						box new_def
					});
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
				AST::LetNode(l)
			}
			AST::ExprNode(e) =>
				AST::ExprNode(self.infer(e, constraints)),
		}
	}

	fn infer(&mut self, mut expr: Expr, constraints: &mut Vec<Constraint>) -> Expr {
		let new_expr_val = match expr.val.clone() {
			ExprVal::LiteralNode(StructLiteral(s)) => {
				let new_struct =
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
						new_struct.iter()
							.map(|a|
								AggregateType {
									name: a.name.clone(),
									r#type: a.val.r#type.clone(),
								}
							)
							.collect()
					);
				constraints.push(Constraint::Equal(expr.r#type.clone(), new_struct_t));

				ExprVal::LiteralNode(StructLiteral(new_struct))
			}

			ExprVal::LiteralNode(AtomicLiteral(lit)) => {
				let lit_t = match lit {
					StrLit(_) => Str,
					IntLit(_) => Int,
					FltLit(_) => Float,
					BoolLit(_) => Bool,
				};
				constraints.push(Constraint::Equal(lit_t, expr.r#type.clone()));

				ExprVal::LiteralNode(AtomicLiteral(lit))
			}

			ExprVal::UnaryNode(u) => {
				let mut op_constraints = u.associations(expr.r#type.clone());
				let new_expr = self.infer(*u.expr.clone(), constraints);
				constraints.push(Constraint::Equal(new_expr.r#type.clone(), u.expr.r#type));
				constraints.append(&mut op_constraints);

				let result_t = u.op.result(new_expr.r#type.clone());
				constraints.push(Constraint::Equal(result_t, expr.r#type.clone()));

				ExprVal::UnaryNode(
					Unary {
						expr: box new_expr,
						op: u.op,
					}
				)
			}

			ExprVal::BinaryNode(b) if b.op == OpID::Member => {
				let new_left = self.infer(*b.left.clone(), constraints);

				constraints.push(Constraint::Equal(new_left.r#type.clone(), b.left.r#type.clone()));

				let mut op_constraints = b.associations();
				constraints.append(&mut op_constraints);
				let result_t = b.op.result(new_left.r#type.clone(), b.right.r#type.clone());
				constraints.push(Constraint::Equal(result_t, expr.r#type.clone()));

				if let Struct(s) = new_left.r#type.clone() {
					if let Some(i) = s.iter().position(|a| matches!(b.right.val.clone(), ExprVal::VarNode(s) if s.name == a.name)) {
						constraints.push(Constraint::Equal(b.right.r#type.clone(), s[i].r#type.clone()));
					}
				}

				ExprVal::BinaryNode(
					Binary {
						op: b.op,
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

				constraints.push(Constraint::Equal(new_left.r#type.clone(), b.left.r#type.clone()));
				constraints.push(Constraint::Equal(new_right.r#type.clone(), b.right.r#type.clone()));

				let mut op_constraints = b.associations();
				constraints.append(&mut op_constraints);

				let result_t = b.op.result(new_left.r#type.clone(), new_right.r#type.clone());
				constraints.push(Constraint::Equal(result_t, expr.r#type.clone()));

				ExprVal::BinaryNode(
					Binary {
						op: b.op,
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

				let block_val_t = match new_block.back() {
					Some(AST::ExprNode(e)) => e.r#type.clone(),
					_ => Void
				};
				self.env.pop();
				constraints.push(Constraint::Equal(block_val_t, expr.r#type.clone()));

				ExprVal::BlockNode(new_block)
			}

			ExprVal::IfNode(i) => {
				let new_then = self.infer(*i.then.clone(), constraints);
				let new_cond = self.infer(*i.cond.clone(), constraints);
				let new_else = i.r#else.map(|box else_expr| {
					let tmp = self.infer(else_expr, constraints);
					constraints.push(Constraint::Equal(new_then.r#type.clone(), tmp.r#type.clone()));
					box tmp
				});
				constraints.push(Constraint::Equal(new_then.r#type.clone(), i.then.r#type.clone()));
				constraints.push(Constraint::Equal(new_then.r#type.clone(), expr.r#type.clone()));
				constraints.push(Constraint::Equal(new_cond.r#type.clone(), i.cond.r#type.clone()));
				constraints.push(Constraint::Equal(new_cond.r#type.clone(), Bool));

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
				let new_var_t = instantiate(
					&self.substitutions,
					&instantiation,
					generic_t.uninstantiated
				);

				if !v.generics.is_empty() {
					assert!(v.generics.len() == new_generics.len());
					for (g1, g2) in v.generics.iter().zip(new_generics.iter()) {
						constraints.push(Constraint::Equal(g1.clone(), g2.clone()));
					}
				}
				constraints.push(Constraint::Equal(new_var_t, expr.r#type.clone()));

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
					r#type: get_type_var(),
				}, constraints);

				let lambda_substitutions = self.solve_constraints(constraints);
				let new_return_t = substitute(&lambda_substitutions, new_body.r#type.clone());

				let new_fn_t = TypeConstructor(TConstructor {
					name: "Function".to_string(),
					args:
						l.args.iter()
							.map(|v|
								substitute(&lambda_substitutions, v.r#type.clone())
							)
							.chain(iter::once(new_return_t))
							.collect(),
			 	});
				let new_args = l.args.into_iter()
					.map(|a|
						Parameter {
							name: a.name,
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
				let new_fn_t = TypeConstructor(
					TConstructor {
						name: "Function".to_string(),
						args: args_t.clone().into_iter().chain(iter::once(expr.r#type.clone())).collect(),
					}
				);

				let new_fn = self.infer(
					Expr {
						val: c.func.val,
						r#type: new_fn_t,
					}, constraints
				);

				let new_args =
					c.args.iter()
						.zip(args_t)
						.map(|(arg, t)| self.infer(
								Expr {
									val: arg.val.clone(),
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
			r#type: expr.r#type,
		}
	}

	fn solve_constraints(&mut self, constraints: &[Constraint]) -> HashMap<u16, Type> {
		let mut substitutions = HashMap::new();
		let mut member_map = HashMap::new();
		for constraint in constraints.iter() {
			match constraint {
				Constraint::Equal(t1, t2) => unify(&mut substitutions, t1.clone(), t2.clone()),

				Constraint::HasMember(t, m) => {
					member_map.entry(t).or_insert(vec![]).push(m.clone());
				}
			}
		}

		for (t, members) in member_map.into_iter() {
			// TODO: this is where row polymorphism will be added
			unify(&mut substitutions, t.clone(), Struct(members));
		}

		substitutions
	}
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
		TypeConstructor(tc) =>
			TypeConstructor(
				TConstructor {
					name: tc.name,
					args: tc.args
						.into_iter()
						.map(|t| instantiate(substitutions, instantiation, t))
						.collect(),
				}
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
		TypeConstructor(tc) => TypeConstructor(TConstructor {
			name: tc.name,
			args: tc.args
				.iter()
				.map(|t1| substitute(substitutions, t1.clone()))
				.collect::<Vec<Type>>(),
		}),
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

fn unify(substitutions: &mut HashMap<u16, Type>, t1: Type, t2: Type) {
	match (t1.clone(), t2.clone()) {
		(
			TypeConstructor(TConstructor {
				name: v1,
				args: args1,
			}),
			TypeConstructor(TConstructor {
				name: v2,
				args: args2,
			}),
		) => {
			if v1 != v2 {
				panic!("Type constructor {:#?} is not the same as type constructor {:#?}, but was attempted to be unified with it", v1, v2);
			}
			if args1.len() != args2.len() {
				panic!("type constructors with unequal lengths are attempting to be unified, {:#?} and {:#?}", t1, t2);
			}
			for (t3, t4) in args1.into_iter().zip(args2.into_iter()) {
				unify(substitutions, t3, t4);
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
				unify(substitutions, a1.r#type, a2.r#type);
			}
		}

		(Pointer(box r1), Pointer(box r2)) =>
			unify(substitutions, r1, r2),

		(TypeVar(i), TypeVar(j)) if i == j => {}
		(TypeVar(i), _) if substitutions.contains_key(&i) => {
			unify(substitutions, substitutions[&i].clone(), t2)
		}
		(_, TypeVar(j)) if substitutions.contains_key(&j) => {
			unify(substitutions, t1, substitutions[&j].clone())
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
			panic!("unified concrete types are not equal in substitutions {:#?}, {:#?} != {:#?}", substitutions, t1, t2),
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

		TypeConstructor(TConstructor { args: a, .. }) => {
			a.into_iter().any(|t1| occurs_in(substitutions, index, t1))
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
	let free_in_env = free_in_env(substitutions, outer_env);

	let generic_type_vars =
		free_in_type(substitutions, t).into_iter()
			.filter(|free_t| !free_in_env.contains(free_t))
			.collect();

	Lambda {
		body: l.body,
		args: l.args,
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
		TypeConstructor(tc) =>
			tc.args.iter()
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

	pub fn annotate(&mut self) {
		let mut inference = Inference::new();
		let mut constraints = Vec::new();
		*self = inference.infer(self.clone(), &mut constraints);
		inference.substitutions = inference.solve_constraints(&constraints);

		self.annotate_helper(&inference.substitutions, false);
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
