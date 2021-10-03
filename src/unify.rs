use std::collections::HashMap;

use crate::{
	ast::{Expr, ExprVal, Let, TConstructor, Type, Type::*, AST},
	lex::TokenLiteral,
	parse::get_type_var,
	tok::binary_op_result_type,
	core::core_vals,
};

struct Inference {
	env: Vec<HashMap<String, Type>>,
	constraints: Vec<(Type, Type)>,
	substitutions: HashMap<u16, Type>
}

impl Inference {
	fn new() -> Self {
		Inference {
			env: vec![core_vals()],
			constraints: vec![],
			substitutions: HashMap::new()
		}
	}

	fn env_insert(&mut self, key: String, value: Type) {
		self.env.last_mut().unwrap().insert(key, value);
	}

	fn env_find(&self, key: String) -> Option<Type> {
		for map in self.env.clone() {
			if map.contains_key(&key) {
				return map.get(&key).cloned();
			}
		}

		None
	}

	fn unify(&mut self, t1: Type, t2: Type) {
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
					self.unify(t3, t4);
				}
			}

			(TypeVar(i), TypeVar(j)) if i == j => {}
			(TypeVar(i), _) if self.substitutions.contains_key(&i) => {
				self.unify(self.substitutions[&i].clone(), t2)
			}
			(_, TypeVar(j)) if self.substitutions.contains_key(&j) => {
				self.unify(t1, self.substitutions[&j].clone())
			}

			(TypeVar(i), _) => {
				assert!(!self.occurs_in(i, t2.clone()));
				self.substitutions.insert(i, t2);
			}
			(_, TypeVar(j)) => {
				assert!(!self.occurs_in(j, t1.clone()));
				self.substitutions.insert(j, t1);
			}
	
			_ =>
				if t1 != t2 {
					panic!("unified concrete types are not equal {:#?} != {:#?}", t1, t2)
				}
		}
	}

	fn occurs_in(&mut self, index: u16, t: Type) -> bool {
		match t {
			TypeVar(i) if self.substitutions.contains_key(&i) => {
				self.occurs_in(index, self.substitutions[&i].clone())
			}
			TypeVar(i) => i == index,
			TypeConstructor(TConstructor { args: a, .. }) => {
				a.into_iter().any(|t1| self.occurs_in(index, t1))
			}
			Void | Int | Float | Str | Bool => false
		}
	}

	fn infer_type(&mut self, a: AST) -> Type {
		match a {
			AST::LetNode(l) => {
				self.env_insert(l.clone().var.name, l.clone().var.r#type);
				if let Some(def) = l.def {
					let def_t = self.infer_type(AST::ExprNode(*def.clone()));
					self.constraints.push(((*def).r#type, def_t.clone()));
					self.constraints.push((l.var.r#type, def_t));
				}
				Void
			},
			AST::ExprNode(expr) => {
				let expr_type = match expr.val.clone() {	
					ExprVal::LiteralNode(lit) =>
						match lit {
							TokenLiteral::StrLit(_) => Type::Str,
							TokenLiteral::IntLit(_) => Type::Int,
							TokenLiteral::FltLit(_) => Type::Float,
							TokenLiteral::BoolLit(_) => Type::Bool
						}, ExprVal::UnaryNode(u) => {
							self.infer_type(AST::ExprNode(*u.expr))
						}, ExprVal::BinaryNode(b) => {
							let (t_left, t_right) = (self.infer_type(AST::ExprNode(*b.left.clone())), self.infer_type(AST::ExprNode(*b.right.clone())));
							self.constraints.push((t_left.clone(), b.left.r#type.clone()));
							self.constraints.push((t_right.clone(), b.right.r#type.clone()));
	
							/* TODO: constraints should not assume binaries only apply to symmetric operand types */
							self.constraints.push((t_left.clone(), t_right.clone()));
							binary_op_result_type(b.op, t_left, t_right)
						},
						

					ExprVal::BlockNode(b) => {
						self.env.push(HashMap::new());
						for boxed_line in b.range(..b.len() - 1) {
							self.infer_type((**boxed_line).clone());
						}
						let block_val_t = match b.back() {
							Some(line) => self.infer_type((**line).clone()),
							_ => Void
						};
						self.env.pop();

						block_val_t
					},

					ExprVal::IfNode(i) => {
						let if_t = self.infer_type(AST::ExprNode(*i.then.clone()));
						let cond_t = self.infer_type(AST::ExprNode(*i.cond.clone()));
						if let Some(expr) = i.r#else {
								let else_t = self.infer_type(AST::ExprNode(*expr));
								self.constraints.push((if_t.clone(), else_t));
						}
						self.constraints.push((if_t.clone(), i.then.r#type.clone()));
						self.constraints.push((cond_t.clone(), i.cond.r#type.clone()));
						self.constraints.push((cond_t, Type::Bool));

						if_t
					},

					ExprVal::IdentNode(v) => self.env_find(v.clone())
													.unwrap_or_else(|| panic!("unable to find variable {:#?} in the environment when inferring types, env looks like {:#?}",
																v,
																self.env.to_vec())),
			
					ExprVal::LambdaNode(l) => {
						self.env.push(HashMap::new());
						for arg in l.args.iter() {
							self.env_insert(arg.name.clone(), arg.r#type.clone());
						}

						let (return_type, body_type) = (self.infer_type(AST::ExprNode(*l.body.clone())),
															l.body.r#type.clone());

						self.constraints.push((return_type.clone(), body_type));

						let fn_type = TypeConstructor(TConstructor {	
							name: "Function".to_string(), 
							args: {
								let mut tmp = l.args.iter()
												.map(|v| v.r#type.clone())
												.collect::<Vec<Type>>();
								tmp.push(return_type);
								tmp
							}
					 	});

						self.env.pop();

						fn_type
					},

					ExprVal::CallNode(c) => {
						let t1 = self.infer_type(AST::ExprNode(*c.func.clone()));
						self.constraints.push((t1.clone(), c.func.r#type.clone()));

						let mut arg_types = c.args.iter()
											.map(|t| self.infer_type(AST::ExprNode((*t).clone())))
											.into_iter()
											.collect::<Vec<Type>>();
						let return_type = get_type_var();
						arg_types.push(return_type.clone());

						self.constraints.push((t1, TypeConstructor(TConstructor {
							name: "Function".to_string(),
							args: arg_types
						})));
						return_type
					},
				};

				self.constraints.push((expr_type.clone(), expr.r#type));
				expr_type
			}
		}
	}

	fn solve_constraints(&mut self) {
		for (t1, t2) in self.constraints.clone() {
			self.unify(t1, t2);
		}
		self.constraints.clear();
	}

	fn substitute(&self, t: Type) -> Type {
		match t {
			TypeVar(n) if self.substitutions.contains_key(&n) =>
										self.substitute(self.substitutions
															.get(&n)
															.unwrap_or_else(|| panic!("No substitution available for variable {}, substitutions is {:#?}", n, self.substitutions))
															.clone()),
			TypeVar(n) => panic!("AAAAA no substitution for {} AAAAAA substitutions is {:#?}", n, self.substitutions),
			TypeConstructor(tc) => TypeConstructor(TConstructor {
				name: tc.name,
				args: tc
					.args
					.iter()
					.map(|t1| self.substitute(t1.clone()))
					.collect::<Vec<Type>>(),
			}),
	
			_ => t,
		}
	}
}

impl Expr {
	fn annotate_helper(&mut self, inference: &mut Inference) {
		self.r#type = inference.substitute(self.r#type.clone());
		match self.val {
			ExprVal::LiteralNode(_) | ExprVal::IdentNode(_) => {}

			ExprVal::BlockNode(ref mut b) => {
				for line in b.iter_mut() {
					line.annotate_helper(inference);
				}
			}

			ExprVal::LambdaNode(ref mut l) => {
				for var in &mut l.args {
					var.r#type = inference.substitute(var.clone().r#type);
				}
				l.body.annotate_helper(inference);
			}

			ExprVal::IfNode(ref mut i) => {
				i.cond.annotate_helper(inference);
				i.then.annotate_helper(inference);
				if let Some(ref mut else_branch) = i.r#else {
					else_branch.annotate_helper(inference);
				}
			}

			ExprVal::BinaryNode(ref mut b) => {
				b.left.annotate_helper(inference);
				b.right.annotate_helper(inference);
			}

			ExprVal::CallNode(ref mut c) => {
				c.func.annotate_helper(inference);
				for arg in &mut c.args {
					arg.annotate_helper(inference);
				}
			}

			ExprVal::UnaryNode(ref mut u) => {
				u.expr.annotate_helper(inference);
			}
		}
	}

	pub fn annotate(&mut self) {
		let mut inference = Inference::new();
		self.r#type = inference.infer_type(AST::ExprNode(self.clone()));

		inference.solve_constraints();

		self.annotate_helper(&mut inference);
	}
}

impl AST {
	fn annotate_helper(&mut self, inference: &mut Inference) {
		match self {
			AST::ExprNode(e) => e.annotate_helper(inference),
			AST::LetNode(Let {
				var: v,
				def: Some(expr)
			}) => {
				v.r#type = inference.substitute(v.r#type.clone());
				expr.annotate_helper(inference);
			}
			_ => {}
		}
	}
}