use crate::Type::*;

lazy_static! {
	static ref SUBSTITUTIONS: Mutex<HashMap<u16, Type>> = Mutex::new(HashMap::new());
	static ref TYPE_CONSTRAINTS: Mutex<Vec<(Type, Type)>> = Mutex::new(vec![]);
}

macro_rules! substitution {
	() => {
		SUBSTITUTIONS.lock().unwrap();
	};
}

fn addConstraint(t1: Type, t2: Type) {
	TYPE_CONSTRAINTS.lock().unwrap().push((t1, t2));
}

macro_rules! constraints {
	() => {
		TYPE_CONSTRAINTS.lock().unwrap().to_vec();
	};
}

fn unify(t1: Type, t2: Type, substitution: HashMap<u16, Type>) -> HashMap<u16, Type> {
	match (t1.clone(), t2.clone()) {
		(TypeConstructor(TConstructor { name: v1, args: args1 }), TypeConstructor(TConstructor { name: v2, args: args2 })) => {
			assert!(v1 == v2);
			assert!(args1.len() == args2.len());
			for (t3, t4) in args1.into_iter().zip(args2.into_iter()) {
				unify(t3, t4, substitution);
			}
		},

		(TypeVar(i), TypeVar(j)) if i == j => {},
		(TypeVar(i), _) if substitution.contains_key(&i) =>
			unify(substitution[&i].clone(), t2, substitution),
		(_, TypeVar(j)) if substitution.contains_key(&j) =>
			unify(t1, substitution[&j].clone(), substitution),

		(TypeVar(i), _) => {
			assert!(!occursIn(i, t2.clone()));
			substitution.insert(i, t2);
		},
		(_, TypeVar(j)) => {
			assert!(!occursIn(j, t1.clone()));
			substitution.insert(j, t1.clone());
		},

		_ => panic!("attempted a weird unification between {:#?} and {:#?}", t1, t2)
	}
	return substitution;
}

fn occursIn(index: u16, t: Type, substitutions: HashMap<u16, Type>) -> bool {
	return match t {
		TypeVar(i) if substitutions.contains_key(&i) =>
			occursIn(index, substitutions[&i].clone(), substitutions),
		TypeVar(i) =>
			i == index,
		TypeConstructor(TConstructor { args: a, .. }) =>
			a.iter().any(|t1| occursIn(index, (*t1).clone(), substitutions)),
		Void | Int | Float | Str | Bool => false
	};
}


fn inferType(ast: AST, env: HashMap<Variable, Type>) -> Type {
	match ast {
		LetNode(_) =>
				Void,
		ExprNode(e) => match e.val {
			LiteralNode(lit) =>
				match lit {
					StrLit(_) => Type::Str,
					IntLit(_) => Type::Int,
					FltLit(_) => Type::Float,
					BoolLit(_) => Type::Bool
				},

			UnaryNode(u) => {
				let t = inferType(ExprNode(*u.expr), env);
				match u.op {
					Not => assert!(t == Bool),
					Minus => assert!(t == Int || t == Float),
					_ => panic!("operator {:?} was attempted to be unified as a unary expression, but is not a unary operator (should never happen)", u.op)
				}
				t
			},
			
			BinaryNode(b) => {
				let (t_left, t_right) = (inferType(ExprNode(*b.left), env.clone()), inferType(ExprNode(*b.right), env.clone()));
				assert!(t_left == t_right);
				addConstraint(t_left.clone(), t_right);
				/* TODO: typechecking for binaries should be dependent on what the op is, FIX THIS */
				t_left
			},
				

			BlockNode(b) =>
				match b.back().clone() {
					None => Void,
					Some(line) => inferType((**line).clone(),  env.clone())
				},

			IfNode(IfElse { then: if_branch, r#else: else_branch, .. }) => {
				let if_t = inferType(ExprNode(*if_branch), env.clone());
				match else_branch {
					Some(expr) => {
						let else_t = inferType(ExprNode(*expr), env.clone());
						assert!(if_t == else_t);
					},
					_ => {}
				}

				return if_t;
			},

			IdentNode(v) => env.get(&v).unwrap_or_else(|| panic!("unable to find variable {:#?} in the environment when inferring types", v)).clone(),

			LambdaNode(l) => TypeConstructor(TConstructor {
				name: format!("Function{}", l.args.len()), 
				args: {
						let mut tmp = l.args.iter()
									.map(|v| v.r#type.clone())
									.collect::<Vec<Type>>();
						tmp.push(inferType(*l.body, env));
						tmp
					}
			}),

			CallNode(c) => {
				let t1 = get_type_var();
				
				let argTypes = c.args.iter()
									.map(|t| return inferType(ExprNode((*t).clone()), env.clone()))
									.collect::<VecDeque<Type>>();
				let returnType = get_type_var();

				addConstraint(t1, TypeConstructor(TConstructor {
					name: format!("Function{}", argTypes.len()),
					args: {
							let mut tmp = argTypes.into_iter()
								.collect::<Vec<Type>>();
							tmp.push(returnType.clone());
							tmp
						}
				}));
				returnType
			},
		}
	}
}

fn solve_constraints() {
	for (t1, t2) in constraints!() {
		unify(t1, t2, HashMap::new());
		constraints!().clear();
	}
}

fn substitute(t: Type, substitution: HashMap<u16, Type>) -> Type {
	match t {
		TypeVar(n) => substitution.get(&n).unwrap_or_else(|| panic!("No substitution available for variable {}", n)).clone(),
		TypeConstructor(tc) => TypeConstructor(
							TConstructor {
								name: tc.name,
								args: tc.args.iter()
										.map(|t1| substitute(t1.clone(), substitution))
										.collect::<Vec<Type>>()
							}),

		_ => t
	}
}/*

fn type_annotate(a: AST) -> AST {
	let constraints = 
	t = match a {
		ExprNode(e) =>
			match e.r#type {
				TypeVar(_) | TypeConstructor(_) => */