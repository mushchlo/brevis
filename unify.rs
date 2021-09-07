use crate::Type::*;

lazy_static! {
	static ref SUBSTITUTIONS: Mutex<HashMap<u16, Type>> = Mutex::new(HashMap::new());
	static ref TYPE_CONSTRAINTS: Mutex<Vec<(Type, Type)>> = Mutex::new(vec![]);
}

fn substitution() -> &'static mut HashMap<u16, Type> {
	&mut SUBSTITUTIONS.lock().unwrap()
}

fn addConstraint(t1: Type, t2: Type) {
	TYPE_CONSTRAINTS.lock().unwrap().push((t1, t2));
}

fn constraints() -> Vec<(Type, Type)> {
	TYPE_CONSTRAINTS.lock().unwrap().to_vec()
}

fn unify(t1: Type, t2: Type) {
	match (t1, t2) {
		(TypeConstructor(TConstructor { name: v1, args: args1 }), TypeConstructor(TConstructor { name: v2, args: args2 })) => {
			assert!(v1 == v2);
			assert!(args1.len() == args2.len());
			for (&t3, &t4) in args1.iter().zip(args2.iter()) {
				unify(t3, t4);
			}
		},

		(TypeVar(i), TypeVar(j)) if i == j => {},
		(TypeVar(i), _) if substitution().contains_key(&i) =>
			unify(substitution()[&i], t2),
		(_, TypeVar(j)) if substitution().contains_key(&j) =>
			unify(t1, substitution()[&j]),

		(TypeVar(i), _) => {
			assert!(!occursIn(i, t2));
			substitution().insert(i, t2);
		},
		(_, TypeVar(j)) => {
			assert!(!occursIn(j, t2));
			substitution().insert(j, t2);
		},

		_ => panic!("attempted a weird unification between {:#?} and {:#?}", t1, t2)
	}
}

fn occursIn(index: u16, t: Type) -> bool {
	return match t {
		TypeVar(i) if substitution().contains_key(&i) =>
			occursIn(index, substitution()[&i]),
		TypeVar(i) =>
			i == index,
		TypeConstructor(TConstructor { args: a, .. }) =>
			a.iter().any(|&t1| occursIn(index, t1)),

		_ => panic!("attempted a weird occursIn with type {:#?}", t)
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

			BlockNode(b) =>
				match b.back() {
					None => Void,
					Some(&line) => inferType(*line,  env)
				},

			IfNode(IfElse { then: if_branch, r#else: else_branch, .. }) => {
				let if_t = inferType(ExprNode(*if_branch), env);
				match else_branch {
					Some(expr) => {
						let else_t = inferType(ExprNode(*expr), env);
						assert!(if_t == else_t);
					},
					_ => {}
				}

				return if_t;
			},

			IdentNode(v) => *env.get(&v).unwrap_or_else(|| panic!("unable to find variable {:#?} in the environment when inferring types", v)),

			LambdaNode(l) => TypeConstructor(TConstructor {
				name: &*format!("Function{}", l.args.len()), 
				args: {
						let tmp = l.args.iter()
									.map(|v| v.r#type)
									.collect::<Vec<Type>>();
						tmp.push(inferType(*l.body, env));
						tmp
					}
			}),

			CallNode(c) => {
				let t1 = get_type_var();
				
				let argTypes = c.args.iter()
									.map(|&t| return inferType(ExprNode(t), env))
									.collect::<VecDeque<Type>>();
				let returnType = get_type_var();

				addConstraint(t1, TypeConstructor(TConstructor {
					name: &*format!("Function{}", argTypes.len()),
					args: {
							let tmp = argTypes.iter()
								.collect::<Vec<Type>>();
							tmp.push(returnType);
							tmp
						}
				}));
				returnType
			},
		}
	}
}