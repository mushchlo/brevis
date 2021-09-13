use crate::Type::*;

lazy_static! {
	static ref SUBSTITUTIONS: Mutex<HashMap<u16, Type>> = Mutex::new(HashMap::new());
	static ref TYPE_CONSTRAINTS: Mutex<Vec<(Type, Type)>> = Mutex::new(vec![]);
	static ref ENVIRONMENT: Mutex<Vec<HashMap<String, Type>>> = Mutex::new(vec![]);
}

macro_rules! substitution {
	() => {
		SUBSTITUTIONS.lock().unwrap()
	};
}

macro_rules! constraints {
	() => {
		TYPE_CONSTRAINTS.lock().unwrap().to_vec()
	};
}

macro_rules! environment {
	() => {
		ENVIRONMENT.lock().unwrap().to_vec()
	};
}

fn addConstraint(t1: Type, t2: Type) {
	TYPE_CONSTRAINTS.lock().unwrap().push((t1, t2));
}

fn env_find(key: String) -> Option<Type> {
	let environment = ENVIRONMENT.lock().unwrap().to_vec();
	for map in environment {
		if map.contains_key(&key) {
			return map.get(&key).cloned();
		}
	}

	return None;
}

fn env_insert(key: String, value: Type) {
	let mut environment = ENVIRONMENT.lock().unwrap();
println!("inserting {}: {:#?}", key, value);
	println!("insertion value is {:#?}", environment.last_mut().unwrap().insert(key, value));
println!("env looks like this after insert: {:#?}", environment);
}

fn env_new_stack() {
	let mut environment = ENVIRONMENT.lock().unwrap();
	environment.push(HashMap::new());
}

fn env_pop_stack() {
	let mut environment = ENVIRONMENT.lock().unwrap();
println!("env looks like this before pop: {:#?}", environment);
	environment.pop();
println!("env looks like this after pop: {:#?}", environment);
}
	
	

fn unify(t1: Type, t2: Type) {
	match (t1.clone(), t2.clone()) {
		(TypeConstructor(TConstructor { name: v1, args: args1 }), TypeConstructor(TConstructor { name: v2, args: args2 })) => {
			assert!(v1 == v2);
			assert!(args1.len() == args2.len());
			for (t3, t4) in args1.into_iter().zip(args2.into_iter()) {
				unify(t3, t4);
			}
		},

		(TypeVar(i), TypeVar(j)) if i == j => {},
		(TypeVar(i), _) if substitution!().contains_key(&i) =>
			unify(substitution!()[&i].clone(), t2),
		(_, TypeVar(j)) if substitution!().contains_key(&j) =>
			unify(t1, substitution!()[&j].clone()),

		(TypeVar(i), _) => {
			assert!(!occursIn(i, t2.clone()));
			substitution!().insert(i, t2);
		},
		(_, TypeVar(j)) => {
			assert!(!occursIn(j, t1.clone()));
			substitution!().insert(j, t1.clone());
		},

		_ => panic!("attempted a weird unification between {:#?} and {:#?}", t1, t2)
	}
}

fn occursIn(index: u16, t: Type) -> bool {
	return match t {
		TypeVar(i) if substitution!().contains_key(&i) =>
			occursIn(index, substitution!()[&i].clone()),
		TypeVar(i) =>
			i == index,
		TypeConstructor(TConstructor { args: a, .. }) =>
			a.iter().any(|t1| occursIn(index, (*t1).clone())),
		Void | Int | Float | Str | Bool => false
	};
}

fn inferType(a: AST) -> Type {
	match a {
		LetNode(l) => {
			env_insert(l.clone().var.name, l.clone().var.r#type);
			if let Some(def) = l.def {
				let def_t = inferType(ExprNode(*def));
				addConstraint(l.var.r#type, def_t);
			}
			Void
		},
		ExprNode(expr) => match expr.val {
			LiteralNode(lit) =>
				match lit {
					StrLit(_) => Type::Str,
					IntLit(_) => Type::Int,
					FltLit(_) => Type::Float,
					BoolLit(_) => Type::Bool
				},
	
			UnaryNode(u) => {
				let t = inferType(ExprNode(*u.expr));
				match u.op {
					Not => assert!(t == Bool),
					Minus => assert!(t == Int || t == Float),
					_ => panic!("operator {:?} was attempted to be unified as a unary expression, but is not a unary operator (should never happen)", u.op)
				}
				t
			},
			
			BinaryNode(b) => {
				let (t_left, t_right) = (inferType(ExprNode(*b.left)), inferType(ExprNode(*b.right)));
//				assert!(t_left == t_right);
				addConstraint(t_left.clone(), t_right);
				/* TODO: typechecking for binaries should be dependent on what the op is, FIX THIS */
				t_left
			},
				
	
			BlockNode(b) => {
				env_new_stack();
println!("entering block, new stack");
				for boxed_line in b.range(..b.len() - 1) {
println!("line is {:#?}", *boxed_line);
					inferType((**boxed_line).clone());
				}
println!("popping stack bc block end");
				env_pop_stack();
				match b.back() {
					Some(line) => inferType((**line).clone()),
					_ => Void
				}
			},
	
			IfNode(i) => {
println!("doing an if (and your mom)");
//				assert!(inferType(ExprNode(*i.cond)) == Bool);
				let if_t = inferType(ExprNode(*i.then));
				match i.r#else {
					Some(expr) => {
						let else_t = inferType(ExprNode(*expr));
//						println!("if_t is `{:#?}`, else_t is `{:#?}`", if_t, else_t);
//						assert!(if_t == else_t);
					},
					_ => {}
				}
println!("finished doing if (but not your mom)");
	
				return if_t;
			},
	
			IdentNode(v) => env_find(v.name.clone()).unwrap_or_else(|| panic!("unable to find variable {:#?} in the environment when inferring types, env looks like {:#?}", v.name, environment!().to_vec())).clone(),
	
			LambdaNode(l) => {
println!("entering fn, new stack");
				env_new_stack();
				for arg in &l.args {
					env_insert(arg.name.clone(), arg.r#type.clone());
				}

				let fn_type = TypeConstructor(TConstructor {	
						name: format!("Function{}", l.args.len()), 
						args: {
								let mut tmp = l.args.iter()
												.map(|v| v.r#type.clone())
												.collect::<Vec<Type>>();
								tmp.push(if let ExprNode(e) = *l.body {
											inferType(ExprNode(e))
										} else { Void });
								tmp
						}
					});
println!("popping env bc fn end");

				env_pop_stack();

				return fn_type;
			},
	
			CallNode(c) => {
println!("inferring type of function, env looks like this: {:#?}", environment!());
				let t1 = inferType(ExprNode(*c.clone().func));
				println!("inferring type of func {:#?} as {:#?}", *c.clone().func, t1.clone());
				
				let arg_types = c.args.iter()
									.map(|t| return inferType(ExprNode((*t).clone())))
									.collect::<VecDeque<Type>>();
				let return_type = get_type_var();
	
				addConstraint(t1, TypeConstructor(TConstructor {
					name: format!("Function{}", arg_types.len()),
					args: {
							let mut tmp = arg_types.into_iter()
								.collect::<Vec<Type>>();
							tmp.push(return_type.clone());
							tmp
						}
				}));
				return_type
			},
		},

		_ => Void
	}
}

fn solve_constraints() {
	for (t1, t2) in constraints!() {
		unify(t1, t2);
		constraints!().clear();
	}
}

fn substitute(t: Type) -> Type {
	match t {
		TypeVar(n) => substitution!().get(&n).unwrap_or_else(|| panic!("No substitution available for variable {}", n)).clone(),
		TypeConstructor(tc) => TypeConstructor(
							TConstructor {
								name: tc.name,
								args: tc.args.iter()
										.map(|t1| substitute(t1.clone()))
										.collect::<Vec<Type>>()
							}),

		_ => t
	}
}

impl Expr {
	fn annotate_helper(&mut self) {
		self.r#type = substitute(self.clone().r#type);
		match self.val {
			LiteralNode(_) | IdentNode(_) => {},
			BlockNode(ref mut b) =>
					for ref mut line in b {
						line.annotate_helper();
					},
				LambdaNode(ref mut l) => {
					for ref mut var in l.clone().args {
						var.r#type = substitute(var.clone().r#type);
					}
					l.body.annotate_helper();
				},
				IfNode(ref mut i) => {
					i.clone().cond.annotate_helper();
					i.clone().then.annotate_helper();
					if let Some(mut else_branch) = i.clone().r#else {
						else_branch.annotate_helper();
					}
				},

				_ => panic!("annotating this thing is not yet supported")
		}
	}

	fn annotate(&mut self) {
println!("inferring types");
		self.r#type = inferType(ExprNode(self.clone()));
println!("solving constraints");
		solve_constraints();
println!("annotating types");
		self.annotate_helper();
	}
}

impl AST {
	fn annotate_helper(&mut self) {
		match self {
			ExprNode(e)	=> e.annotate_helper(),
			LetNode(Let { def: Some(expr), .. }) => expr.annotate_helper(),

			_ => {}
		}
	}
}