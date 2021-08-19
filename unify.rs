use std::collections::HashMap;

lazy_static! {
	static ref SUBSTITUTIONS: Mutex<HashMap<u16, Type>> = Mutex::new(HashMap::new());
}

fn substitution() -> &mut HashMap<u16, Type> {
	SUBSTITUTIONS.lock().unwrap()
}

fn unify(t1: Type, t2: Type) {
	match (t1, t2) {
		(TypeConstructor((v1, args1)), TypeConstructor((v2, args2))) => {
			assert!(v1 == v2);
			assert!(args1.len() == args2.len());
			for (t3, t4) in args1.iter().zip(args2.iter()) {
				unify(t3, t4);
			}
		},

		(TypeVar(i), TypeVar(j)) if i == j => {},
		(TypeVar(i), _) if substitution().contains_key(i) =>
			unify(substitution[i], t2),
		(_, TypeVar(j)) if substitution().contains_key(j) =>
			unify(t1, substitution()[j]),

		(TypeVar(i), _) => {
			assert!(!occursIn(i, t2)),
			substitution().insert(i, t2)
		},
		(_, TypeVar(j)) => {
			assert!(!occursIn(j, t2)),
			substitution().insert(j, t2)
		},

		_ => panic!("attempted a weird unification between {:#?} and {:#?}", t1, t2)
	}
}

fn occursIn(index: u16, t: Type) -> bool {
	return match t {
		TypeVar(i) if substitution().contains_key(i) =>
			occursIn(index, substitution()[i]),
		TypeVar(i) =>
			i == index,
		TConstructor((_, args)) =>
			args.iter().any(|t1| occursIn(index, t1)),

		_ => panic!("attempted a weird occursIn with type {:#?}", t)
	};
}