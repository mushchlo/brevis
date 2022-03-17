use std::collections::HashMap;

use crate::types::{
	Type,
	Type::*,
	TypeVarId,
};

impl std::fmt::Display for Type {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{}", display_type(self, &mut HashMap::new()))
	}
}

// Used in errors like "expected an integer, ..."
pub fn name_of(t: &Type) -> String {
	match t {
		Void => " void literal".to_string(),
		Int => "n integer".to_string(),
		Float => " floating-point number".to_string(),
		Str => " string".to_string(),
		Bool => " boolean".to_string(),
		Forall(_, sub_t) => format!(" generic {}", name_of(sub_t)),
		TypeVar(_) => " generic value".to_string(),
		Struct(_) => " structure".to_string(),
		Pointer(_, false) => " pointer to a".to_string(),
		Pointer(_, true) => " pointer to a mutable".to_string(),
		Func(_) => " function".to_string(),
	}
}

fn display_type(t: &Type, generic_names: &mut HashMap<TypeVarId, char>) -> String {
	match t {
		Void => "void".to_string(),
		Int => "int".to_string(),
		Float => "float".to_string(),
		Str => "string".to_string(),
		Bool => "bool".to_string(),
		Forall(args, sub_t) =>
			format!("forall {} => {}",
				args.iter()
					.map(|&tv| display_type(&TypeVar(tv), generic_names))
					.reduce(|acc, next| acc + ", " + &next)
					.unwrap_or_else(|| "".to_string()),
				display_type(sub_t, generic_names)
			),
		TypeVar(v) => {
			let generic_names_copy = generic_names.clone();
			let entry = generic_names.entry(*v);
			let name = entry.or_insert_with(||
				('a'..='z')
					.chain('A'..='Z')
					.find(|c| !generic_names_copy.iter().any(|(_, name)| name == c))
					.unwrap_or_else(||
						panic!("Ran out of generic names!")
					)
			);

			format!("'{}", name)
		}
		Pointer(box pointed_t, mutable) =>
			format!("&{}{}",
				if *mutable { "mut" } else { "" },
				display_type(pointed_t, generic_names)
			),
		Struct(s) =>
			format!("[ {} ]",
				s.iter()
					.map(|agg|
						format!("{}: {}", agg.name, display_type(&agg.r#type, generic_names))
					)
					.reduce(|acc, next|
						acc + ", " + &next
					)
					.unwrap()
			),
		Func(args_t) => {
			let mut cloned_args = args_t.clone();
			let ret_t = cloned_args.pop().unwrap();
			format!("fn ({}) -> {}",
				cloned_args.iter()
					.map(|arg_t| display_type(arg_t, generic_names))
					.reduce(|acc, next|
						acc + ", " + &next
					)
					.unwrap(),
				display_type(&ret_t, generic_names)
			)
		}
	}
}