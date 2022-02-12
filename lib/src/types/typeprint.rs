use std::collections::HashMap;

use crate::parse::ast::{
	Type,
	Type::*,
};

impl std::fmt::Display for Type {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{}", display_type(self, &mut HashMap::new()))
	}
}

// Used in errors like "expected an integer, ..."
pub fn name_of(t: &Type) -> &str {
	match t {
		Void => " void literal",
		Int => "n integer",
		Float => " floating-point number",
		Str => " string",
		Bool => " boolean",
		TypeVar(_) => " generic value",
		Struct(_) => " structure",
		Pointer(_) => " pointer",
		TypeConstructor(tc) if &tc.name == "Function" => " function",
		TypeConstructor(_) => panic!(),
	}
}

fn display_type(t: &Type, generic_names: &mut HashMap<u16, char>) -> String {
	match t {
		Void => "void".to_string(),
		Int => "int".to_string(),
		Float => "float".to_string(),
		Str => "string".to_string(),
		Bool => "bool".to_string(),
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
		Pointer(box pointed_t) => format!("&{}", display_type(pointed_t, generic_names)),
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
		TypeConstructor(tc) if &tc.name == "Function" => {
			let mut args_t = tc.args.clone();
			let ret_t = args_t.pop().unwrap();
			format!("fn ({}) -> {}",
				args_t.iter()
					.map(|arg_t| display_type(arg_t, generic_names))
					.reduce(|acc, next|
						acc + ", " + &next
					)
					.unwrap(),
				display_type(&ret_t, generic_names)
			)
		}
		TypeConstructor(tc) => panic!("Can't pretty-print this type of type constructor: {}", tc.name)
	}
}