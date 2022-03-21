use std::sync::Mutex;
use std::collections::HashMap;

use crate::{
	lex::cradle::SourceLoc,
	parse::ast::{
		Expr,
		ExprVal,
		Parameter,
	},
	core::core_vals,
};

impl Expr {
	pub fn annotate_captures(&mut self) {
		let cloned_core_vals = core_vals.clone();

		// This environment's sub-stacks aren't added and removed on any change
		// of scope, just those due to function entrances and exits. The first
		// stack is special, as globals aren't captured, they're already in scope in
		// generated code.
		let env: Mutex<Vec<HashMap<String, Parameter>>> =
			Mutex::new(vec![
				cloned_core_vals.into_iter()
					.map(|(name, r#type)|
						(name.clone(), Parameter {
							name,
							r#type,
							mutable: false,
							name_loc: SourceLoc::nonexistent(),
							type_loc: None,
						})
					).collect()
			]);

		// While traversing the AST, we push and pop fresh stacks on the
		// main stack when we enter and leave functions, and push any variables
		// used that aren't in the most recent scope of the environment onto
		// substacks.
		let captured_stack: Mutex<Vec<Vec<Parameter>>> =
			Mutex::new(vec![ Vec::new() ]);

		let transifier = |e: &mut Expr| {
			let mut env = env.lock().unwrap();
			let mut captured_stack = captured_stack.lock().unwrap();
			match &e.val {
				ExprVal::Var(var) => {
					if !env.last().unwrap().contains_key(&var.name)
						&& !env.first().unwrap().contains_key(&var.name) {
							captured_stack.last_mut().unwrap()
								.push(env.iter()
									.find_map(|hashmap| hashmap.get(&var.name))
									.unwrap_or_else(|| panic!("var {} resulted in no map in {:#?} :(", var.name, env))
									.clone()
								);
					}
				}
				ExprVal::Lambda { args, .. } => {
					captured_stack.push(Vec::new());
					env.push(
						args.iter()
							.map(|p| (p.name.clone(), p.clone()))
							.collect()
					);
				}
				ExprVal::Let { declared, .. } => {
					for declared in declared.assignees() {
						env.last_mut().unwrap().insert(declared.name.clone(), declared.clone());
					}
				}

				_ => {}
			}

			true
		};
		let post_transifier = |e: &mut Expr| {
			if let ExprVal::Lambda { captured, .. } = &mut e.val {
				env.lock().unwrap().pop();
				*captured = captured_stack.lock().unwrap()
					.pop().unwrap().into_iter().collect();
			}
		};

		self.transform(transifier, post_transifier);
	}
}