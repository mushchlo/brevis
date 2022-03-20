use std::{
	collections::HashMap,
	sync::Mutex,
};

use crate::{
	parse::ast::{
		Expr,
		ExprVal,
	},
};

// Expects an inferred, ANFified, monomorphized AST
pub fn optimize(e: &mut Expr) {
	remove_dead_code(e);
}

// Only removes useless declarations for now.
fn remove_dead_code(e: &mut Expr) {
	use self::ExprVal::*;

// All of the variables declared in a block. Popped from when we leave
// a block, which allows us to keep the used variable structure table
// as just a simple hashmap, which we remove from when we exit a block.
	let declared_in_block: Mutex<Vec<Vec<String>>> =
		Mutex::new(Vec::new());

	let used: Mutex<HashMap<String, bool>> =
		Mutex::new(HashMap::new());

	let use_counter = |ex: &mut Expr| {
		let mut declared_in_block = declared_in_block.lock().unwrap();
		let mut used = used.lock().unwrap();
		match &mut ex.val {
			Let(l) => {
				for assignee in l.declared.assignees() {
					declared_in_block.last_mut().unwrap().push(assignee.name.clone());
					used.insert(assignee.name.clone(), false);
				}

			}

			Block(_) => {
				declared_in_block.push(Vec::new());
			}

			Var(var) => {
				used.insert(var.name.clone(), true);
			}

			_ => {}
		}
		true
	};

	let deleter = |ex: &mut Expr| {
		let mut declared_in_block = declared_in_block.lock().unwrap();
		let mut used = used.lock().unwrap();
		match &ex.val {
			Block(b) => {
				ex.val = Block(
					b.clone().into_iter()
						.filter(|line|
							if let Let(l) = &line.val {
								l.declared.assignees().iter().all(|a| used[&a.name])
							} else {
								true
							}
						)
						.collect()
				);
				for declared in declared_in_block.pop().unwrap() {
					used.remove(&declared);
				}
			}

			Let(l) => {
			// Nice try, recursively defined values.
				for declared in l.declared.assignees() {
					used.insert(declared.name.clone(), false);
				}
			}

			_ => {}
		}
	};

	e.transform(use_counter, deleter);
}