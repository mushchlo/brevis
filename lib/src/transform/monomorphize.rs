use std::collections::{
	HashMap,
	VecDeque,
};
use std::sync::{
	atomic::{AtomicUsize, Ordering},
	Mutex,
};

use crate::{
	lex::cradle::SourceLoc,
	parse::ast::{
		Expr,
		ExprVal::*,
		Let,
		Pattern,
		Parameter,
		Variable,
	},
	core::core_vals,
	types::{
		Type,
		TypeVarId,
		annotate_helper,
	},
	util::Env,
};


static MONOMORPHIZE_COUNTER: AtomicUsize = AtomicUsize::new(0);


type DefTypevars = (Expr, Vec<TypeVarId>);
type Instantiation = Vec<(TypeVarId, Type)>;


// Monomorphization expects an already-inferred
// AST, and thus should be run after type inference.

// For every block in an AST, monomorphize() will place new declarations of monomorphized
// functions in context, and modify calls to refer to those monomorphized functions by their proper
// names.
impl Expr {
	pub fn monomorphize(&mut self) {
		let fns: Mutex<Vec<HashMap<String, DefTypevars>>> =
			Mutex::new(Vec::new());

		let monomorphized_fns: Mutex<HashMap<(String, Instantiation), String>> =
			Mutex::new(HashMap::new());

		let pre_trans = |e: &mut Expr| {
			let mut fns = fns.lock().unwrap();
			let mut monomorphized_fns = monomorphized_fns.lock().unwrap();

			match &mut e.val {
				BlockNode(_) => {
					fns.push(HashMap::new());
				}

				VarNode(ref mut v) => {
					if !v.generics.is_empty() && !core_vals.contains_key(&v.name) {
						let generics = v.generics.clone();
						let mono_fn_name =
							monomorphized_fns
								.entry((v.name.clone(), generics.into_iter().collect()))
								.or_insert_with(||
									format!("monomorphized_{}__{}",
										MONOMORPHIZE_COUNTER.fetch_add(1, Ordering::SeqCst),
										v.name
									)
								);
						*v = Variable {
							name: mono_fn_name.clone(),
							generics: HashMap::new(),
						};
					}
				}

				_ => {}
			}

			true
		};

		let post_trans = |e: &mut Expr| {
			let mut fns = fns.lock().unwrap();
			let mut monomorphized_fns = monomorphized_fns.lock().unwrap();
			match &mut e.val {
				LetNode(ref mut l) => {
					match &l.def.r#type {
						Type::Forall(generics, _) if !generics.is_empty() => {
							fns.insert_in_env(
								l.declared.assert_assignee().name.clone(),
								(*l.def.clone(), generics.clone())
							);
						}
						_ => {}
					}
				}

				BlockNode(ref mut b) => {
					let local_fn_declarations = fns.pop().unwrap();

					let mut monomorphized_defs = monomorphized_fns.iter_mut()
						.filter(|((generic_name, _), _)|
							local_fn_declarations.iter()
								.any(|(name, _)| name == generic_name)
						)
						.map(|((generic_name, instantiation), monomorphized_name)| {
							let (mut mono_fn, _) = local_fn_declarations[generic_name].clone();

							let substitutions = instantiation.clone().into_iter().collect();
							let trans_expr = annotate_helper(&substitutions, None, None);
							mono_fn.transform(trans_expr, |_| {});

							Expr {
								val:
									LetNode(Let {
										declared: Pattern::Assignee(Parameter {
											name: monomorphized_name.clone(),
											mutable: false,
										// These are zero values, as this variable doesn't
										// exist in the source code.
											name_loc: SourceLoc::nonexistent(),
											type_loc: None,
											r#type: mono_fn.r#type.clone(),
										}),
										def: box mono_fn
									}),
								r#type: Type::Void,
								loc: SourceLoc::nonexistent(),
							}
						})
						.collect::<VecDeque<_>>();
					monomorphized_defs.append(b);
					*b = monomorphized_defs;
				}

				_ => {},
			}
		};

		self.transform(pre_trans, post_trans);
	}
}