use std::collections::{
	HashMap,
	VecDeque,
};
use std::sync::atomic::{AtomicUsize, Ordering};

use ast::{
	AST,
	Literal::*,
	Expr,
	ExprVal::*,
	Type,
	Let,
	Parameter,
	Variable,
};

use core::core_vals;


static MONOMORPHIZE_COUNTER: AtomicUsize = AtomicUsize::new(0);


// Monomorphization expects an already-inferred
// AST, and thus should be run after type inference.

// For every block in an AST, monomorphize() will place new declarations of monomorphized
// functions in context, and modify calls to refer to those monomorphized functions by their proper
// names.

impl AST {
	fn monomorphize(
		&mut self,
		fns: &mut Vec<HashMap<String, (Expr, Vec<u16>)>>,
		monomorphized_fns: &mut HashMap<(String, Vec<(u16, Type)>), String>
	) {
		match self {
			AST::LetNode(ref mut l) => {
				if let Some(ref mut e) = l.def {
					e.monomorphize(fns, monomorphized_fns);
					match &e.val {
						LambdaNode(lam) if !lam.generics.is_empty() => {
							fns.last_mut().unwrap().insert(
								l.var.name.clone(),
								(*e.clone(), lam.generics.clone())
							);
						// We're monomorphizing this function, no reason for its generic
						// version to exist!
							l.def = None;
							l.var.r#type = Type::Void;
						}
						_ => {}
					}
				}
			}
			AST::ExprNode(ref mut e) => {
				e.monomorphize(fns, monomorphized_fns)
			}
		}
	}
}

impl Expr {
	fn env_find<V: Clone + std::fmt::Debug>(env: &[HashMap<String, V>], fn_name: &str) -> V {
		for map in env {
			for (name, v) in map.iter() {
				if name == fn_name {
					return v.clone();
				}
			}
		}
		panic!("{} was not found in env {:#?}!", fn_name, env);
	}

	pub fn monomorphize(
		&mut self,
		fns: &mut Vec<HashMap<String, (Expr, Vec<u16>)>>,
		monomorphized_fns: &mut HashMap<(String, Vec<(u16, Type)>), String>
	) {
		match self.val {
			CallNode(ref mut c) => {
				for arg in &mut c.args {
					arg.monomorphize(fns, monomorphized_fns);
				}
				c.func.monomorphize(fns, monomorphized_fns);
			}

			VarNode(ref mut v) => {
				if !v.generics.is_empty() && !core_vals.contains_key(&v.name) {
					let instantiation =
						Expr::env_find(fns, &v.name).1
							.into_iter()
							.zip(v.generics.clone().into_iter())
							.collect::<HashMap<_, _>>();

					let mono_fn_name =
						monomorphized_fns
							.entry((v.name.clone(), instantiation.into_iter().collect()))
							.or_insert_with(||
								format!("monomorphized_{}__{}",
									MONOMORPHIZE_COUNTER.fetch_add(1, Ordering::SeqCst),
									v.name
								)
							);
					 self.val=
						VarNode(Variable {
							name: mono_fn_name.clone(),
							generics: vec![],
						});
				}
			}

			BlockNode(ref mut b) => {
				fns.push(HashMap::new());
				for line in b.iter_mut() {
					line.monomorphize(fns, monomorphized_fns);
				}
				*b = b.clone().into_iter()
					.filter(|line|
						match line {
							box AST::LetNode(l) =>
								!fns.last().unwrap().contains_key(&l.var.name),
							_ => true
						}
					)
					.collect::<VecDeque<_>>();

				let local_fn_declarations = fns.pop().unwrap();

				let mut monomorphized_defs = monomorphized_fns
					.iter_mut()
					.filter(|((generic_name, _), _)|
						local_fn_declarations.iter()
							.any(|(name, _)| name == generic_name)
					)
					.map(|((generic_name, instantiation), monomorphized_name)| {
						let (mut mono_fn, _) = local_fn_declarations[generic_name].clone();
						mono_fn.annotate_helper(&instantiation.clone().into_iter().collect(), true);

						box AST::LetNode(Let {
							var: Parameter {
								name: monomorphized_name.clone(),
								r#type: mono_fn.r#type.clone(),
							},
							def: Some(box mono_fn)
						})
					})
					.collect::<VecDeque<_>>();
				monomorphized_defs.append(b);
				*b = monomorphized_defs;
			}

			LambdaNode(ref mut l) => {
				l.body.monomorphize(fns, monomorphized_fns);
			}

			IfNode(ref mut ifelse) => {
				ifelse.cond.monomorphize(fns, monomorphized_fns);
				ifelse.then.monomorphize(fns, monomorphized_fns);
				if let Some(ref mut branch) = ifelse.r#else {
					branch.monomorphize(fns, monomorphized_fns);
				};
			}

			UnaryNode(ref mut u) => {
				u.expr.monomorphize(fns, monomorphized_fns);
			}

			BinaryNode(ref mut b) => {
				b.right.monomorphize(fns, monomorphized_fns);
				b.left.monomorphize(fns, monomorphized_fns);
			}

			LiteralNode(StructLiteral(ref mut s)) => {
				for agg in s {
					agg.val.monomorphize(fns, monomorphized_fns);
				}
			}

			LiteralNode(AtomicLiteral(_)) => {}
		}
	}
}