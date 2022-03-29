mod op;
pub mod typeprint;
mod unify;

use std::iter::{self, FromIterator};
use std::collections::{HashMap, HashSet, BTreeSet};
use std::sync::{
	Arc,
	atomic::{AtomicUsize, Ordering},
};

pub use self::unify::{
	annotate_helper,
	Constraint,
	solve_constraints,
};

pub type TypeVarId = usize;

#[derive(Clone, Debug, PartialEq, std::cmp::Eq, Hash, PartialOrd, Ord)]
pub enum Type {
	Void,
	Int,
	Float,
	Str,
	Bool,

	// Union(Vec<Aggregate>),
	Struct(BTreeSet<AggregateType>),

	Pointer(Box<Type>, Mutability),
// The argument types, ending with the return type
	Func(Vec<Type>),

// A type generic over these arguments
	Forall(Vec<TypeVarId>, Box<Type>),

	Free(TypeVarId),
}

#[derive(Clone, Debug, PartialEq, std::cmp::Eq, Hash, PartialOrd, Ord)]
pub struct AggregateType {
	pub name: String,
	pub r#type: Type,
}

#[derive(Clone, Copy, Debug, PartialEq, std::cmp::Eq, Hash, PartialOrd, Ord)]
pub enum Mutability {
	Unknown(TypeVarId),
	Mutable,
	Immutable,
}

use self::Type::*;

lazy_static::lazy_static! {
	static ref TYPE_VAR_COUNTER: Arc<AtomicUsize> =
		Arc::new(AtomicUsize::new(0));
}

pub fn get_type_var_id() -> TypeVarId {
	TYPE_VAR_COUNTER.fetch_add(1, Ordering::SeqCst)
}

pub fn get_type_var() -> Type {
	Type::Free(get_type_var_id())
}

// Flattens a Forall-quantified type, returning a type with free
// typevars, and a hashmap of generic types to typevars.
pub fn instantiate(t: &Type, substitutions: &HashMap<TypeVarId, Type>) -> (Type, HashMap<TypeVarId, Type>) {
	let instantiation = generics_in_type(substitutions, t).into_iter()
		.zip(iter::repeat_with(get_type_var))
		.collect();
	(substitute(&instantiation, t), instantiation)
}

pub fn substitute(substitutions: &HashMap<TypeVarId, Type>, t: &Type) -> Type {
	match t {
		Void => Void,
		Int => Int,
		Float => Float,
		Str => Str,
		Bool => Bool,
		Pointer(box r, mutable) =>
			Pointer(box substitute(substitutions, r), *mutable),

		Free(v) if substitutions.contains_key(v) =>
			substitute(
				substitutions,
				substitutions.get(v)
					.unwrap_or_else(|| unreachable!())
			),

		Free(v) => Free(*v),
		Func(args_t) => Func(
			args_t.iter()
				.map(|t1| substitute(substitutions, t1))
				.collect::<Vec<Type>>(),
		),
		Struct(s) => Struct(
			s.iter().map(|a|
				AggregateType {
					name: a.name.clone(),
					r#type: substitute(substitutions, &a.r#type),
				}
			).collect()
		),
		Forall(type_vars, box generic_t) => {
			let new_generic_t = substitute(substitutions, generic_t);
			let free = free_in_type(substitutions, &new_generic_t);
			let new_type_vars = type_vars.iter().copied().filter(|tv| free.contains(tv)).collect::<Vec<_>>();

			if !new_type_vars.is_empty() {
				Forall(new_type_vars, box new_generic_t)
			} else {
				new_generic_t
			}
		}
	}
}

pub fn substitute_mutability(substitutions: &HashMap<TypeVarId, Mutability>, t: &Type) -> Type {
	match t {
		Void => Void,
		Int => Int,
		Float => Float,
		Str => Str,
		Bool => Bool,
		Pointer(box r, Mutability::Unknown(u)) if substitutions.contains_key(u) =>
			Pointer(box substitute_mutability(substitutions, r), substitutions[u]),
	// Referencing that doesn't use mutability and isn't otherwise constrained
	// is assumed to be immutable; we try to be conservative about allowing mutability.
		Pointer(box r, Mutability::Unknown(_)) =>
			Pointer(box substitute_mutability(substitutions, r), Mutability::Immutable),
		Pointer(box r, mutable) =>
			Pointer(box substitute_mutability(substitutions, r), *mutable),
		Free(v) => Free(*v),
		Func(args_t) => Func(
			args_t.iter()
				.map(|t1| substitute_mutability(substitutions, t1))
				.collect::<Vec<Type>>(),
		),
		Struct(s) => Struct(
			s.iter().map(|a|
				AggregateType {
					name: a.name.clone(),
					r#type: substitute_mutability(substitutions, &a.r#type),
				}
			).collect()
		),
		Forall(type_vars, box generic_t) =>
			Forall(type_vars.clone(), box substitute_mutability(substitutions, generic_t)),
	}
}

pub fn generalize(t: Type) -> Type {
	let free_vars = free_in_type(&HashMap::new(), &t);
	if !free_vars.is_empty() {
		Forall(free_vars.into_iter().collect(), box t)
	} else {
		t
	}
}

fn occurs_in(substitutions: &HashMap<TypeVarId, Type>, index: TypeVarId, t: &Type) -> bool {
	match t {
		Void | Int | Float | Str | Bool => false,

		Pointer(box p, _) =>
			occurs_in(substitutions, index, p),

		Free(i) if substitutions.contains_key(i) =>
			occurs_in(substitutions, index, &substitutions[i].clone()),

		Free(i) => *i == index,

		Struct(s) => s.iter().any(|a| occurs_in(substitutions, index, &a.r#type)),

		Func(args_t) =>
			args_t.iter().any(|t1| occurs_in(substitutions, index, t1)),

		Forall(_, box generic_t) =>
			occurs_in(substitutions, index, generic_t),
	}
}

pub fn free_in_type(substitutions: &HashMap<TypeVarId, Type>, t: &Type) -> HashSet<TypeVarId> {
	match t {
		Void | Int | Float | Str | Bool =>
			HashSet::new(),
		Free(i) if substitutions.contains_key(i) =>
			free_in_type(substitutions, &substitutions[i]),
		Free(i) =>
			HashSet::from([*i]),
		Pointer(box r, _) =>
			free_in_type(substitutions, r),
		Func(args_t) =>
			args_t.iter()
				.map(|arg| free_in_type(substitutions, arg))
				.reduce(|mut acc, next| {
					acc.extend(next.into_iter());
					acc
				})
				.unwrap_or_else(HashSet::new),
		Struct(s) =>
			s.iter()
				.map(|agg| free_in_type(substitutions, &agg.r#type))
				.reduce(|mut acc, next| {
					acc.extend(next.into_iter());
					acc
				})
				.unwrap_or_else(HashSet::new),
		Forall(_, box generic_t) =>
			free_in_type(substitutions, generic_t),
	}
}

pub fn generics_in_type(substitutions: &HashMap<TypeVarId, Type>, t: &Type) -> HashSet<TypeVarId> {
	match t {
		Void | Int | Float | Str | Bool =>
			HashSet::new(),
		Free(i) if substitutions.contains_key(i) =>
			generics_in_type(substitutions, &substitutions[i]),
		Free(_) =>
			HashSet::new(),
		Pointer(box r, _) =>
			generics_in_type(substitutions, r),
		Func(args_t) =>
			args_t.iter()
				.map(|arg| generics_in_type(substitutions, arg))
				.reduce(|mut acc, next| {
					acc.extend(next.into_iter());
					acc
				})
				.unwrap_or_else(HashSet::new),
		Struct(s) =>
			s.iter()
				.map(|agg| generics_in_type(substitutions, &agg.r#type))
				.reduce(|mut acc, next| {
					acc.extend(next.into_iter());
					acc
				})
				.unwrap_or_else(HashSet::new),
		Forall(type_vars, box generic_t) => {
			let mut ret = HashSet::from_iter(type_vars.iter().cloned());
			ret.extend(generics_in_type(substitutions, generic_t).into_iter());
			ret
		}
	}
}