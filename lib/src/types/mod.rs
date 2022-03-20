mod op;
pub mod typeprint;
mod unify;

pub use self::unify::{
	annotate_helper,
	Constraint,
	solve_constraints,
	substitute,
	generalize,
	free_in_type,
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
	Struct(Vec<AggregateType>),

	Pointer(Box<Type>, Mutability),
// The argument types, ending with the return type
	Func(Vec<Type>),

// A type generic over these arguments
	Forall(Vec<TypeVarId>, Box<Type>),

	TypeVar(TypeVarId),
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