mod anf;
mod monomorphize;
mod defunctionalize;
mod optimize;

pub use crate::transform::{
	anf::anfify,
	optimize::optimize,
};