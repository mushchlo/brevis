mod anf;
mod defunctionalize;
mod desugar;
mod monomorphize;
mod optimize;

pub use crate::transform::{
	anf::anfify,
	desugar::desugar,
	optimize::optimize,
};