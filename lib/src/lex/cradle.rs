use std::iter::Peekable;
use std::str::Chars;

pub struct CharsPos<'a> {
	pub pos: SourcePos,
	pub source: Peekable<Chars<'a>>,
}

impl<'a> CharsPos<'a> {
	pub fn new(source: &'a str) -> Self {
		CharsPos {
			pos: SourcePos::new(),
			source: source.chars().peekable(),
		}
	}
}


#[derive(Debug, Copy, Clone, PartialEq, std::cmp::Eq, Hash)]
pub struct SourceLoc {
	pub start: SourcePos,
	pub end: usize,
}

impl SourceLoc {
	pub fn new(start: SourcePos, end: usize) -> Self {
		SourceLoc {
			start,
			end,
		}
	}

	// Used for expressions and statements created by the compiler, with no
	// real location in the source code.
	pub fn nonexistent() -> Self {
		SourceLoc {
			start: SourcePos::new(),
			end: 0,
		}
	}

	pub fn join(&self, other: SourceLoc) -> Self {
		SourceLoc {
			start: if self.start.index > other.start.index { other.start } else { self.start },
			end: if self.end > other.end { self.end } else { other.end },
		}
	}
}

#[derive(Debug, Copy, Clone, PartialEq, std::cmp::Eq, Hash)]
pub struct SourcePos {
	pub row: u32,
	pub col: u32,
	pub index: usize,
}

impl SourcePos {
	pub fn new() -> Self {
		SourcePos {
			row: 1,
			col: 1,
			index: 0,
		}
	}
}

impl std::fmt::Display for SourcePos {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, ":{}:{}", self.row, self.col)
	}
}

impl<'a> Iterator for CharsPos<'a> {
	type Item = (SourcePos, char);

	fn next(&mut self) -> Option<Self::Item> {
		match self.source.next() {
			None => None,
			Some(ch) => {
				self.pos.index += 1;
				if ch == '\n' {
					self.pos.row += 1;
					self.pos.col = 1;
				} else {
					self.pos.col += 1;
				}
				Some((self.pos, ch))
			}
		}
	}
}