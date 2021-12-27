use std::iter::Peekable;
use std::str::Chars;

pub struct CharsPos<'a> {
	pub pos: SourcePos,
	pub iter: Peekable<Chars<'a>>,
}

#[derive(Debug, Copy, Clone)]
pub struct SourcePos {
	pub row: u32,
	pub col: u32,
}

impl SourcePos {
	pub fn new() -> Self {
		SourcePos {
			row: 0,
			col: 0,
		}
	}
}

impl std::fmt::Display for SourcePos {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{}:{}", self.row, self.col)
	}
}

impl<'a> Iterator for CharsPos<'a> {
	type Item = (SourcePos, char);

	fn next(&mut self) -> Option<Self::Item> {
		match self.iter.next() {
			None => None,
			Some(ch) => {
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

pub trait CharsPosition {
	fn chars_pos(&self) -> CharsPos;
}

impl CharsPosition for str {
	fn chars_pos(&self) -> CharsPos<'_> {
		CharsPos {
			pos: SourcePos { row: 1, col: 1 },
			iter: self.chars().peekable(),
		}
	}
}
