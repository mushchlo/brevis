extern crate peeking_take_while;

use std::str::Chars;
use std::iter::Peekable;
use peeking_take_while::PeekableExt;


struct CharsPos<'a> {
	pos: SourcePos,
	iter: Peekable<Chars<'a>>
}

#[derive(Debug, Copy, Clone)]
struct SourcePos {
	row: u32,
	col: u32
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
			None		=>	None,
			Some(ch)	=> {
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

trait CharsPosition {
	fn chars_pos(&self) -> CharsPos;
}

impl CharsPosition for str {
	fn chars_pos(&self) -> CharsPos<'_> {
		CharsPos { pos: SourcePos { row: 1, col: 1 }, iter: self.chars().peekable() }
	}
}