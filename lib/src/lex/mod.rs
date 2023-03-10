use std::collections::VecDeque;

pub mod cradle;
pub mod tok;

use crate::lex::cradle::{CharsPos, SourceLoc, SourcePos};

use crate::lex::tok::{
	Token,
	TokenValue,
	TokenValue::*,
	TokenLiteral,
	LiteralVal,
	KeyWord,
	OpID::*,
	UOpID::*,
	KEYWORD_DICT,
	BINARY_OP_DICT,
};


#[derive(Clone, Debug)]
pub struct TokenStream(pub VecDeque<Token>);

pub fn lex(s: &str) -> TokenStream {
	let mut src = CharsPos::new(s);
	let mut tokstrm: VecDeque<Token> = VecDeque::new();

	while let Some((_, c)) = src.skip_whitespace().next() {
		if c == '/' && *src.peek().unwrap().1 == '*' {
			src.skip_char('*');
			src.skip_multiline_comment();
		} else {
			tokstrm.push_back(match c {
				'"' => src.get_strlit(),
				'0'..='9' => src.get_numlit(c),
				_ => {
					if is_op_char(c) {
						src.get_op(c, tokstrm.back().map(|tok| &tok.val))
					} else if is_punc_char(c) {
						src.make_token(|_| TokenValue::Punc(c))
					} else {
						src.get_id(c)
					}
				}
			});
		}
	}

	TokenStream { 0: tokstrm }
}

impl CharsPos<'_> {
	pub fn peek(&mut self) -> Option<(SourcePos, &char)> {
		Some((self.pos, self.source.peek()?))
	}

	fn skip_char(&mut self, expected: char) {
		if let Some((_, c)) = self.next() {
			if c == expected {
				return;
			}
		}
		panic!("expected `{}` character at {}", expected, self.pos)
	}

	fn read_while<F>(&mut self, first_char: Option<char>, mut cond: F) -> (String, SourceLoc)
	where
		F: FnMut(&char) -> bool, // sometimes callers need to retain state between closure calls
	{
		let mut acc = match first_char {
			Some(c) => String::from(c),
			None => String::new()
		};
		let start_pos = self.peek()
			.map(|(pos, _)| pos)
			.unwrap_or_else(SourcePos::new);
		let mut loc = SourceLoc::new(start_pos, start_pos.index);
		loop {
			match self.peek() {
				Some((end_pos, c)) if cond(c) => {
					acc.push(*c);
					loc.end = end_pos.index;
					self.next();
				}

				_ => {
					break (acc, loc);
				}
			}
		}
	}

	fn read_lit<T, F>(&mut self, cond: F) -> T
	where
		T: std::str::FromStr,
		F: FnMut(&char) -> bool,
	{
		let begin = self.pos;
		self.read_while(None, cond).0.parse::<T>().unwrap_or_else(|_| {
			panic!(
				":{}:{} malformed {} literal",
				begin.row,
				begin.col,
				std::any::type_name::<T>()
			)
		})
	}

	fn get_strlit(&mut self) -> Token {
		let mut escaped = false;

		let strlit = self.make_token(|s| {
			let start_pos = s.peek().unwrap().0;
			let val = LiteralVal::Str(s.read_lit(|&ch| {
				let cont = ch != '"' || escaped;
				escaped = !escaped && ch == '\\';
				cont
			}));
			let end_pos = s.peek().unwrap().0;
			TokenValue::Literal(TokenLiteral {
				val,
				loc: SourceLoc::new(start_pos, end_pos.index),
			})
		});
		self.skip_char('"');
		strlit
	}

	fn get_numlit(&mut self, first_digit: char) -> Token {
		let mut is_flt = false;
		let mut last_e = false;
		let startpos = self.pos;
		let (to_num_lit, loc) = self.read_while(Some(first_digit), |&ch| {
			let cont = if is_flt_char(ch) || (last_e && ch == '-') {
				is_flt = true;
				true
			} else {
				ch.is_digit(10)
			};
			last_e = ch == 'e' || ch == 'E';
			cont
		});

		// because tokenizing either a floating literal or an integer literal requires
		// knowing which it is before converting, a call can't be easily made to make_token

		let num_lit = TokenValue::Literal(
			TokenLiteral {
				val: if is_flt {
					LiteralVal::Flt(to_num_lit.parse::<f64>().unwrap_or_else(|_| {
						panic!(
							":{}:{} malformed floating-point literal `{}`",
							startpos.row, startpos.col, to_num_lit
						)
					}))
				} else {
					LiteralVal::Int(to_num_lit.parse::<i64>().unwrap_or_else(|_| {
						panic!(
							":{}:{} malformed integer literal",
							startpos.row, startpos.col
						)
					}))
				},
				loc,
			}
		);

		Token {
			val: num_lit,
			loc: SourceLoc::new(startpos, self.pos.index),
		}
	}

	fn skip_multiline_comment(&mut self) {
		let mut comment_depth = 1;
		while comment_depth > 0 {
			match self.next() {
				None => panic!("unterminated comment"),
				Some((_, c)) => {
					comment_depth += match c {
						'/' if *self.peek().unwrap().1 == '*' => {
							self.skip_char('*');
							1
						},
						'*' if *self.peek().unwrap().1 == '/' => {
							self.skip_char('/');
							-1
						},
						_ => 0,
					}
				}
			}
		}
	}

	fn skip_whitespace(&mut self) -> &mut Self {
		while let Some((_, c)) = self.peek() {
			if !c.is_whitespace() {
				break;
			}
			self.next();
		}
		self
	}

	fn get_op(&mut self, first_char: char, context: Option<&TokenValue>) -> Token {
		self.make_token(|s|
			map_op(s.read_while(Some(first_char), |&c| is_op_char(c)).0, context)
		)
	}

	fn get_id(&mut self, first_char: char) -> Token {
		self.make_token(|s| {
			let (s, loc) = s.read_while(Some(first_char), |&c| !is_special_char(c));
			map_keyword(s, loc)
		})
	}

	fn make_token<F>(&mut self, mut f: F) -> Token
	where
		F: FnMut(&mut CharsPos) -> TokenValue,
	{
		let startpos = self.pos;
		let value = f(self);
		Token {
			val: value,
			loc: SourceLoc::new(startpos, self.pos.index),
		}
	}
}

fn map_keyword(kwstr: String, loc: SourceLoc) -> TokenValue {
	match kwstr.as_str() {
		"true" => Literal(TokenLiteral {
			val: LiteralVal::Bool(true),
			loc,
		}),
		"false" => Literal(TokenLiteral {
			val: LiteralVal::Bool(false),
			loc,
		}),
		"and" => BinaryOp(And),
		"or" => BinaryOp(Or),
		"xor" => BinaryOp(Xor),
		id => match match_dict(KEYWORD_DICT.iter().cloned(), id) {
			None => Ident(kwstr),
			Some(k) => KeyWord(k),
		},
	}
}

fn map_op(opstr: String, prev_op: Option<&TokenValue>) -> TokenValue {
	match opstr.as_str() {
		"!" => UnaryOp(Not),
		"-" if !matches!(prev_op, Some(Literal(_))) && !matches!(prev_op, Some(Ident(_))) => {
			UnaryOp(Neg)
		}
		"@" => UnaryOp(At),
		"&" => UnaryOp(Ref(false)),
		"=" => AssignOp(Eq),
		"->" => KeyWord(KeyWord::Arrow),
		"." => KeyWord(KeyWord::Dot),
		op => BinaryOp(
			match_dict(BINARY_OP_DICT.iter().cloned(), op)
				.unwrap_or_else(|| panic!("invalid operator `{}`", opstr)),
		),
	}
}

fn match_dict<'a, T, I>(dict: I, matcher: &str) -> Option<T>
where
	T: Clone,
	T: 'a,
	I: IntoIterator<Item = (T, &'a str)>,
{
	Some(dict.into_iter().find(|t| t.1 == matcher)?.0)
}

fn is_op_char(c: char) -> bool {
	".`|+-*/=!~<>%@&".contains(&c.to_string())
}

fn is_punc_char(c: char) -> bool {
	"',;:[]{}()".contains(&c.to_string())
}

fn is_special_char(c: char) -> bool {
	c.is_whitespace() || is_op_char(c) || is_punc_char(c)
}

// returns true if c is a character found in flt literals, but not int literals
// '-' symbol not included because if at the end of a flt literal, could just be infix
fn is_flt_char(c: char) -> bool {
	c == '.' || c == 'e' || c == 'E'
}
