include!("ast.rs");

use crate::AST::*;
use crate::ExprVal::*;
use lazy_static::lazy_static;
use std::sync::Mutex;

lazy_static! {
	static ref TYPE_DICT: Mutex<Vec<(&'static str, Type)>> = Mutex::new(vec![]);
	static ref DECLARED_VARS: Mutex<Vec<(Variable, i16)>> = Mutex::new(vec![]);
	static ref TYPE_VAR_COUNTER: Mutex<u16> = Mutex::new(0);
}

impl TokenStream {
	fn peek(&self) -> Option<Token> {
		Some(self.0.front()?.clone())
	}

	fn next(&mut self) -> Option<Token> {
		self.0.pop_front()
	}

	fn expect_token(&self, expected: TokenValue) {
		match self.peek() {
			None	=> panic!("expected token {:?}, found EOF", expected),
			Some(t) if t.val != expected	=> panic!("expected token {:?} at {}, found token {:?}", expected, t.start, t.val),
			_		=> {}
		}
	}

	fn skip_token(&mut self, expected: TokenValue) {
		self.expect_token(expected.clone());
		self.next();
	}

	fn parse(&mut self) -> AST {
		let mut parsed = VecDeque::<Box<AST>>::new();

		while let Some(_) = self.peek() {
			parsed.push_back(Box::new(self.parse_expr()));
			self.skip_token(Punc(';'));
		}

		new_expr_ast(BlockNode(parsed))
	}

	fn parse_expr(&mut self) -> AST {
		let atom = self.parse_atom();

		let parsed = self.maybe_call(|s| s.maybe_binary(atom.clone(), -1));
//		println!("declared vars looks like this:\n{:#?}",
//					DECLARED_VARS.lock().unwrap());
		return parsed;
	}

	fn maybe_call<F>(&mut self, mut get_ast: F) -> AST
	where F: FnMut(&mut TokenStream) -> AST
	{
		let a = get_ast(self);

		if let Some(Token { val: Punc('('), .. }) = self.peek() {
			self.parse_call(a)
		} else {
			a
		}
	}

	fn maybe_binary(&mut self, left_ast: AST, prev_prec: i8) -> AST {
		if let Some(tok) = self.peek() {
			if let BinaryOp(_) | AssignOp(_) = tok.val {
				// TODO: this is a mess, make it not so
				let (curr_prec, curr_op) = match tok.val {
					AssignOp(a)	=> (0, a),
					BinaryOp(b)	=> (precedence(b), b),
					_			=> panic!("unreachable")
				};

				if curr_prec > prev_prec {
					self.next();
					let atom = self.parse_atom();
					let parsed = new_expr_ast(BinaryNode(Binary {
						left: Box::new(left_ast),
						right: Box::new(self.maybe_binary(atom, curr_prec)),
						op: curr_op
					}));

					return self.maybe_binary(parsed, prev_prec);
				}
			}
		}

		return left_ast;
	}

	fn parse_atom(&mut self) -> AST {
		self.maybe_call(|s|
			match s.peek().unwrap().val {
				Punc(p) =>
					match p {
						'('	=> {
							s.next();
							let expr_ast = s.parse_expr();
							s.skip_token(Punc(')'));
							expr_ast
						},
						'{'	=> s.parse_block(),
						_	=>	panic!("unexpected punctuation {:?} at {}", p, s.peek().unwrap().start)
					},
				KeyWord(kw)	=>
					match kw {
						KeyWord::If		=> s.parse_if(),
						KeyWord::λ		=> s.parse_lambda(),
						KeyWord::Let	=> s.parse_let(),
						_	=> panic!("unexpected keyword {:?}", kw)
					},
				Literal(l)	=> {
									s.skip_token(Literal(l.clone()));
									new_expr_ast(LiteralNode(l))
								},
				Ident(_)	=> new_expr_ast(IdentNode(s.parse_var())),
				UnaryOp(u)	=> s.parse_unary(u),
				_ => panic!("unexpected token {:?}", s.peek().unwrap())
			})
	}

	fn parse_let(&mut self) -> AST {
		self.skip_token(KeyWord(KeyWord::Let));
		let parsed_var = self.declare_var(None);
		let parsed_def = if let Some(Token { val: AssignOp(Eq), .. }) = self.peek() {
			self.skip_token(AssignOp(Eq));
			Some(Box::new(self.parse_expr()))
		} else {
			None
		};
		DECLARED_VARS.lock().unwrap().push((parsed_var.clone(), 0));
		return LetNode(Let {
			var: parsed_var,
			def: parsed_def
		});
	}

	fn delimited<F, R>(&mut self, begin: TokenValue, end: TokenValue, delim: TokenValue, mut parser: F) -> VecDeque<R>
	where F: FnMut(&mut TokenStream) -> R
	{
		let mut parsed = VecDeque::<R>::new();
		let mut first = true;

		self.skip_token(begin);
		while let Some(mut tok) = self.peek() {
			if !first && tok.val != end {
				self.skip_token(delim.clone());
				match self.peek() {
					Some(t) => tok = t,
					None	=> break
				}
			} else {
				first = false;
			}

			if tok.val == end {
				break;
			}

			parsed.push_back(parser(self));
		}
		self.skip_token(end);

		parsed
	}

	fn declare_var(&mut self, initial_scope: Option<i16>) -> Variable {
		if let Some(Token { val: Ident(id), start: pos, .. }) = self.next() {
			if matches!(type_of(&id), Some(_)) {		// making sure declared var does not already exist
				panic!("variable {} was already declared, but was declared again at {}", id, pos);
			}
			let var_type = match self.peek() {
				Some(Token { val: Punc(':'), .. }) => {
					self.skip_token(Punc(':'));
					if let Some(Token { val: t, .. }) = self.next() {
						match_type(t)
					} else {
						panic!("expected a type at {}, found EOF", pos);
					}
				},
				_	=> get_type_var()
			};
			let var = Variable { name: id, r#type: var_type };

			if let Some(scope) = initial_scope {
				DECLARED_VARS.lock().unwrap().push((var.clone(), scope));
			}

			return var
		}
		panic!("expected identifier for declared variable, found EOF");
	}

	fn parse_var(&mut self) -> Variable {
		if let Some(Token { val: Ident(id), start: pos, .. }) = self.next() {
			if let Some(t) = type_of(&id) {
				return Variable { name: id, r#type: t };
			}
			panic!("identifier {} at {} was used before declaration", id, pos);
		}
		panic!("expected identifier of a declared variable at {}", self.peek().unwrap().start);
	}
/*
	fn parse_ident(&mut self) -> Variable {
		match self.next() {
			Some(Token { val: Ident(id), .. }) => {
					let t = 
					Variable { name: id, r#type: Type::TypeVar(get_type_var()) },
			Some(tok)	=> panic!("Expected an identifier at {}, found token {:?}", tok.start, tok.val),
			_		=> panic!("Expected an identifier, found EOF")
		}
	}*/

	fn parse_if(&mut self) -> AST {
		self.skip_token(KeyWord(KeyWord::If));
		let mut parsed = IfElse {
			cond: Box::new(self.parse_expr()),
			then: Box::new(self.parse_expr()),
			r#else: None
		};

		if let Some(Token { val: KeyWord(KeyWord::Else), .. }) = self.peek() {
			self.next();
			parsed.r#else = Some(Box::new(self.parse_expr()));
		}

		new_expr_ast(IfNode(parsed))
	}

	fn parse_block(&mut self) -> AST {
		for (_, d) in DECLARED_VARS.lock().unwrap().iter_mut() {
			*d += 1;
		}
		let parsed = self.delimited(Punc('{'), Punc('}'), Punc(';'),
							|s| Box::new(s.parse_expr()));
		for (_, d) in DECLARED_VARS.lock().unwrap().iter_mut() {
			*d -= 1;
		}
		filter_if(&mut DECLARED_VARS.lock().unwrap(), |(_, d)| *d < 0);

		match parsed.len() {
			0 => panic!("empty block :/"),
			1 => *parsed.front().unwrap().clone(),
			_ => new_expr_ast(BlockNode(parsed))
		}
	}

	fn parse_lambda(&mut self) -> AST {
		self.skip_token(KeyWord(KeyWord::λ));
		new_expr_ast(LambdaNode(Lambda {
			args: self.delimited(Punc('('), Punc(')'), Punc(','), |s| s.declare_var(Some(-1))),
			body: Box::new(self.parse_expr())
		}))
	}

	fn parse_call(&mut self, f: AST) -> AST {
		new_expr_ast(CallNode(Call {
			func: Box::new(f),
			args: self.delimited(Punc('('), Punc(')'), Punc(','), |s| s.parse_expr())
		}))
	}

	fn parse_unary(&mut self, op: OpID) -> AST {
		self.skip_token(UnaryOp(op));
		if let Some(tok) = self.peek() {
			match tok.val {
				UnaryOp(u) if u == op => {
					self.skip_token(UnaryOp(op));
					return self.parse_atom();
				},
				Literal(l) => {
					self.skip_token(Literal(l.clone()));
					if op == Minus {
						return new_expr_ast(LiteralNode(match l {
							IntLit(i)	=> IntLit(-i),
							FltLit(f)	=> FltLit(-f),
							_			=> panic!("Usage of unary minus operator `-` on a non-numberic value at {}", tok.start)
						}));
					} else if op == Not {
						return new_expr_ast(LiteralNode(match l {
							BoolLit(b)	=> BoolLit(!b),
							_			=> panic!("Usage of not operator `!` on a non-boolean value at {}", tok.start)
						}));
					}
				},
				_ => return new_expr_ast(UnaryNode(Unary {
						op: op,
						expr: Box::new(self.parse_atom())
					}))
			}
		}
		panic!("Operator {:?} is not a unary", op);
	}
}

fn precedence(id: OpID) -> i8 {
	match id {
		Eq => 0,

		Or => 1,
		Xor => 2,
		And => 3,

		Gt | Lt | Gteq |
			Lteq | Doeq | Noteq => 7,

		Add | Sub => 10,

		Mul | Div => 20,

		_ => panic!("operator {:?} has no precedence, but it was requested", id)
	}
}

fn match_type(tok: TokenValue) -> Type {
	match tok {
		KeyWord(kw)	=>
			match kw {
				KeyWord::Int		=> Type::Int,
				KeyWord::Float		=> Type::Float,
				KeyWord::String		=> Type::Str,
				_		=> panic!("Keyword {:?} does not represent a type, but was used in place of one", kw)
			},
		Ident(id)	=>
			match TYPE_DICT.lock().unwrap()
							.iter()
							.find(|d| d.0 == id) {
								Some((_, t)) => t.clone(),
								_	=> panic!("identifier {} does not represent a type, but was used in place of one", id)
			},
		_	=> panic!("expected type, received {:?}", tok)
	}
}

fn get_type_var() -> Type {
	let type_var = *TYPE_VAR_COUNTER.lock().unwrap();
	*TYPE_VAR_COUNTER.lock().unwrap() += 1;
	return Type::TypeVar(type_var)
}

fn type_of(id: &str) -> Option<Type> {
	return Some(DECLARED_VARS.lock().unwrap()
					.iter()
					.find(|(Variable { name: n, .. }, _)| *n == id)?
					.0.r#type
					.clone());
}

fn filter_if<T, F>(v: &mut Vec<T>, pred: F)
	where F: Fn(&T) -> bool
{
	let mut i = 0;
	while i < v.len() {
		if pred(&v[i]) {
			v.remove(i);
		} else {
			i += 1
		}
	}
}

fn new_expr_ast(value: ExprVal) -> AST {
	ExprNode(Expr {
		val: value,
		r#type: get_type_var()
	})
}