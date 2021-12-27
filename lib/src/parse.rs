use ast::{
	*,
	Type::*,
	AST::*,
	ExprVal::*,
	Literal::*
};

use lex::TokenStream;

use cradle::SourcePos;

use tok::{
	Token,
	TokenLiteral::*,
	TokenValue,
	TokenValue::*,
	KeyWord,
	OpID,
	OpID::*,
};
use core::core_vals;

use lazy_static::lazy_static;
use std::collections::{HashMap, VecDeque};
use std::sync::Mutex;

lazy_static! {
	static ref TYPE_DICT: Mutex<HashMap<&'static str, Type>> = Mutex::new(HashMap::new());
	static ref TYPE_VAR_COUNTER: Mutex<u16> = Mutex::new(0);

	static ref VAR_TYPES: Mutex<Vec<HashMap<String, Type>>> =
		Mutex::new(vec![
			core_vals(),
			HashMap::new()
		]);
}

fn type_of(id: &str) -> Option<Type> {
	let var_types = VAR_TYPES.lock().unwrap().to_vec();
	for map in var_types {
		if map.contains_key(id) {
			return map.get(id).cloned();
		}
	}

	None
}

fn var_types_insert(key: String, value: Type) {
	let mut var_types = VAR_TYPES.lock().unwrap();
	var_types.last_mut().unwrap().insert(key, value);
}

fn var_types_new_stack() {
	let mut var_types = VAR_TYPES.lock().unwrap();
	var_types.push(HashMap::new());
}

fn var_types_pop_stack() {
	let mut var_types = VAR_TYPES.lock().unwrap();
	var_types.pop();
}

impl AST {
	fn expect_expr(self) -> Expr {
		match self {
			AST::ExprNode(e) => e,
			_ => panic!("an expression was expected, but a statement was found instead"),
		}
	}
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
			None => panic!("expected token {:?}, found EOF", expected),
			Some(t) if t.val != expected => panic!(
				"expected token {:?} at {}, found token {:?}",
				expected, t.start, t.val
			),
			_ => {}
		}
	}

	fn skip_token(&mut self, expected: TokenValue) {
		self.expect_token(expected);
		self.next();
	}

	pub fn parse(&mut self) -> Expr {
	    *VAR_TYPES.lock().unwrap() =
		    vec![
			    core_vals(),
			    HashMap::new()
		    ];
	    TYPE_DICT.lock().unwrap().clear();
		*TYPE_VAR_COUNTER.lock().unwrap() = 0;
		let mut parsed = VecDeque::<Box<AST>>::new();

		while self.peek().is_some() {
			parsed.push_back(Box::new(self.parse_node()));
			self.skip_token(Punc(';'));
		}

		Expr {
			val: ExprVal::BlockNode(parsed),
			r#type: Void,
		}
	}

	fn parse_node(&mut self) -> AST {
		let atom = self.parse_atom();
		self.maybe_call(|s| match atom.clone() {
			ExprNode(e) => ExprNode(s.maybe_binary(e, -1)),
			_ => atom.clone(),
		})
	}

	fn parse_expr(&mut self) -> Expr {
		let expr = self.parse_atom().expect_expr();
		self.maybe_call(|s| ExprNode(s.maybe_binary(expr.clone(), -1)))
			.expect_expr()
	}

	fn maybe_call<F>(&mut self, mut get_ast: F) -> AST
	where
		F: FnMut(&mut TokenStream) -> AST,
	{
		let a = get_ast(self);

		if let ExprNode(e) = a.clone() {
			if let Some(Token { val: Punc('('), .. }) = self.peek() {
				return self.parse_call(e);
			}
		}

		a
	}

	fn maybe_binary(&mut self, left_expr: Expr, prev_prec: i8) -> Expr {
		if let Some(tok) = self.peek() {
			match tok.val {
				BinaryOp(InfixFn) => {
					self.skip_token(BinaryOp(InfixFn));
				// TODO: MASSIVE BUG HERE. INFIX FNs CANNOT CONTAIN BINARY OP??? AWFUL IMPL
					let f = new_expr(self.maybe_call(|s| s.parse_atom()).expect_expr().val);
					self.skip_token(BinaryOp(InfixFn));
					let right_expr = self.parse_expr();
					let parsed = new_expr(CallNode(Call {
						func: box f,
						args: VecDeque::from([ left_expr, right_expr ]),
					}));

					return self.maybe_binary(parsed, -1);
				}

				BinaryOp(_) | AssignOp(_) => {
					// TODO: this is a mess, make it not so
					let (curr_prec, curr_op) = match tok.val {
						AssignOp(a) => (0, a),
						BinaryOp(b) => (precedence(b), b),
						_ => panic!("unreachable"),
					};

					if curr_prec > prev_prec {
						self.next();
						let right_expr =
							if tok.val == BinaryOp(Member) {
								if let Some(tok) = self.peek() {
									match tok.val {
										Ident(member) => {
											self.skip_token(Ident(member.clone()));
											new_expr(IdentNode(member))
										}

										_ => panic!("At {}, the dot operator was used with a non-identifier RHS argument.", tok.start)
									}
								} else {
									panic!("Expected a RHS argument for the dot operator, found EOF");
								}
							} else {
								let tmp = self.parse_atom().expect_expr();
								self.maybe_binary(tmp, curr_prec)
							};

						let parsed = new_expr(BinaryNode(Binary {
							left: Box::new(left_expr),
							right: Box::new(right_expr),
							op: curr_op,
						}));

						return self.maybe_binary(parsed, prev_prec);
					}
				}

				_ => {}
			}
		}

		left_expr
	}

	fn parse_atom(&mut self) -> AST {
		self.maybe_call(|s| match s.peek().unwrap().val {
			Punc(p) => match p {
				'(' => {
					s.next();
					let expr = s.parse_expr();
					s.skip_token(Punc(')'));
					ExprNode(expr)
				}
				'{' => s.parse_block(),
				'[' => s.parse_struct_literal(),
				_ => panic!(
					"unexpected punctuation {:?} at {}",
					p,
					s.peek().unwrap().start
				),
			},
			KeyWord(kw) => match kw {
				KeyWord::If => s.parse_if(),
				KeyWord::λ => s.parse_lambda(),
				KeyWord::Let => s.parse_let(),
				_ => panic!("unexpected keyword {:?}", kw),
			},
			Literal(l) => {
				s.skip_token(Literal(l.clone()));
				new_expr_ast(LiteralNode(AtomicLiteral(l)))
			}
			Ident(_) => {
				let id = s.parse_id();
				ExprNode(Expr {
					val: IdentNode(id.clone()),
					r#type: type_of(&id).unwrap()
				})
			}
			UnaryOp(u) => s.parse_unary(u),
			_ => panic!("unexpected token {:?}", s.peek().unwrap()),
		})
	}

	fn parse_let(&mut self) -> AST {
		self.skip_token(KeyWord(KeyWord::Let));
		let parsed_var = self.declare_var();
		let parsed_def = if let Some(Token {
			val: AssignOp(Eq),
			..
		}) = self.peek()
		{
			self.skip_token(AssignOp(Eq));
			Some(Box::new(self.parse_expr()))
		} else {
			None
		};

		LetNode(Let {
			var: parsed_var,
			def: parsed_def,
		})
	}

	fn delimited<F, R>(
		&mut self,
		begin: TokenValue,
		end: TokenValue,
		delim: TokenValue,
		mut parser: F,
	) -> VecDeque<R>
	where
		F: FnMut(&mut TokenStream) -> R,
	{
		let mut parsed = VecDeque::<R>::new();
		let mut first = true;

		self.skip_token(begin);
		while let Some(mut tok) = self.peek() {
			if !first && tok.val != end {
				self.skip_token(delim.clone());
				match self.peek() {
					Some(t) => tok = t,
					None => break,
				}
			} else if first {
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

	fn declare_var(&mut self) -> Variable {
		if let Some(Token {
			val: Ident(id),
			start: pos,
			..
		}) = self.next()
		{
			if type_of(&id).is_some() {
				// making sure declared var does not already exist
				panic!(
					"variable {} was already declared, but was declared again at {}",
					id, pos
				);
			}
			let var_type = match self.peek() {
				Some(Token { val: Punc(':'), .. }) => {
					self.skip_token(Punc(':'));
					self.parse_type()
				}
				_ => get_type_var(),
			};
			let var = Variable {
				name: id,
				r#type: var_type,
			};

			var_types_insert(var.name.clone(), var.r#type.clone());

			return var;
		}
		panic!("expected identifier for declared variable, found EOF");
	}

	fn parse_id(&mut self) -> String {
		if let Some(Token {
			val: Ident(id),
			start: pos,
			..
		}) = self.next()
		{
			if type_of(&id).is_some() {
				return id;
			} else {
				panic!("identifier {} at {} was used before declaration", id, pos);
			}
		}
		panic!(
			"expected identifier of a declared variable at {}",
			self.peek().unwrap().start
		);
	}

	fn parse_if(&mut self) -> AST {
		self.skip_token(KeyWord(KeyWord::If));
		let mut parsed = IfElse {
			cond: Box::new(self.parse_expr()),
			then: Box::new(self.parse_expr()),
			r#else: None,
		};

		if let Some(Token {
			val: KeyWord(KeyWord::Else),
			..
		}) = self.peek()
		{
			self.next();
			parsed.r#else = Some(Box::new(self.parse_expr()));
		}

		new_expr_ast(IfNode(parsed))
	}

	fn parse_block(&mut self) -> AST {
		var_types_new_stack();
		let parsed = self.delimited(Punc('{'), Punc('}'), Punc(';'), |s| {
			Box::new(s.parse_node())
		});
		var_types_pop_stack();

		match parsed.len() {
			0 => panic!("empty block :/"),
			1 => *parsed.front().unwrap().clone(),
			_ => new_expr_ast(BlockNode(parsed)),
		}
	}

	fn parse_struct_literal(&mut self) -> AST {
		let struct_args = self.delimited(Punc('['), Punc(']'), Punc(','), |s| {
			let possibly_name = s.next();

			let name = match possibly_name {
				Some(Token {
					val: Ident(name),
					..
				}) => name,

				Some(nonsense) => panic!("expected a name for member of structure literal at {}, found {:#?}", nonsense.start, nonsense),

				None => panic!("expected a name for member of structure literal, found EOF"),
			};
			s.skip_token(Punc(':'));
			let val = s.parse_expr();

			Aggregate {
				name,
				val
			}
		});

		new_expr_ast(
			LiteralNode(
				StructLiteral(struct_args.into())
			)
		)
	}

	fn parse_lambda(&mut self) -> AST {
		self.skip_token(KeyWord(KeyWord::λ));
		var_types_new_stack();
		let lambda_expr = new_expr_ast(LambdaNode(Lambda {
				args: self.delimited(Punc('('), Punc(')'), Punc(','), |s| s.declare_var()),
				body: Box::new(self.parse_expr()),
			}));
		var_types_pop_stack();
		lambda_expr
	}

	fn parse_call(&mut self, f: Expr) -> AST {
		new_expr_ast(CallNode(Call {
			func: Box::new(f),
			args: self.delimited(Punc('('), Punc(')'), Punc(','), |s| s.parse_expr()),
		}))
	}

	fn parse_unary(&mut self, op: OpID) -> AST {
		self.skip_token(UnaryOp(op));
		if let Some(tok) = self.peek() {
			match tok.val {
				UnaryOp(u) if u == op => {
					self.skip_token(UnaryOp(op));
					return self.parse_atom();
				}
				Literal(l) => {
					self.skip_token(Literal(l.clone()));
					if op == Minus {
						return new_expr_ast(LiteralNode(match l {
							IntLit(i) => AtomicLiteral(IntLit(-i)),
							FltLit(f) => AtomicLiteral(FltLit(-f)),
							_ => panic!(
								"Usage of unary minus operator `-` on a non-numeric value at {}",
								tok.start
							),
						}));
					} else if op == Not {
						return new_expr_ast(LiteralNode(match l {
							BoolLit(b) => AtomicLiteral(BoolLit(!b)),
							_ => panic!(
								"Usage of not operator `!` on a non-boolean value at {}",
								tok.start
							),
						}));
					}
				}
				_ => {
					return new_expr_ast(UnaryNode(Unary {
						op,
						expr: Box::new(self.parse_atom().expect_expr()),
					}))
				}
			}
		}
		panic!("Operator {:?} is not a unary", op);
	}

	fn parse_type(&mut self) -> Type {
		match self.next() {
			Some(tok) => match tok.val {
				KeyWord(kw) => match kw {
					KeyWord::Int => Type::Int,
					KeyWord::Float => Type::Float,
					KeyWord::String => Type::Str,
					KeyWord::λ => {
						let mut arg_types =
							self.delimited(Punc('('), Punc(')'), Punc(','), |s| s.parse_type());
						let mut return_types =
							self.delimited(Punc('('), Punc(')'), Punc(','), |s| s.parse_type());
						if return_types.is_empty() {
							return_types.push_front(Void);
						}
						Type::TypeConstructor(TConstructor {
							name: "Function".to_string(),
							args: {
								arg_types.append(&mut return_types);
								Vec::from(arg_types)
							},
						})
					}
					_ => panic!(
						"Keyword {:?} does not represent a type, but was used in place of one",
						kw
					),
				},
				Ident(id) =>
					match TYPE_DICT.lock().unwrap().get(&*id) {
						Some(t) => t.clone(),
						_ => panic!("identifier {} does not represent a type, but was used in place of one", id),
					},

				Punc('[') => {
					self.0.push_front(Token {
						val: Punc('['),
						start: SourcePos::new(),
						end: SourcePos::new(),
					}); // a dummy token to allow for the delimited() call below.
					Type::Struct(
						self.delimited(Punc('['), Punc(']'), Punc(','), |s| {
							let name =
								match s.next() {
									Some(Token { val: Ident(id), .. }) =>
										id,
									Some(not_a_name) =>
										panic!("At {}, non-identifier {:?} was used as a member for a structure type", not_a_name.start, not_a_name.val),
									None =>
										panic!("Expected an identifier as a member for a struct type, found EOF"),
								};
							s.skip_token(Punc(':'));
							let r#type = s.parse_type();
							AggregateType {
								name,
								r#type,
							}
						}).into()
					)
				}

				non_type => panic!("expected type, received {:?}", non_type),
			},

			None => panic!("expected a type, received EOF"),
		}
	}
}

fn precedence(id: OpID) -> i8 {
	match id {
		Eq => 0,

		Or => 1,
		Xor => 2,
		And => 3,

		Gt | Lt | Gteq | Lteq | Doeq | Noteq => 7,

		Add | Sub | Concat => 10,

		Mul | Div | Mod => 20,

		Member => 30,

		_ => panic!("operator {:?} has no precedence, but it was requested", id),
	}
}

pub fn get_type_var() -> Type {
	let type_var = *TYPE_VAR_COUNTER.lock().unwrap();
	*TYPE_VAR_COUNTER.lock().unwrap() += 1;
	Type::TypeVar(type_var)
}

fn new_expr_ast(value: ExprVal) -> AST {
	ExprNode(Expr {
		val: value,
		r#type: get_type_var(),
	})
}

fn new_expr(value: ExprVal) -> Expr {
	Expr {
		val: value,
		r#type: get_type_var(),
	}
}
