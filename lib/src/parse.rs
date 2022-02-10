use ast::{
	*,
	Type::*,
	AST::*,
	ExprVal::*,
	Literal::*
};
use lex::TokenStream;
use cradle::{SourcePos, SourceLoc};
use tok::{
	Token,
	TokenLiteral::*,
	TokenValue,
	TokenValue::*,
	KeyWord,
	OpID,
	OpID::*,
	UOpID,
	UOpID::*,
};
use core::core_vals;

use lazy_static::lazy_static;
use std::collections::{HashMap, VecDeque};
use std::sync::{
	Arc,
	Mutex,
	atomic::{
		AtomicU16,
		Ordering,
	},
};

lazy_static! {
	static ref TYPE_DICT: Mutex<Vec<HashMap<String, Type>>> =
		Mutex::new(vec![HashMap::new()]);
	static ref TYPE_VAR_COUNTER: Arc<AtomicU16> =
		Arc::new(AtomicU16::new(0));

	static ref VAR_TYPES: Mutex<Vec<HashMap<String, Type>>> =
		Mutex::new(vec![
			core_vals.clone(),
			HashMap::new()
		]);
}

fn find_in(id: &str, dict: &[HashMap<String, Type>]) -> Option<Type> {
	for map in dict {
		if map.contains_key(id) {
			return map.get(id).cloned();
		}
	}

	None
}

fn insert_in(key: String, value: Type, dict: &mut Vec<HashMap<String, Type>>) {
	dict.last_mut().unwrap().insert(key, value);
}

fn new_stack_in(dict: &mut Vec<HashMap<String, Type>>) {
	dict.push(HashMap::new());
}

fn pop_stack_in(dict: &mut Vec<HashMap<String, Type>>) {
	dict.pop();
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
				"expected token {:?} at {:?}, found token {:?}",
				expected, t.loc, t.val
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
				core_vals.clone(),
				HashMap::new()
			];
		*TYPE_DICT.lock().unwrap() = vec![ HashMap::new() ];
		let mut parsed = VecDeque::<AST>::new();

		let mut end_loc = 0;
		while self.peek().is_some() {
			end_loc = self.peek().unwrap().loc.end;
			parsed.push_back(self.parse_node());
			self.skip_token(Punc(';'));
		}

		Expr {
			val: ExprVal::BlockNode(parsed),
			loc: SourceLoc::new(SourcePos::new(), end_loc),
			r#type: get_type_var(),
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
		new_stack_in(&mut *TYPE_DICT.lock().unwrap());
		let expr = self.parse_atom().expect_expr();
		let ret = self.maybe_call(|s| ExprNode(s.maybe_binary(expr.clone(), -1)))
			.expect_expr();
		pop_stack_in(&mut *TYPE_DICT.lock().unwrap());
		ret
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
		if let Some(op_tok) = self.peek() {
			match op_tok.val {
				BinaryOp(InfixFn) => {
					self.skip_token(BinaryOp(InfixFn));
				// TODO: MASSIVE BUG HERE. INFIX FNs CANNOT CONTAIN BINARY OP??? AWFUL IMPL
					let e = self.maybe_call(|s| s.parse_atom()).expect_expr();
					let f = new_expr(e.loc, e.val);
					self.skip_token(BinaryOp(InfixFn));
					let right_expr = self.parse_expr();
					let parsed = new_expr(
						left_expr.loc.join(right_expr.loc),
						CallNode(Call {
							func: box f,
							args: VecDeque::from([ left_expr, right_expr ]),
						})
					);

					return self.maybe_binary(parsed, -1);
				}

				BinaryOp(_) | AssignOp(_) => {
					// TODO: this is a mess, make it not so
					let (curr_prec, curr_op) = match op_tok.val {
						AssignOp(a) => (0, a),
						BinaryOp(b) => (precedence(b), b),
						_ => panic!("unreachable"),
					};

					if curr_prec > prev_prec {
						self.next();
						let right_expr =
							if curr_op == OpID::Member {
								self.maybe_call(|s|
									ExprNode(if let Some(operand_tok) = s.peek() {
										match &operand_tok.val {
											Ident(member) => {
												s.skip_token(Ident(member.clone()));
												new_expr(
													operand_tok.loc,
													VarNode(
														Variable {
															name: member.clone(),
															generics: vec![],
														}
													)
												)
											}

											non_id => panic!("At {:?}, the dot operator was used with a non-identifier RHS argument {:#?}.", op_tok.loc, non_id)
										}
									} else {
										panic!("Expected a RHS argument for the dot operator, found EOF");
									})
								).expect_expr()
							} else {
								let tmp = self.parse_atom().expect_expr();
								self.maybe_binary(tmp, curr_prec)
							};

						let parsed = new_expr(
							left_expr.loc.join(right_expr.loc),
							match right_expr.val.clone() {
								CallNode(c) if curr_op == Member => {
									CallNode(Call {
										args: c.args,
										func: box new_expr(
											SourceLoc::new(left_expr.loc.start, right_expr.loc.start.index),
											BinaryNode(Binary {
												left: box left_expr,
												right: c.func,
												op: curr_op,
												op_loc: op_tok.loc,
											})
										),
									})
								}
								_ =>
									BinaryNode(Binary {
										left: Box::new(left_expr),
										right: Box::new(right_expr),
										op: curr_op,
										op_loc: op_tok.loc,
									}),
							}
						);

						return self.maybe_binary(parsed, prev_prec);
					}
				}

				_ => {}
			}
		}

		left_expr
	}

	fn parse_atom(&mut self) -> AST {
		self.maybe_call(|s| {
			let loc = s.peek().unwrap().loc;
			match s.peek().unwrap().val {
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
						"unexpected punctuation {:?} at {:?}",
						p,
						s.peek().unwrap().loc
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
					new_expr_ast(loc, LiteralNode(AtomicLiteral(l)))
				}
				Ident(_) => {
					let id = s.parse_id();
					ExprNode(Expr {
						val: VarNode(
							Variable {
								name: id.clone(),
								generics: vec![],
							}
						),
						loc,
						r#type: find_in(&id, &*VAR_TYPES.lock().unwrap()).unwrap()
					})
				}
				UnaryOp(u) => s.parse_unary(u),
				_ => panic!("unexpected token {:?}", s.peek().unwrap()),
			}
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

	fn declare_var(&mut self) -> Parameter {
		if let Some(Token {
			val: Ident(id),
			loc: name_loc,
		}) = self.next()
		{
			if find_in(&id, &*VAR_TYPES.lock().unwrap()).is_some() {
				// making sure declared var does not already exist
				panic!(
					"variable {} was already declared, but was declared again at {:?}",
					id, name_loc
				);
			}
			let (var_type, type_loc) = match self.peek() {
				Some(t) if t.val == Punc(':') => {
					let mut type_loc = t.loc;
					self.skip_token(Punc(':'));
					let var_type = self.parse_type();
					type_loc.end = self.peek().unwrap().loc.start.index;
					(var_type, Some(type_loc))
				}
				_ => (get_type_var(), None)
			};
			let var = Parameter {
				name: id,
				r#type: var_type,
				name_loc,
				type_loc,
			};

			insert_in(var.name.clone(), var.r#type.clone(), &mut *VAR_TYPES.lock().unwrap());
			var
		} else {
			panic!("expected identifier for declared variable, found EOF")
		}
	}

	fn parse_id(&mut self) -> String {
		if let Some(Token {
			val: Ident(id),
			loc
		}) = self.next()
		{
			if find_in(&id, &*VAR_TYPES.lock().unwrap()).is_some() {
				return id;
			} else {
				panic!("identifier {} at {:?} was used before declaration", id, loc);
			}
		}
		panic!(
			"expected identifier of a declared variable at {:?}",
			self.peek().unwrap().loc
		);
	}

	fn parse_if(&mut self) -> AST {
		let begin_pos = self.peek().unwrap().loc.start;
		self.skip_token(KeyWord(KeyWord::If));
		let mut parsed = IfElse {
			cond: Box::new(self.parse_expr()),
			then: Box::new(self.parse_expr()),
			r#else: None,
		};

		let mut end_pos = parsed.then.loc.end;
		if let Some(Token {
			val: KeyWord(KeyWord::Else),
			loc: else_loc,
		}) = self.peek()
		{
			end_pos = else_loc.end;
			self.next();
			parsed.r#else = Some(Box::new(self.parse_expr()));
		}

		new_expr_ast(SourceLoc::new(begin_pos, end_pos), IfNode(parsed))
	}

	fn parse_block(&mut self) -> AST {
		new_stack_in(&mut *VAR_TYPES.lock().unwrap());
		let begin_pos = self.peek().unwrap().loc.start;
		let mut end_pos = self.peek().unwrap().loc.end;
		let parsed = self.delimited(Punc('{'), Punc('}'), Punc(';'), |s| {
			end_pos = s.peek().unwrap().loc.end;
			s.parse_node()
		});
		pop_stack_in(&mut *VAR_TYPES.lock().unwrap());

		match parsed.len() {
			0 => panic!("empty block :/"),
			1 => parsed.front().unwrap().clone(),
			_ => new_expr_ast(
				SourceLoc::new(begin_pos, end_pos),
				BlockNode(parsed)
			),
		}
	}

	fn parse_struct_literal(&mut self) -> AST {
		let begin_pos = self.peek().unwrap().loc.start;
		let mut end_pos = self.peek().unwrap().loc.end;
		let struct_args = self.delimited(Punc('['), Punc(']'), Punc(','), |s| {
			let possibly_name = s.next();

			let name = match possibly_name {
				Some(Token {
					val: Ident(name),
					loc
				}) => {
					end_pos = loc.end;
					name
				}

				Some(nonsense) => panic!("expected a name for member of structure literal at {:#?}, found {:#?}", nonsense.loc, nonsense),

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
			SourceLoc::new(begin_pos, end_pos),
			LiteralNode(
				StructLiteral(struct_args.into())
			)
		)
	}

	fn parse_lambda(&mut self) -> AST {
		let begin_pos = self.peek().unwrap().loc;
		self.skip_token(KeyWord(KeyWord::λ));
		new_stack_in(&mut *VAR_TYPES.lock().unwrap());

		let lambda_val = Lambda {
			args: self.delimited(Punc('('), Punc(')'), Punc(','), |s| s.declare_var()),
			generics: vec![],
			body: Box::new(self.parse_expr()),
		};
		pop_stack_in(&mut *VAR_TYPES.lock().unwrap());
		new_expr_ast(begin_pos.join(lambda_val.body.loc), LambdaNode(lambda_val))
	}

	fn parse_call(&mut self, f: Expr) -> AST {
		let begin_pos = self.peek().unwrap().loc.start;
		let call_val = Call {
			func: Box::new(f),
			args: self.delimited(Punc('('), Punc(')'), Punc(','), |s|
				s.parse_expr()
			),
		};
		new_expr_ast(
			SourceLoc::new(begin_pos, self.peek().unwrap().loc.start.index),
			CallNode(call_val)
		)
	}

	fn parse_unary(&mut self, op: UOpID) -> AST {
		let op_loc = self.peek().unwrap().loc;
		self.skip_token(UnaryOp(op));

	// TODO: Refactor so that this doesn't cancel some unary operators,
	// that should be left to the optimizer (which needs written)
		if let Some(tok) = self.peek() {
			match tok.val {
				Literal(l) => {
					self.skip_token(Literal(l.clone()));
					if op == Neg {
						return new_expr_ast(
							op_loc.join(tok.loc),
							LiteralNode(match l {
								IntLit(i) => AtomicLiteral(IntLit(-i)),
								FltLit(f) => AtomicLiteral(FltLit(-f)),
								_ => panic!(
									"Usage of unary minus operator `-` on a non-numeric literal at {:#?}",
									tok.loc
								),
							})
						);
					} else if op == Not {
						return new_expr_ast(
							op_loc.join(tok.loc),
							LiteralNode(match l {
								BoolLit(b) => AtomicLiteral(BoolLit(!b)),
								_ => panic!(
									"Usage of not operator `!` on a non-boolean value at {:#?}",
									tok.loc
								),
							})
						);
					}
				}
				_ => {
					let expr = box self.parse_atom().expect_expr();
					return new_expr_ast(
						op_loc.join(expr.loc),
						UnaryNode(Unary {
							op,
							op_loc,
							expr,
						})
					)
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
				}
				Punc('\'') => {
					let generic_name = match self.next() {
						Some(Token { val: Ident(id), .. }) =>
							format!("'{}", id),
						Some(non_id) =>
							panic!("expected an identifier for a generic type at {:#?}, found {:#?}", non_id.loc, non_id.val),
						None =>
							panic!("expected an identifier for a generic type, found EOF")
					};
					let type_var =
						if let Some(t) = find_in(&generic_name, &*TYPE_DICT.lock().unwrap()) {
							t
						} else {
							get_type_var()
						};
					insert_in(generic_name, type_var.clone(), &mut *TYPE_DICT.lock().unwrap());
					type_var
				}
				Ident(id) =>
					match TYPE_DICT.lock().unwrap().last().unwrap().get(&id) {
						Some(t) => t.clone(),
						_ => panic!("identifier {} does not represent a type, but was used in place of one", id),
					},

				Punc('[') => {
					self.0.push_front(Token {
						val: Punc('['),
						loc: SourceLoc::nonexistent(),
					}); // a dummy token to allow for the delimited() call below.
					Type::Struct(
						self.delimited(Punc('['), Punc(']'), Punc(','), |s| {
							let name =
								match s.next() {
									Some(Token { val: Ident(id), .. }) =>
										id,
									Some(not_a_name) =>
										panic!("At {:?}, non-identifier {:?} was used as a member for a structure type", not_a_name.loc, not_a_name.val),
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

				UnaryOp(Ref) =>
					Type::Pointer(box self.parse_type()),

				non_type => panic!("expected type at {:?}, received {:?}", tok.loc, non_type),
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
	Type::TypeVar(TYPE_VAR_COUNTER.fetch_add(1, Ordering::SeqCst))
}

fn new_expr_ast(loc: SourceLoc, value: ExprVal) -> AST {
	ExprNode(new_expr(loc, value))
}

fn new_expr(loc: SourceLoc, value: ExprVal) -> Expr {
	Expr {
		val: value,
		loc,
		r#type: get_type_var(),
	}
}
