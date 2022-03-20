pub mod ast;

use crate::{
	util::Env,
	parse::ast::{
		Expr,
		ExprVal,
		Variable,
		Literal::*,
		Pattern,
	},
	types::{
		Type,
		Type::*,
		TypeVarId,
		AggregateType,
		Mutability
	},
	lex::{
		TokenStream,
		cradle::{SourcePos, SourceLoc},
		tok::{
			Token,
			TokenValue,
			TokenValue::*,
			KeyWord,
			OpID,
			OpID::*,
			UOpID,
			UOpID::*,
		},
	},
	core::core_vals,
};

use lazy_static::lazy_static;
use std::collections::{HashMap, VecDeque, HashSet};
use std::sync::{
	Arc,
	Mutex,
	atomic::{
		AtomicUsize,
		Ordering,
	},
};

lazy_static! {
	static ref TYPE_DICT: Mutex<Vec<HashMap<String, Type>>> =
		Mutex::new(vec![HashMap::new()]);
	static ref TYPE_VAR_COUNTER: Arc<AtomicUsize> =
		Arc::new(AtomicUsize::new(0));

	static ref VAR_ENV: Mutex<Vec<HashMap<String, SourceLoc>>> =
		Mutex::new(vec![
			core_vals.iter()
				.map(|(name, _)| (name.clone(), SourceLoc::nonexistent()))
				.collect(),
			HashMap::new()
		]);
}

macro_rules! var_env {
	() => {
		*VAR_ENV.lock().unwrap()
	}
}

macro_rules! type_dict {
	() => {
		*TYPE_DICT.lock().unwrap()
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
				expected, t.loc.start, t.val
			),
			_ => {}
		}
	}

	fn skip_token(&mut self, expected: TokenValue) {
		self.expect_token(expected);
		self.next();
	}

	pub fn parse(&mut self) -> Expr {
		var_env!() =
			vec![
				core_vals.iter()
					.map(|(name, _)| (name.clone(), SourceLoc::nonexistent()))
					.collect(),
				HashMap::new()
			];
		type_dict!() = vec![ HashMap::new() ];
		let mut parsed = VecDeque::<Expr>::new();

		let mut end_loc = 0;
		while self.peek().is_some() {
			end_loc = self.peek().unwrap().loc.end;
			parsed.push_back(self.parse_node());
			self.skip_token(Punc(';'));
		}

		Expr {
			val: ExprVal::Block(parsed),
			loc: SourceLoc::new(SourcePos::new(), end_loc),
			r#type: get_type_var(),
		}
	}

	fn parse_node(&mut self) -> Expr {
		let atom = self.parse_atom();
		self.maybe_call(|s| match &atom.val {
			ExprVal::Let(_) => atom.clone(),
			_ => s.maybe_binary(atom.clone(), -1),
		})
	}

	fn parse_expr(&mut self) -> Expr {
		type_dict!().new_stack();
		let expr = self.parse_atom();
		let ret = self.maybe_call(|s| s.maybe_binary(expr.clone(), -1));
		type_dict!().pop_stack();
		ret
	}

	fn maybe_call<F>(&mut self, mut get_expr: F) -> Expr
	where
		F: FnMut(&mut TokenStream) -> Expr,
	{
		let e = get_expr(self);

		if let Some(Token { val: Punc('('), .. }) = self.peek() {
			self.parse_call(e)
		} else {
			e
		}
	}

	fn maybe_binary(&mut self, left_expr: Expr, prev_prec: i8) -> Expr {
		use self::ExprVal::Binary;

		if let Some(op_tok) = self.peek() {
			match op_tok.val {
				BinaryOp(InfixFn) => {
					self.skip_token(BinaryOp(InfixFn));
				// TODO: MASSIVE BUG HERE. INFIX FNs CANNOT CONTAIN BINARY OP??? AWFUL IMPL
					let e = self.parse_atom();
					let f = new_expr(e.loc, e.val);
					self.skip_token(BinaryOp(InfixFn));
					let right_expr = self.parse_expr();
					let parsed = new_expr(
						left_expr.loc.join(right_expr.loc),
						ExprVal::Call(ast::Call {
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
									if let Some(operand_tok) = s.peek() {
										match &operand_tok.val {
											Ident(member) => {
												s.skip_token(Ident(member.clone()));
												new_expr(
													operand_tok.loc,
													ExprVal::Var(ast::Variable {
														name: member.clone(),
														declaration_loc: SourceLoc::nonexistent(),
														generics: HashMap::new(),
													})
												)
											}

											non_id => panic!("At {}, the dot operator was used with a non-identifier RHS argument {:#?}.", op_tok.loc.start, non_id)
										}
									} else {
										panic!("Expected a RHS argument for the dot operator, found EOF");
									}
								)
							} else {
								let tmp = self.parse_atom();
								self.maybe_binary(tmp, curr_prec)
							};

						let parsed = new_expr(
							left_expr.loc.join(right_expr.loc),
							match &right_expr.val {
							// TODO: This is a hack to fix call parsing being messed up in the case
							// of a member access. Fix this on a higher level.
								ExprVal::Call(c) if curr_op == Member => {
									ExprVal::Call(ast::Call {
										args: c.args.clone(),
										func: box new_expr(
											SourceLoc::new(left_expr.loc.start, right_expr.loc.start.index),
											Binary(ast::Binary {
												left: box left_expr,
												right: c.func.clone(),
												op: curr_op,
												op_loc: op_tok.loc,
											})
										),
									})
								}
								_ =>
									Binary(ast::Binary {
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

	fn parse_atom(&mut self) -> Expr {
		self.maybe_call(|s| {
			let loc = s.peek().unwrap().loc;
			match s.peek().unwrap().val {
				Punc(p) => match p {
					'(' => {
						s.next();
						let expr = s.parse_expr();
						s.skip_token(Punc(')'));
						expr
					}
					'{' => s.parse_block(),
					'[' => s.parse_struct_literal(),
					_ => panic!(
						"unexpected punctuation {:?} at {}",
						p,
						s.peek().unwrap().loc.start
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
					new_expr(loc, ExprVal::Literal(AtomicLiteral(l)))
				}
				Ident(_) => {
					let r#type = get_type_var();
					let (name, declaration_loc) = s.parse_id();
					Expr {
						val: ExprVal::Var(Variable {
							name,
							declaration_loc,
							generics: HashMap::new(),
						}),
						loc,
						r#type,
					}
				}
				UnaryOp(u) => s.parse_unary(u),
				_ => panic!("unexpected token {:?}", s.peek().unwrap()),
			}
		})
	}

	fn parse_let(&mut self) -> Expr {
		var_env!().new_stack();
		let start_loc = self.peek().unwrap().loc;
		self.skip_token(KeyWord(KeyWord::Let));
		let declared = self.parse_pattern();

		self.skip_token(AssignOp(Eq));
		let def = box self.parse_expr();
		var_env!().pop_stack();
		for assignee in declared.assignees() {
			self.declare(assignee);
		}
		let end_loc = def.loc;

		Expr {
			val:
				ExprVal::Let(ast::Let {
					declared,
					def,
				}),
			r#type: Type::Void,
			loc: start_loc.join(end_loc),
		}
	}

	// Returns the parsed structure, along with the location it was in.
	fn delimited<F, R>(
		&mut self,
		begin: TokenValue,
		end: TokenValue,
		delim: TokenValue,
		mut parser: F,
	) -> (VecDeque<R>, SourceLoc)
	where
		F: FnMut(&mut TokenStream) -> R,
	{
		let mut parsed = VecDeque::<R>::new();
		let mut first = true;
		let mut loc = SourceLoc::new(self.peek().unwrap().loc.start, self.peek().unwrap().loc.end);

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
		loc.end = self.peek().unwrap().loc.end;
		self.skip_token(end);

		(parsed, loc)
	}

	fn parse_parameter(&mut self) -> ast::Parameter {
		let mutable = match self.peek() {
			Some(t) if t.val == KeyWord(KeyWord::Mut) => {
				self.skip_token(KeyWord(KeyWord::Mut));
				true
			}
			_ => false,
		};
		if let Some(Token {
			val: Ident(name),
			loc: name_loc,
		}) = self.next()
		{
			if var_env!().find(&name).is_some() {
				// making sure declared var does not already exist
				panic!(
					"variable {} was already declared, but was declared again at {}",
					name, name_loc.start
				);
			}
			let (r#type, type_loc) = match self.peek() {
				Some(t) if t.val == Punc(':') => {
					let mut type_loc = t.loc;
					self.skip_token(Punc(':'));
					let var_type = self.parse_type();
					type_loc.end = self.peek().unwrap().loc.start.index;
					(var_type, Some(type_loc))
				}
				_ => (get_type_var(), None)
			};
			ast::Parameter {
				name,
				r#type,
				mutable,
				name_loc,
				type_loc,
			}
		} else {
			panic!("expected identifier for declared variable, found EOF")
		}
	}

	fn declare(&mut self, declared: &ast::Parameter) {
		var_env!().insert_in_env(declared.name.clone(), declared.name_loc);
	}

	fn declare_var(&mut self) -> ast::Parameter {
		let declared = self.parse_parameter();
		self.declare(&declared);
		declared
	}

	fn parse_id(&mut self) -> (String, SourceLoc) {
		if let Some(Token {
			val: Ident(id),
			loc
		}) = self.next()
		{
			if let Some(origin) = var_env!().find(&id) {
				(id, *origin)
			} else {
				panic!("identifier {} at {} was used before declaration", id, loc.start);
			}
		} else {
			panic!(
				"expected identifier of a declared variable at {}",
				self.peek().unwrap().loc.start
			)
		}
	}

	fn parse_if(&mut self) -> Expr {
		let begin_pos = self.peek().unwrap().loc.start;
		self.skip_token(KeyWord(KeyWord::If));
		let mut parsed = ast::IfElse {
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

		new_expr(
			SourceLoc::new(begin_pos, end_pos),
			ExprVal::If(parsed)
		)
	}

	fn parse_block(&mut self) -> Expr {
		var_env!().new_stack();
		let (parsed, block_loc) = self.delimited(Punc('{'), Punc('}'), Punc(';'), |s|
			s.parse_node()
		);
		var_env!().pop_stack();

		match parsed.len() {
			0 => panic!("empty block :/"),
			1 => parsed.front().unwrap().clone(),
			_ => new_expr(
				block_loc,
				ExprVal::Block(parsed)
			),
		}
	}

	fn parse_struct_literal(&mut self) -> Expr {
		let (struct_args, struct_loc) = self.delimited(Punc('['), Punc(']'), Punc(','), |s| {
			let possibly_name = s.next();

			let name = match possibly_name {
				Some(Token { val: Ident(name), .. }) => name,

				Some(nonsense) => panic!("expected a name for member of structure literal at {}, found {:#?}", nonsense.loc.start, nonsense),

				None => panic!("expected a name for member of structure literal, found EOF"),
			};
			s.skip_token(Punc(':'));
			let val = s.parse_expr();

			ast::Aggregate {
				name,
				val
			}
		});

		new_expr(
			struct_loc,
			ExprVal::Literal(
				StructLiteral(struct_args.into())
			)
		)
	}

	fn parse_lambda(&mut self) -> Expr {
		let begin_loc = self.peek().unwrap().loc;
		self.skip_token(KeyWord(KeyWord::λ));
		var_env!().new_stack();

		let (args, _) = self.delimited(Punc('('), Punc(')'), Punc(','), |s| s.declare_var());
		let return_type =
			if let Some(Token {
				val: TokenValue::KeyWord(KeyWord::Arrow),
				..
			}) = self.peek() {
				self.skip_token(TokenValue::KeyWord(KeyWord::Arrow));
				self.parse_type()
			} else {
				get_type_var()
			};
		let fn_type = Func(
			args.iter()
				.map(|_| get_type_var())
				.chain(std::iter::once(return_type))
				.collect()
		);
		let lambda_val = ast::Lambda {
			args,
			captured: HashSet::new(),
			body: Box::new(self.parse_expr()),
		};

		var_env!().pop_stack();

		Expr {
			loc: begin_loc.join(lambda_val.body.loc),
			val: ExprVal::Lambda(lambda_val),
			r#type: fn_type,
		}
	}

	fn parse_call(&mut self, f: Expr) -> Expr {
		let (args, args_loc) = self.delimited(
			Punc('('),
			Punc(')'),
			Punc(','),
			|s| s.parse_expr()
		);
		let loc = f.loc.join(args_loc);
		let call_val = ast::Call {
			func: Box::new(f),
			args,
		};
		new_expr(
			loc,
			ExprVal::Call(call_val)
		)
	}

	fn parse_unary(&mut self, mut op: UOpID) -> Expr {
		let op_loc = self.peek().unwrap().loc;
		self.skip_token(UnaryOp(op));

		if let Ref(mutable) = &mut op {
			*mutable = match self.peek().map(|t| t.val) {
				Some(KeyWord(KeyWord::Mut)) => {
					self.next();
					true
				}
				_ => false,
			};
		}

		let expr = box self.parse_atom();
		new_expr(
			op_loc.join(expr.loc),
			ExprVal::Unary(ast::Unary {
				op,
				op_loc,
				expr,
			})
		)
	}

	fn parse_type(&mut self) -> Type {
		match self.next() {
			Some(tok) => match tok.val {
				KeyWord(kw) => match kw {
					KeyWord::Void => Type::Void,
					KeyWord::Bool => Type::Bool,
					KeyWord::Int => Type::Int,
					KeyWord::Float => Type::Float,
					KeyWord::String => Type::Str,
					KeyWord::λ => {
						let (mut arg_types, _) =
							self.delimited(Punc('('), Punc(')'), Punc(','), |s| s.parse_type());
						let ret_type =
							if let Some(Token {
								val: TokenValue::KeyWord(KeyWord::Arrow),
								..
							}) = self.peek() {
								self.skip_token(TokenValue::KeyWord(KeyWord::Arrow));
								self.parse_type()
							} else {
								Type::Void
							};
						arg_types.push_back(ret_type);
						Type::Func(Vec::from(arg_types))
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
							panic!("expected an identifier for a generic type at {}, found {:#?}", non_id.loc.start, non_id.val),
						None =>
							panic!("expected an identifier for a generic type, found EOF")
					};
					let type_var = type_dict!()
						.find(&generic_name)
						.cloned()
						.unwrap_or_else(get_type_var);
					type_dict!().insert_in_env(generic_name, type_var.clone());
					type_var
				}
				Ident(id) =>
					match type_dict!().last().unwrap().get(&id) {
						Some(t) => t.clone(),
						_ => panic!("identifier {} does not represent a type, but was used in place of one", id),
					},

				Punc('[') => {
					self.0.push_front(Token {
						val: Punc('['),
						loc: SourceLoc::nonexistent(),
					}); // a dummy token to allow for the delimited() call below.
					let (struct_args, _) =
						self.delimited(Punc('['), Punc(']'), Punc(','), |s| {
							let name =
								match s.next() {
									Some(Token { val: Ident(id), .. }) =>
										id,
									Some(not_a_name) =>
										panic!("At {}, non-identifier {:?} was used as a member for a structure type", not_a_name.loc.start, not_a_name.val),
									None =>
										panic!("Expected an identifier as a member for a struct type, found EOF"),
								};
							s.skip_token(Punc(':'));
							let r#type = s.parse_type();
							AggregateType {
								name,
								r#type,
							}
						});
					Struct(struct_args.into())
				}

				UnaryOp(Ref(_)) => {
					let mutable = match self.peek().map(|t| t.val) {
						Some(KeyWord(KeyWord::Mut)) => {
							self.next();
							Mutability::Mutable
						}
						_ => Mutability::Immutable,
					};
					Type::Pointer(box self.parse_type(), mutable)
				}

				non_type => panic!("expected type at {}, received {:?}", tok.loc.start, non_type),
			},

			None => panic!("expected a type, received EOF"),
		}
	}

	pub fn parse_pattern(&mut self) -> Pattern {
		let tok = self.peek().unwrap_or_else(|| panic!("Expected pattern, found EOF"));
		match tok.val {
		// Empty pattern
			Ident(s) if &s == "_" => {
				self.next();
				Pattern::Empty(tok.loc)
			},

		// Literal pattern
			Literal(lit) => {
				self.next();
				Pattern::Literal(lit)
			}

		// Either variable match or function pattern
			Ident(_) | KeyWord(KeyWord::Mut) => {
				let var = self.declare_var();
				if self.peek().map(|t| t.val) == Some(Punc('(')) {
					let args = Vec::from(
						self.delimited(Punc('('), Punc(')'), Punc(','), |s|
							s.declare_var()
						).0
					);
					Pattern::Func { func: var, args }
				} else {
					Pattern::Assignee(var)
				}
			}

		// Anonymous structure matching
			Punc('[') =>
				Pattern::Struct(Vec::from(
					self.delimited(Punc('['), Punc(']'), Punc(','), |s| {
						let field = s.parse_parameter();
						if Some(Punc(':')) == s.peek().map(|t| t.val) {
							s.next();
							(field.name, s.parse_pattern())
						} else {
							s.declare(&field);
							(field.name.clone(), Pattern::Assignee(field))
						}
					}).0
				)),

			not_pattern => panic!("Expected a pattern, found {:#?}", not_pattern)
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

pub fn get_type_var_id() -> TypeVarId {
	TYPE_VAR_COUNTER.fetch_add(1, Ordering::SeqCst)
}

pub fn get_type_var() -> Type {
	Type::Free(get_type_var_id())
}

fn new_expr(loc: SourceLoc, value: ExprVal) -> Expr {
	Expr {
		val: value,
		loc,
		r#type: get_type_var(),
	}
}
