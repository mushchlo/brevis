use crate::lex::cradle::SourceLoc;


#[derive(Clone, Hash, PartialEq, std::cmp::Eq)]
pub struct ErrorMessage {
	pub msg: String,
	pub origins: Vec<SourceLoc>,
}

pub fn print_error<F>(source: &str, err: ErrorMessage, errfn: F)
where F: Fn(String) {
	let origins_str = err.origins.iter()
		.map(|loc| format!("{}", loc.start))
		.reduce(|acc, next| acc + ", " + &next)
		.unwrap();
	errfn(format!("At {}, {}\n{}\n",
		origins_str,
		err.msg,
		err.origins.into_iter()
			.map(|loc| get_context(source, loc))
			.reduce(|acc, next| acc + "\n" + &next)
			.unwrap()
	));
}

fn get_context(source: &str, err_loc: SourceLoc) -> String {
	let line = source.lines().nth((err_loc.start.row - 1) as usize).unwrap();
	let end_col = err_loc.end + err_loc.start.col as usize - err_loc.start.index;
	let underline = line.chars().enumerate()
		.map(|(i, _)|
			if err_loc.start.col - 1 <= i as u32 && (1 + i as usize) < end_col {
				'~'
			} else {
				' '
			}
		)
		.collect::<String>();
	format!("{}\n{}", line, underline)
}
