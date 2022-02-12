use crate::lex::cradle::SourceLoc;

pub struct ErrorMessage {
	pub msg: String,
	pub origin: SourceLoc,
}

pub fn print_error<F>(source: &str, err: ErrorMessage, errfn: F)
where F: Fn(String) {
	errfn(format!("At {}, {}\n{}\n",
		err.origin.start,
		err.msg,
		get_context(source, err.origin)
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
