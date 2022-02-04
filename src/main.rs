extern crate brevislib;

use std::{
    fs,
    env,
};
use brevislib::{
	core,
};

fn main() {
	if env::args().len() < 2 {
		panic!("please specify at least one source file");
	}

	let compile: Box<dyn Fn(String) -> String> =
		if env::args().any(|s| s == "-9") {
			Box::new(|s| brevislib::compile_c(s, core::CORE_FNS_9))
		} else if env::args().any(|s| s == "-js") {
			Box::new(|s| brevislib::compile_js(s, core::CORE_FNS_JS))
   		} else {
			Box::new(|s| brevislib::compile_c(s, core::CORE_FNS_POSIX))
		};

	let contents = fs::read_to_string(env::args().last().unwrap()).expect("failed to open/read file");
	println!("{}", compile(contents));

}

#[cfg(test)]
mod tests {
	extern crate regex;

	use self::regex::Regex;
	use std::{
		fs,
		process::{
			Command,
			Stdio,
		},
		path::Path,
		io::{Write, Read, BufReader},
	};
	use brevislib::core;

	#[test]
	fn behavior_tests() -> std::io::Result<()> {
		let source_re = Regex::new(r"^(.*?)\.bv$").unwrap();
		let files =
			fs::read_dir(Path::new("./tests/"))
				.unwrap()
				.map(|f| f.unwrap().path().to_str().unwrap().to_owned())
				.filter(|name| source_re.is_match(&name))
				.map(|name| source_re.captures(&name).unwrap()[1].to_string());

		let mut run_aout = Command::new("./a.out");
		run_aout.current_dir("./tests");

		for file in files {
			let mut gcc = Command::new("gcc")
				.args(["-W", "-Wall", "-pedantic", "-Wno-main", "-xc", "-"])
				.stdin(Stdio::piped())
				.stdout(Stdio::piped())
				.stderr(Stdio::piped())
				.current_dir("./tests")
				.spawn()
				.unwrap();

			let expected =
				fs::read_to_string(file.clone() + ".txt")
					.expect("failed to find output file");

			let compiled_c =
				brevislib::compile_c(fs::read_to_string(file.clone() + ".bv").unwrap(), core::CORE_FNS_POSIX);


			let mut gcc_out = BufReader::new(gcc.stdout.take().unwrap());
			let mut gcc_err = BufReader::new(gcc.stderr.take().unwrap());

			write!(gcc.stdin.as_ref().unwrap(), "{}", &compiled_c)?;

			gcc.wait()?;

			let output =
				String::from_utf8(run_aout.output().unwrap().stdout).unwrap();

			let (mut gcc_output, mut gcc_errors) = (String::new(), String::new());
			gcc_out.read_to_string(&mut gcc_output)?;
			gcc_err.read_to_string(&mut gcc_errors)?;

//			assert!(gcc_output.is_empty());
//			assert!(gcc_errors.is_empty());

			assert!(output == expected);

		}

		fs::remove_file(Path::new("./tests/a.out"))
	}
}
