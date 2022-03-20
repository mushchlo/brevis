extern crate brevislib;
extern crate clap;

use brevislib::core;
use clap::{arg, Command};

fn main() {
	let args = Command::new("The Brevis Compiler")
		.version("0.0.1")
		.author("Chloe MacGregor <chloemacgregor29@gmail.com>")
		.about("Compiles Brevis source code for PCs and the web")
		.arg(
			arg!(<FILE> "Source code to compile")
				.required(true)
		)
		.arg(arg!(
			-d --debug ... "Prints out an inferred and annotated AST for debugging"
		))
		.arg(
			arg!(--target <TARGET> "The target language to compile to")
				.required(false)
				.possible_values(["9", "js", "c89"])
				.default_value("c89")
		)
		.get_matches();

	let (core, target) = match args.value_of("target").unwrap() {
		"9" => (core::CORE_FNS_9, "c"),
		"js" => (core::CORE_FNS_JS, "js"),
		"c89" => (core::CORE_FNS_POSIX, "c"),
		huh => panic!("{}", huh)
	};

	let contents =
		std::fs::read_to_string(args.value_of("FILE").unwrap())
			.expect("failed to open/read file");
	let print_ast = args.occurrences_of("debug") > 0;
	let compiled = brevislib::compile(
		&contents,
		core,
		target,
		print_ast,
		|e| eprintln!("{}", e)
	);
	println!("{}", compiled);

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
				brevislib::compile(
					&fs::read_to_string(file.clone() + ".bv").unwrap(),
					core::CORE_FNS_POSIX,
					"c",
					|e| eprintln!("{}", e),
				);


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
