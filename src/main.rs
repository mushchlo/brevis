extern crate brevislib;

use std::{
    fs,
    env,
};
use brevislib::{
	core,
};

fn main() {
	let compile: Box<dyn Fn(String) -> String> =
		if env::args().any(|s| s == "-9") {
			Box::new(|s| brevislib::compile_c(s, core::CORE_FNS_9))
		} else if env::args().any(|s| s == "-js") {
			Box::new(|s| brevislib::compile_js(s, ""))
   		} else {
			Box::new(|s| brevislib::compile_c(s, core::CORE_FNS_POSIX))
		};

	let contents = fs::read_to_string("test.bv").expect("failed to open/read file");
	println!("{}", compile(contents));
}
