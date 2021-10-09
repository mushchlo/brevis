extern crate brevislib;

use std::{
    fs,
    env
};
use brevislib::{
    core::*,
    *,
};

fn main() {
	let (lang, core_fns) = if env::args().any(|s| s == "-9") {
		("c", core::CORE_FNS_9)
	} else if env::args().any(|s| s == "-py") {
        ("py", "")
	} else if env::args().any(|s| s == "-js") {
		("js", "")
    } else {
		("c", core::CORE_FNS_POSIX)
	};

	let contents = fs::read_to_string("test.bv").expect("failed to open/read file");
	println!("{}",
			if lang == "c" {
				brevislib::compile_c(contents, core_fns)
			} else if lang == "js" {
			 	brevislib::compile_js(contents, core::CORE_FNS_JS)
			} else {
                 brevislib::compile_py(contents)
            }
    );
}
