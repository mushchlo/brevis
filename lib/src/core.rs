use std::collections::HashMap;
use crate::types::{
	Type,
	Type::*,
};
use lazy_static::lazy_static;
use parse::get_type_var;

lazy_static! {
	pub static ref core_vals: HashMap<String, Type> =
		[
			("print", vec![Str, Void]),
			("itoa", vec![Int, Str]),
			("ftoa", vec![Float, Str]),
			("sizeof",
				vec![
					get_type_var(),
					Int,
				]
			),
			("alloc", {
				let t = get_type_var();
				vec![
					Int,
					Type::Pointer(box t)
				]
			}),
		].iter().map(|(name, args)|
			(
				name.to_string(),
				Type::Func(args.clone())
			)
		).collect();
}

pub const CORE_FNS_JS: &str =
r#"var buffered = "";
function print(s) {
	buffered += String(s);
}
var itoa = String;
var ftoa = String;
"#;

pub const CORE_FNS_9: &str =
r#"#include <u.h>
#include <libc.h>

char*
itoa(vlong val)
{
	return smprint("%lld", val);
}

char*
ftoa(long double d)
{
	return smprint("%llf", d);
}

void*
alloc(ulong size)
{
	void* allocated = malloc(size);
	if(allocated == nil)
		exits("Failed allocation! Exiting...");
	return allocated;
}

char*
concat(char* s1, char* s2)
{
	return smprint("%s%s", s1, s2);
}
"#;

pub const CORE_FNS_POSIX: &str =
r#"#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void print(char* s){ printf("%s", s); }

char*
itoa(long long int val)
{
	static char buf[32] = {0};
	int i = 30;

	if(val == 0){
		strcpy(buf, "0");
		return buf;
	}
	for(; val && i; --i, val /= 10)
		buf[i] = "0123456789"[val % 10];

	return &buf[i+1];
}

char*
ftoa(long double n)
{
	char *buf = malloc(50);
	snprintf(buf, 50, "%llf", n);

	return buf;
}

void*
alloc(unsigned long size)
{
	void* allocated = malloc(size);
	if(allocated == NULL){
		fprintf(stderr, "Failed allocation! Exiting...");
		exit(1);
	}
	return allocated;
}

char*
concat(const char *s1, const char *s2)
{
	size_t len1,len2;
	char *result = malloc((len1 = strlen(s1)) + (len2 = strlen(s2)) + 1);

	if(result){
		memcpy(result, s1, len1);
		memcpy(result + len1, s2, len2 + 1);
	}
	return result;
}

"#;
