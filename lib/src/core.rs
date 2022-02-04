use std::collections::HashMap;
use crate::ast::{
	Type,
	Type::*,
	TConstructor
};

pub fn core_vals() -> HashMap<String, Type> {
	[
		("print", vec![Str, Void]),
		("itoa", vec![Int, Str]),
		("ftoa", vec![Float, Str]),
	].iter().map(|(name, args)|
		(
			name.to_string(),
			Type::TypeConstructor(TConstructor {
				name: "Function".to_string(),
				args: args.clone(),
			})
		)
	).collect()
}

pub const CORE_FNS_JS: &str =
r#"var buffered = "";
function _print(s) {
	buffered += String(s);
}
var _itoa = String;
var _ftoa = String;
"#;

pub const CORE_FNS_9: &str =
r#"#include <u.h>
#include <libc.h>

void _print(char* s){ print("%s", s); }

char*
_itoa(vlong val)
{
	return smprint("%lld", val);
}

char*
_ftoa(long double d)
{
	return smprint("%llf", d);
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

void _print(char* s){ printf("%s", s); }

char*
_itoa(long long int val)
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
_ftoa(long double n)
{
	char *buf = malloc(50);
	snprintf(buf, 50, "%llf", n);

	return buf;
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
