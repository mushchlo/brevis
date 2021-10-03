extern crate maplit;
extern crate lazy_static;

use lazy_static::lazy_static;
use maplit::hashmap;
use std::collections::HashMap;
use crate::ast::{
	Type,
	TConstructor
};

pub fn core_vals() -> HashMap<String, Type> {
	hashmap! {
		"print".to_string() =>
			Type::TypeConstructor(TConstructor {
				name: "Function".to_string(),
				args: vec![Type::Str, Type::Void]
			}),
		"itoa".to_string() =>
			Type::TypeConstructor(TConstructor {
				name: "Function".to_string(),
				args: vec![Type::Int, Type::Str]
			}),
	}
}

pub const CORE_FNS_9: &str =
r#"#include <u.h>
#include <libc.h>

void _print(char* s){ print("%s", s); }

char*
_itoa(vlong val)
{
	smprint("%lld", val);
}

char*
concat(char* s1, char* s2)
{
	smprint("%s%s", s1, s2);
}"#;

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