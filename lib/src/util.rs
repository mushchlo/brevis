use std::collections::HashMap;


pub trait Env<K, V>
where K: std::cmp::Eq + std::hash::Hash
{
	fn find(&self, key: &K) -> Option<&V>;
	fn insert_in_env(&mut self, key: K, val: V);
	fn new_stack(&mut self);
	fn pop_stack(&mut self);
}

// A helper function that ended up being written ungenerically in several places
// in the compiler, despite using a common pattern for env types

impl<K, V> Env<K, V> for Vec<HashMap<K, V>>
where K: std::cmp::Eq + std::hash::Hash
{
	fn find(&self, key: &K) -> Option<&V>
	{
		for map in self {
			if let Some(v) = map.get(key) {
				return Some(v);
			}
		}

		None
	}

	fn insert_in_env(&mut self, key: K, value: V) {
		self.last_mut().unwrap().insert(key, value);
	}

	fn new_stack(&mut self) {
		self.push(HashMap::new());
	}

	fn pop_stack(&mut self) {
		self.pop();
	}
}