use std::collections::HashMap;

use super::{callable::Callable, interpreter::RuntimeError};

#[derive(Clone, Debug, PartialEq)]
pub struct LoxClass {
    name: String,
    methods: HashMap<String, Callable>,
}
impl LoxClass {
    pub fn new(name: String, methods: HashMap<String, Callable>) -> Self {
        Self { name, methods }
    }

    pub fn find_method(&self, name: &str) -> Option<&Callable> {
        self.methods.get(name)
    }
}

impl std::fmt::Display for LoxClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}
