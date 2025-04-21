use std::{collections::HashMap, rc::Rc};

use super::callable::Callable;

#[derive(Clone, Debug, PartialEq)]
pub struct LoxClass {
    name: String,
    methods: HashMap<String, Callable>,
    superclass: Option<Rc<LoxClass>>,
}
impl LoxClass {
    pub fn new(
        name: String,
        methods: HashMap<String, Callable>,
        superclass: Option<Rc<LoxClass>>,
    ) -> Self {
        Self {
            name,
            methods,
            superclass,
        }
    }

    pub fn find_method(&self, name: &str) -> Option<&Callable> {
        self.methods.get(name).or_else(|| {
            if let Some(superclass) = &self.superclass {
                superclass.find_method(name)
            } else {
                None
            }
        })
    }
}

impl std::fmt::Display for LoxClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}
