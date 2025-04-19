use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::{
    class::LoxClass,
    interpreter::{RuntimeError, RuntimeValue},
    scanner::tokens::Token,
};

#[derive(Clone, Debug, PartialEq)]
pub struct LoxInstance {
    klass: Rc<LoxClass>,
    fields: HashMap<String, RuntimeValue>,
}
impl LoxInstance {
    pub fn new(klass: Rc<LoxClass>) -> Self {
        Self {
            klass,
            fields: HashMap::new(),
        }
    }

    pub fn get(me: &Rc<RefCell<LoxInstance>>, field: &Token) -> Result<RuntimeValue, RuntimeError> {
        if let Some(field) = me.borrow().fields.get(&field.lexeme).cloned() {
            Ok(field)
        } else {
            Ok(RuntimeValue::Callable(
                me.borrow()
                    .klass
                    .find_method(&field.lexeme)
                    .cloned()
                    .ok_or(RuntimeError::UndefinedVariable {
                        name: field.lexeme.clone(),
                    })?
                    .bind(me)?,
            ))
        }
    }

    pub fn set(&mut self, field: &Token, val: RuntimeValue) {
        self.fields.insert(field.lexeme.clone(), val);
    }
}

impl std::fmt::Display for LoxInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} instance", self.klass)
    }
}
