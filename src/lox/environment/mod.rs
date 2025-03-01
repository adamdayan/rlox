use std::collections::HashMap;

use super::{interpreter::RuntimeError, scanner::tokens::Value};

/// Stores the variables and their values present in an environment
pub struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    /// Define a variable in the environment. Defining a variable name multiple times is valid
    pub fn define(&mut self, name: String, val: Value) {
        self.values.insert(name, val);
    }

    /// Gets a variable's value; if not present returns RuntimeError::UndefinedVariable.
    pub fn get(&self, name: &String) -> Result<&Value, RuntimeError> {
        // NOTE: should this return &Value or Value?
        self.values
            .get(name)
            .ok_or(RuntimeError::UndefinedVariable { name: name.clone() })
    }
}
