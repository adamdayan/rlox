use std::collections::HashMap;

use super::{interpreter::RuntimeError, scanner::tokens::Value};

/// Stores the variables and their values present in an environment
#[derive(Debug)]
pub struct Environment<'parent> {
    values: HashMap<String, Value>,
    enclosing: Option<&'parent mut Environment<'parent>>,
}

impl<'parent> Environment<'parent> {
    pub fn new(enclosing: Option<&'parent mut Environment<'parent>>) -> Self {
        Environment {
            values: HashMap::new(),
            enclosing,
        }
    }

    /// Define a variable in the environment. Defining a variable name multiple times is valid
    pub fn define(&mut self, name: String, val: Value) {
        self.values.insert(name, val);
    }

    /// Gets a variable's value; if not present returns RuntimeError::UndefinedVariable. We return
    /// a RuntimeError on undefined variable use because it is valid to refer to an undefined
    /// variable e.g. in a function definition and recursive function definitions become
    /// challenging if undefined variable use is a syntax error
    pub fn get(&self, name: &String) -> Result<Value, RuntimeError> {
        match self.values.get(name) {
            // NOTE: is it right that we clone() here? We need visit_variable to return Value, not
            // &Value but perhaps the clone should take place in visit_variable?
            Some(val) => Ok(val.clone()),
            None => {
                if let Some(enc) = &self.enclosing {
                    return enc.get(name);
                }
                Err(RuntimeError::UndefinedVariable { name: name.clone() })
            }
        }
    }

    pub fn assign(&mut self, name: &String, value: Value) -> Result<(), RuntimeError> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value);
        } else {
            if let Some(enc) = self.enclosing.as_mut() {
                return enc.assign(name, value);
            }
            return Err(RuntimeError::UndefinedVariable { name: name.clone() });
        }
        Ok(())
    }
}
