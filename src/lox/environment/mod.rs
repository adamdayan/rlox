use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::interpreter::{RuntimeError, RuntimeValue};

// TODO: move Rc<RefCell<>> inside Environment
/// Stores the variables and their values present in an environment
#[derive(Debug, PartialEq)]
pub struct Environment {
    values: RefCell<HashMap<String, RuntimeValue>>,
    // NOTE: not super happy about using Rc<RefCell> here, I think it should b epossible with plain
    // references but I couldn't get the lifetimes to work
    enclosing: Option<Rc<Environment>>,
}

impl Environment {
    pub fn new(enclosing: Option<Rc<Environment>>) -> Self {
        Environment {
            values: RefCell::new(HashMap::new()),
            enclosing,
        }
    }

    /// Define a variable in the environment. Defining a variable name multiple times is valid
    pub fn define(&self, name: String, val: RuntimeValue) {
        self.values.borrow_mut().insert(name, val);
    }

    /// Gets a variable's value; if not present returns RuntimeError::UndefinedVariable. We return
    /// a RuntimeError on undefined variable use because it is valid to refer to an undefined
    /// variable e.g. in a function definition and recursive function definitions become
    /// challenging if undefined variable use is a syntax error
    pub fn get(&self, name: &str) -> Result<RuntimeValue, RuntimeError> {
        match self.values.borrow().get(name) {
            // NOTE: is it right that we clone() here? We need visit_variable to return RuntimeValue, not
            // &RuntimeValue but perhaps the clone should take place in visit_variable?
            Some(val) => Ok(val.clone()),
            None => {
                if let Some(enc) = &self.enclosing {
                    return enc.get(name);
                }
                Err(RuntimeError::UndefinedVariable {
                    name: name.to_owned(),
                })
            }
        }
    }

    pub fn get_at(&self, name: &str, depth: usize) -> Result<RuntimeValue, RuntimeError> {
        let mut env = self;
        for i in 0..depth {
            // NOTE: couldn't do get_ancestor because without Rc env might be dead and with Rc the
            // types differed
            if let Some(anc) = &env.enclosing {
                env = anc
            } else {
                return Err(RuntimeError::BadEnvironmentDepth(i));
            }
        }
        env.get(name)
    }

    pub fn assign(&self, name: &String, value: RuntimeValue) -> Result<(), RuntimeError> {
        if self.values.borrow().contains_key(name) {
            self.values.borrow_mut().insert(name.to_string(), value);
        } else {
            if let Some(enc) = self.enclosing.as_ref() {
                return enc.assign(name, value);
            }
            return Err(RuntimeError::UndefinedVariable { name: name.clone() });
        }
        Ok(())
    }
}
