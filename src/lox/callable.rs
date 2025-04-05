use std::rc::Rc;

use super::{
    ast::Function,
    environment::Environment,
    interpreter::{Interpreter, RuntimeError, RuntimeValue},
};

// NOTE: it might be better to make this an interface? However, how to make PartialEq? I think it
// would require Any + upcasting
#[derive(Clone)]
pub enum Callable {
    Function {
        decl: Function,
        closure: Rc<Environment>,
    },
    Native {
        arity: usize,
        function: Rc<dyn Fn(Vec<RuntimeValue>) -> RuntimeValue>,
    },
}
impl Callable {
    /// Execute the callable
    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<RuntimeValue>,
        // NOTE: is it actually right that we should use the immediately outer environment?
    ) -> Result<RuntimeValue, RuntimeError> {
        match self {
            Self::Function { decl, closure } => {
                // construct function environment from the environment in which it's declared
                let func_env = Rc::new(Environment::new(Some(closure.clone())));
                // define each argument under its parameter name
                for (name, arg) in decl.params.iter().zip(arguments.iter()) {
                    func_env.define(name.lexeme.clone(), arg.clone());
                }
                // execute function block
                let val = match interpreter.execute_block(&decl.body, &func_env) {
                    Err(RuntimeError::Return(val)) => val,
                    _ => RuntimeValue::Nil,
                };
                // TODO: handle return statements
                Ok(val)
            }
            Self::Native { arity: _, function } => Ok(function(arguments)),
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            Self::Native { arity, function: _ } => *arity,
            Self::Function { decl, .. } => decl.params.len(),
        }
    }
}

impl PartialEq for Callable {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Native {
                    arity: my_arity,
                    function: my_function,
                },
                Self::Native {
                    arity: their_arity,
                    function: their_function,
                },
            ) => {
                // use ptr equality to check whether dyn trait Fns are the same
                my_arity == their_arity && std::ptr::eq(&my_function, &their_function)
            }
            (
                Self::Function {
                    decl: my_decl,
                    closure: my_closure,
                },
                Self::Function {
                    decl: their_decl,
                    closure: their_closure,
                },
            ) => my_decl == their_decl && my_closure == their_closure,
            (_, _) => false,
        }
    }
}

impl std::fmt::Display for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Native { .. } => write!(f, "<native fn>"),
            Self::Function { decl, .. } => write!(f, "fn {}", decl.name.lexeme),
        }
    }
}

impl std::fmt::Debug for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Native { .. } => write!(f, "<native fn>"),
            Self::Function { decl, .. } => write!(f, "{:?}", decl),
        }
    }
}
