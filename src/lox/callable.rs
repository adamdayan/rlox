use std::{cell::RefCell, rc::Rc};

use super::{
    ast::Function,
    environment::Environment,
    interpreter::{Interpreter, RuntimeError, RuntimeValue},
};

// NOTE: it might be better to make this an interface? However, how to make PartialEq? I think it
// would require Any + upcasting
#[derive(Clone)]
pub enum Callable<'t> {
    Function {
        decl: Function<'t>,
    },
    Native {
        arity: usize,
        function: Rc<dyn Fn(Vec<RuntimeValue<'t>>) -> RuntimeValue<'t>>,
    },
}
impl<'t> Callable<'t> {
    /// Execute the callable
    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<RuntimeValue<'t>>,
        // NOTE: is it actually right that we should use the immediately outer environment?
        env: &Rc<RefCell<Environment<'t>>>,
    ) -> Result<RuntimeValue<'t>, RuntimeError<'t>> {
        match self {
            Self::Function { decl } => {
                // construct function environment
                let func_env = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));
                // define each argument under its parameter name
                for (name, arg) in decl.params.iter().zip(arguments.iter()) {
                    func_env
                        .borrow_mut()
                        .define(name.lexeme.clone(), arg.clone());
                }
                // execute function block
                interpreter.execute_block(&decl.body, &func_env)?;
                // TODO: handle return statements
                Ok(RuntimeValue::Nil)
            }
            Self::Native { arity: _, function } => Ok(function(arguments)),
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            Self::Native { arity, function: _ } => *arity,
            Self::Function { decl } => decl.params.len(),
        }
    }
}

impl PartialEq for Callable<'_> {
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
            (Self::Function { decl: my_decl }, Self::Function { decl: their_decl }) => {
                my_decl == their_decl
            }
            (_, _) => false,
        }
    }
}

impl std::fmt::Display for Callable<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Native { .. } => write!(f, "<native fn>"),
            Self::Function { decl } => write!(f, "{:?}", decl),
        }
    }
}

impl std::fmt::Debug for Callable<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Native { .. } => write!(f, "<native fn>"),
            Self::Function { decl } => write!(f, "{:?}", decl),
        }
    }
}
