use std::{cell::RefCell, rc::Rc};

use super::{
    ast::Function,
    class::LoxClass,
    environment::Environment,
    instance::LoxInstance,
    interpreter::{Interpreter, RuntimeError, RuntimeValue},
};

// NOTE: it might be better to make this an interface? However, how to make PartialEq? I think it
// would require Any + upcasting
#[derive(Clone)]
pub enum Callable {
    Function {
        decl: Function,
        closure: Rc<Environment>,
        is_initialiser: bool,
    },
    Native {
        arity: usize,
        function: Rc<dyn Fn(Vec<RuntimeValue>) -> RuntimeValue>,
    },
    Class {
        class: Rc<LoxClass>,
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
            Self::Function {
                decl,
                closure,
                is_initialiser,
            } => {
                // construct function environment from the environment in which it's declared
                let func_env = Rc::new(Environment::new(Some(closure.clone())));
                // define each argument under its parameter name
                for (name, arg) in decl.params.iter().zip(arguments.iter()) {
                    func_env.define(name.lexeme.clone(), arg.clone());
                }
                // execute function block
                let inner_func_env = Rc::new(Environment::new(Some(func_env.clone())));
                let val = match interpreter.execute_block(&decl.body, &inner_func_env) {
                    Err(RuntimeError::Return(val)) => {
                        if *is_initialiser {
                            return closure.get("this");
                        }
                        val
                    }
                    Err(e) => return Err(e),
                    _ => RuntimeValue::Nil,
                };
                // TODO: handle return statements
                Ok(val)
            }
            Self::Native { arity: _, function } => Ok(function(arguments)),
            Self::Class { class } => {
                let instance = Rc::new(RefCell::new(LoxInstance::new(class.clone())));
                // if we have a constructor, call it on the instance
                if let Some(init) = class.find_method("init") {
                    let _ = init
                        .clone()
                        .bind(&instance)
                        .expect("must bind successfully")
                        .call(interpreter, arguments)?;
                };
                Ok(RuntimeValue::Instance(instance))
            }
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            Self::Native { arity, function: _ } => *arity,
            Self::Function { decl, .. } => decl.params.len(),
            Self::Class { class } => class.find_method("init").map_or(0, |init| init.arity()),
        }
    }

    pub fn bind(&mut self, instance: &Rc<RefCell<LoxInstance>>) -> Result<Self, RuntimeError> {
        match self {
            Self::Function {
                decl,
                closure,
                is_initialiser,
            } => {
                let env = Environment::new(Some(closure.clone()));
                env.define("this".to_owned(), RuntimeValue::Instance(instance.clone()));
                Ok(Self::Function {
                    decl: decl.clone(),
                    closure: Rc::new(env),
                    is_initialiser: *is_initialiser,
                })
            }
            other => Err(RuntimeError::NotAFunction(other.clone())),
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
                    is_initialiser: my_is_initialiser,
                },
                Self::Function {
                    decl: their_decl,
                    closure: their_closure,
                    is_initialiser: their_is_initialiser,
                },
            ) => {
                my_decl == their_decl
                    && my_closure == their_closure
                    && my_is_initialiser == their_is_initialiser
            }
            (_, _) => false,
        }
    }
}

impl std::fmt::Display for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Native { .. } => write!(f, "<native fn>"),
            Self::Function { decl, .. } => write!(f, "fn {}", decl.name.lexeme),
            Self::Class { class } => write!(f, "{class}"),
        }
    }
}

impl std::fmt::Debug for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Native { .. } => write!(f, "<native fn>"),
            Self::Function { decl, .. } => write!(f, "{:?}", decl),
            Self::Class { class } => write!(f, "{class:?}"),
        }
    }
}
