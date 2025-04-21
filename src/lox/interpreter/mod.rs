use std::{cell::RefCell, collections::HashMap, ops::Deref, rc::Rc};

use crate::lox::ast::{Expr, ExprVisitor};

use super::{
    ast::{
        Assign, Binary, Block, Call, Class, Function, Get, Grouping, If, Literal, Logical,
        PrintExpression, PureExpression, Return, Set, Stmt, StmtVisitor, This, Unary, Variable,
        VariableDeclaration, While,
    },
    callable::Callable,
    class::LoxClass,
    environment::Environment,
    instance::LoxInstance,
    resolver::Resolvable,
    scanner::tokens::{ParsedValue, Token, TokenType},
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("Invalid operator: {0}. Line {1}", operator.lexeme, operator.line)]
    InvalidOperator { operator: Rc<Token> },
    #[error("Invalid operand: {:?}. Line {}", val, operator.line)]
    InvalidOperand {
        operator: Rc<Token>,
        val: RuntimeValue,
    },
    #[error("Invalid operands: {:?} {:?}. Line {}", left_val, right_val, operator.line)]
    InvalidOperands {
        operator: Rc<Token>,
        left_val: RuntimeValue,
        right_val: RuntimeValue,
    },
    #[error("Undefined variable: {0}", name)]
    UndefinedVariable { name: String },
    #[error("Cannot call non-callable value: {:?}", value)]
    CallNonCallable { value: RuntimeValue },
    #[error("Provided {0} arguments for function with arity {1}")]
    WrongArgsNum(usize, usize),
    #[error("No enclosing environment at depth {0}")]
    BadEnvironmentDepth(usize),
    #[error("Expressions of type {0:?} are not resolvable")]
    UnresolvableExpression(Expr),
    #[error("{0} is not an Object, can only access property on an object")]
    NotAnObject(RuntimeValue),
    #[error("{0} is not a Function")]
    NotAFunction(Callable),
    #[error("Superclass must be a class: {0}")]
    BadSuperType(RuntimeValue),
    // TODO: replace this HORRIBLE hack with std::core::ops::ControlFlow once its stabilised
    /// Used as a hack to extract return values from deep in the call stack
    #[error("NOT A REAL ERROR")]
    Return(RuntimeValue),
}

fn is_truthy(val: &RuntimeValue) -> bool {
    match val {
        RuntimeValue::Nil => false,
        RuntimeValue::Boolean(val) => *val,
        _ => true,
    }
}

/// Values computed at runtime.
#[derive(Clone, Debug, PartialEq)]
pub enum RuntimeValue {
    Boolean(bool),
    // Use Rc for non-copy type
    String(Rc<String>),
    Number(f32),
    Callable(Callable),
    Nil,
    Instance(Rc<RefCell<LoxInstance>>),
}

impl From<&ParsedValue> for RuntimeValue {
    fn from(val: &ParsedValue) -> Self {
        match val {
            ParsedValue::Boolean(v) => RuntimeValue::Boolean(*v),
            ParsedValue::String(v) => RuntimeValue::String(v.clone()),
            ParsedValue::Number(v) => RuntimeValue::Number(*v),
            ParsedValue::Nil => RuntimeValue::Nil,
        }
    }
}

impl std::fmt::Display for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeValue::Boolean(val) => write!(f, "{val}"),
            RuntimeValue::String(val) => write!(f, "{val}"),
            RuntimeValue::Number(val) => {
                if val % 1.0 == 0.0 {
                    return write!(f, "{:.0}", val);
                }
                write!(f, "{val}")
            }
            RuntimeValue::Nil => write!(f, "nil"),
            RuntimeValue::Callable(func) => write!(f, "{func}"),
            RuntimeValue::Instance(instance) => write!(f, "{}", instance.borrow()),
        }
    }
}

pub struct Interpreter {
    // TODO: decide if I actually want to use this
    had_runtime_error: bool,
    locals: HashMap<Resolvable, usize>,
    globals: Rc<Environment>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            had_runtime_error: false,
            locals: HashMap::new(),
            globals: Rc::new(Environment::new(None)),
        }
    }

    pub fn interpret(&mut self, statements: &[Stmt]) -> Result<(), RuntimeError> {
        self.define_native_functions();

        for statement in statements {
            self.execute(statement)?;
        }

        Ok(())
    }

    /// Defines native functions in the global environment
    fn define_native_functions(&mut self) {
        // add clock() method
        self.globals.define(
            "clock".to_owned(),
            RuntimeValue::Callable(Callable::Native {
                arity: 0,
                function: Rc::new(|_: Vec<RuntimeValue>| {
                    let seconds_since_epoch = std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .expect("Time went backwards")
                        .as_secs();
                    RuntimeValue::Number(seconds_since_epoch as f32)
                }),
            }),
        );
    }

    fn execute(&mut self, statement: &Stmt) -> Result<(), RuntimeError> {
        self.visit_statement(statement, &self.globals.clone())
    }

    pub fn execute_block(
        &mut self,
        statements: &[Stmt],
        env: &Rc<Environment>,
    ) -> Result<(), RuntimeError> {
        for stmt in statements {
            {
                self.visit_statement(stmt, env)?;
            }
        }
        Ok(())
    }

    fn evaluate(
        &mut self,
        expr: &Expr,
        env: &Rc<Environment>,
    ) -> Result<RuntimeValue, RuntimeError> {
        self.visit_expr(expr, env)
    }

    pub fn resolve(&mut self, resolvable: Resolvable, depth: usize) {
        self.locals.insert(resolvable, depth);
    }

    fn lookup_variable(
        &self,
        name: &Token,
        var: &Variable,
        env: Rc<Environment>,
    ) -> Result<RuntimeValue, RuntimeError> {
        if let Some(dist) = self.locals.get(&Resolvable::Variable(var.clone())) {
            env.get_at(&name.lexeme, *dist)
        } else {
            self.globals.get(&name.lexeme)
        }
    }
}

/// Visitor pattern that evaluates expressions
impl ExprVisitor<Result<RuntimeValue, RuntimeError>> for Interpreter {
    fn visit_expr(
        &mut self,
        expr: &Expr,
        env: &Rc<Environment>,
    ) -> Result<RuntimeValue, RuntimeError> {
        match expr {
            Expr::Binary(binary) => self.visit_binary(binary, env),
            Expr::Unary(unary) => self.visit_unary(unary, env),
            Expr::Grouping(grouping) => self.visit_grouping(grouping, env),
            Expr::Literal(literal) => self.visit_literal(literal, env),
            Expr::Variable(variable) => self.visit_variable(variable, env),
            Expr::Assign(assign) => self.visit_assign(assign, env),
            Expr::Logical(logic) => self.visit_logical(logic, env),
            Expr::Call(call) => self.visit_call(call, env),
            Expr::Get(get) => self.visit_get(get, env),
            Expr::Set(set) => self.visit_set(set, env),
            Expr::This(this) => self.visit_this(this, env),
        }
    }

    fn visit_literal(
        &mut self,
        literal: &Literal,
        _env: &Rc<Environment>,
    ) -> Result<RuntimeValue, RuntimeError> {
        // NOTE: clone seems very wasteful in the string case. But I think Value (not &Valye) is required because I could end up
        // constructing new Values at runtime e.g. concat, adding etc?
        Ok((&literal.0).into())
    }

    fn visit_grouping(
        &mut self,
        grouping: &Grouping,
        env: &Rc<Environment>,
    ) -> Result<RuntimeValue, RuntimeError> {
        self.visit_expr(&grouping.0, env)
    }

    fn visit_unary(
        &mut self,
        unary: &Unary,
        env: &Rc<Environment>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let inner_value = self.visit_expr(&unary.right, env)?;
        match (&unary.operator.token_type, inner_value) {
            (TokenType::Minus, RuntimeValue::Number(num)) => Ok(RuntimeValue::Number(-num)),
            // "-" operator with non-Number inner_value is invalid
            (TokenType::Minus, val) => Err(RuntimeError::InvalidOperand {
                operator: unary.operator.clone(),
                val,
            }),
            (TokenType::Bang, val) => Ok(RuntimeValue::Boolean(!is_truthy(&val))),
            // no other operator types are valid for a unary expression
            (_, _) => Err(RuntimeError::InvalidOperator {
                operator: unary.operator.clone(),
            }),
        }
    }

    fn visit_binary(
        &mut self,
        binary: &Binary,
        env: &Rc<Environment>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let left_value = self.visit_expr(&binary.left, env)?;
        let right_value = self.visit_expr(&binary.right, env)?;

        match &binary.operator.token_type {
            // Greater
            TokenType::Greater => match (&left_value, &right_value) {
                (RuntimeValue::Number(left_num), RuntimeValue::Number(right_num)) => {
                    Ok(RuntimeValue::Boolean(left_num > right_num))
                }
                (_, _) => Err(RuntimeError::InvalidOperands {
                    operator: binary.operator.clone(),
                    left_val: left_value,
                    right_val: right_value,
                }),
            },
            // GreaterEqual
            TokenType::GreaterEqual => match (&left_value, &right_value) {
                (RuntimeValue::Number(left_num), RuntimeValue::Number(right_num)) => {
                    Ok(RuntimeValue::Boolean(left_num >= right_num))
                }
                (_, _) => Err(RuntimeError::InvalidOperands {
                    operator: binary.operator.clone(),
                    left_val: left_value,
                    right_val: right_value,
                }),
            },
            // Less
            TokenType::Less => match (&left_value, &right_value) {
                (RuntimeValue::Number(left_num), RuntimeValue::Number(right_num)) => {
                    Ok(RuntimeValue::Boolean(left_num < right_num))
                }
                (_, _) => Err(RuntimeError::InvalidOperands {
                    operator: binary.operator.clone(),
                    left_val: left_value,
                    right_val: right_value,
                }),
            },
            // LessEqual
            TokenType::LessEqual => match (&left_value, &right_value) {
                (RuntimeValue::Number(left_num), RuntimeValue::Number(right_num)) => {
                    Ok(RuntimeValue::Boolean(left_num <= right_num))
                }
                (_, _) => Err(RuntimeError::InvalidOperands {
                    operator: binary.operator.clone(),
                    left_val: left_value,
                    right_val: right_value,
                }),
            },
            // subtraction
            TokenType::Minus => match (&left_value, &right_value) {
                (RuntimeValue::Number(left_num), RuntimeValue::Number(right_num)) => {
                    Ok(RuntimeValue::Number(left_num - right_num))
                }
                (_, _) => Err(RuntimeError::InvalidOperands {
                    operator: binary.operator.clone(),
                    left_val: left_value,
                    right_val: right_value,
                }),
            },
            // division
            TokenType::Slash => match (&left_value, &right_value) {
                (RuntimeValue::Number(left_num), RuntimeValue::Number(right_num)) => {
                    Ok(RuntimeValue::Number(left_num / right_num))
                }
                (_, _) => Err(RuntimeError::InvalidOperands {
                    operator: binary.operator.clone(),
                    left_val: left_value,
                    right_val: right_value,
                }),
            },
            // multiplication
            TokenType::Star => match (&left_value, &right_value) {
                (RuntimeValue::Number(left_num), RuntimeValue::Number(right_num)) => {
                    Ok(RuntimeValue::Number(left_num * right_num))
                }
                (_, _) => Err(RuntimeError::InvalidOperands {
                    operator: binary.operator.clone(),
                    left_val: left_value,
                    right_val: right_value,
                }),
            },
            // "+" is used for both number addition and string concatenation
            TokenType::Plus => match (&left_value, &right_value) {
                // addition
                (RuntimeValue::Number(left_num), RuntimeValue::Number(right_num)) => {
                    Ok(RuntimeValue::Number(left_num + right_num))
                }
                // concatenation
                (RuntimeValue::String(left_str), RuntimeValue::String(right_str)) => Ok(
                    RuntimeValue::String(Rc::new(left_str.deref().to_owned() + right_str)),
                ),
                (_, _) => Err(RuntimeError::InvalidOperands {
                    operator: binary.operator.clone(),
                    left_val: left_value,
                    right_val: right_value,
                }),
            },
            _ => Err(RuntimeError::InvalidOperator {
                operator: binary.operator.clone(),
            }),
        }
    }

    fn visit_variable(
        &mut self,
        variable: &Variable,
        env: &Rc<Environment>,
    ) -> Result<RuntimeValue, RuntimeError> {
        self.lookup_variable(&variable.name, variable, env.clone())
    }

    fn visit_assign(
        &mut self,
        assign: &Assign,
        env: &Rc<Environment>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let val = self.evaluate(&assign.value, env)?;
        env.assign(&assign.name.lexeme, val.clone())?;
        Ok(val)
    }

    fn visit_logical(
        &mut self,
        logical: &Logical,
        env: &Rc<Environment>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let left = self.evaluate(&logical.left, env)?;
        // check Or's shortcircuit
        if logical.operator.token_type == TokenType::Or {
            if is_truthy(&left) {
                return Ok(left);
            }
        } else {
            // check And's shortcircuit
            if !is_truthy(&left) {
                return Ok(left);
            }
        }
        self.evaluate(&logical.right, env)
    }

    fn visit_call(
        &mut self,
        call: &Call,
        env: &Rc<Environment>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let callee = self.evaluate(&call.callee, env)?;
        let arguments = call
            .arguments
            .iter()
            .map(|a| self.evaluate(a, env))
            .collect::<Result<Vec<_>, RuntimeError>>()?;

        match callee {
            RuntimeValue::Callable(callable) => {
                if arguments.len() != callable.arity() {
                    return Err(RuntimeError::WrongArgsNum(
                        arguments.len(),
                        callable.arity(),
                    ));
                }
                callable.call(self, arguments)
            }
            _ => Err(RuntimeError::CallNonCallable { value: callee }),
        }
    }

    fn visit_get(
        &mut self,
        get: &Get,
        env: &Rc<Environment>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let obj = self.evaluate(&get.object, env)?;
        match obj {
            RuntimeValue::Instance(instance) => Ok(LoxInstance::get(&instance, &get.name)?),
            _ => Err(RuntimeError::NotAnObject(obj)),
        }
    }

    fn visit_set(
        &mut self,
        set: &Set,
        env: &Rc<Environment>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let obj = self.evaluate(&set.object, env)?;
        if let RuntimeValue::Instance(instance) = obj {
            let val = self.evaluate(&set.val, env)?;
            instance.borrow_mut().set(&set.name, val.clone());
            Ok(val)
        } else {
            Err(RuntimeError::NotAnObject(obj))
        }
    }

    fn visit_this(
        &mut self,
        this: &This,
        env: &Rc<Environment>,
    ) -> Result<RuntimeValue, RuntimeError> {
        if let Some(dist) = self.locals.get(&Resolvable::This(this.clone())) {
            env.get_at(&this.0.lexeme, *dist)
        } else {
            self.globals.get(&this.0.lexeme)
        }
    }
}

impl StmtVisitor<Result<(), RuntimeError>> for Interpreter {
    fn visit_statement(
        &mut self,
        statement: &Stmt,
        env: &Rc<Environment>,
    ) -> Result<(), RuntimeError> {
        match statement {
            Stmt::Expression(expr) => self.visit_expression_statement(expr, env),
            Stmt::Print(value) => self.visit_print_statement(value, env),
            Stmt::VariableDeclaration(decl) => self.visit_variable_declaration(decl, env),
            Stmt::Block(block) => self.visit_block(block, env),
            Stmt::If(if_statement) => self.visit_if(if_statement, env),
            Stmt::While(while_statement) => self.visit_while(while_statement, env),
            Stmt::Function(function_statement) => self.visit_function(function_statement, env),
            Stmt::Return(return_statement) => self.visit_return(return_statement, env),
            Stmt::Class(class_statement) => self.visit_class(class_statement, env),
        }
    }

    fn visit_expression_statement(
        &mut self,
        expression: &PureExpression,
        env: &Rc<Environment>,
    ) -> Result<(), RuntimeError> {
        let _value = self.evaluate(&expression.0, env)?;
        Ok(())
    }

    fn visit_print_statement(
        &mut self,
        print_expression: &PrintExpression,
        env: &Rc<Environment>,
    ) -> Result<(), RuntimeError> {
        let value = self.evaluate(&print_expression.0, env)?;
        println!("{value}");
        Ok(())
    }

    fn visit_variable_declaration(
        &mut self,
        variable_declaration: &VariableDeclaration,
        env: &Rc<Environment>,
    ) -> Result<(), RuntimeError> {
        let val = match &variable_declaration.initialiser {
            None => RuntimeValue::Nil,
            Some(init) => self.evaluate(init, env)?,
        };
        env.define(variable_declaration.name.lexeme.clone(), val);
        Ok(())
    }

    fn visit_block(&mut self, block: &Block, env: &Rc<Environment>) -> Result<(), RuntimeError> {
        let block_env = Rc::new(Environment::new(Some(env.clone())));
        self.execute_block(&block.inner, &block_env)
    }

    fn visit_if(&mut self, if_statement: &If, env: &Rc<Environment>) -> Result<(), RuntimeError> {
        let cond = self.evaluate(&if_statement.condition, env)?;
        // execute the right branch
        if is_truthy(&cond) {
            self.visit_statement(&if_statement.then_branch, env)?;
        } else {
            // we can only execute the else statement if there is one
            if let Some(else_branch) = &if_statement.else_branch {
                self.visit_statement(else_branch, env)?;
            }
        }
        Ok(())
    }

    fn visit_while(
        &mut self,
        while_statement: &While,
        env: &Rc<Environment>,
    ) -> Result<(), RuntimeError> {
        while is_truthy(&(self.evaluate(&while_statement.condition, env)?)) {
            self.visit_statement(&while_statement.body, env)?;
        }
        Ok(())
    }

    fn visit_function(
        &mut self,
        function_statement: &Function,
        env: &Rc<Environment>,
    ) -> Result<(), RuntimeError> {
        let func = Callable::Function {
            // NOTE: do I really need to clone here()
            decl: function_statement.clone(),
            // functions "close-over" the environment in which they're declared
            closure: env.clone(),
            is_initialiser: false,
        };
        env.define(
            function_statement.name.lexeme.clone(),
            RuntimeValue::Callable(func),
        );
        Ok(())
    }

    fn visit_return(
        &mut self,
        return_statement: &Return,
        env: &Rc<Environment>,
    ) -> Result<(), RuntimeError> {
        let val = if let Some(expr) = &return_statement.val {
            self.evaluate(expr, env)?
        } else {
            RuntimeValue::Nil
        };
        // we use the HORRIBLE hack of a fake Return error to more easily return through the call
        // stack. Will replace with std::ops::core::ControlFlow as soon as its stabilised
        Err(RuntimeError::Return(val))
    }

    fn visit_class(
        &mut self,
        class_statement: &Class,
        env: &Rc<Environment>,
    ) -> Result<(), RuntimeError> {
        let superclass = if let Some(superclass_var) = &class_statement.superclass {
            let superclass = self.evaluate(superclass_var, env)?;
            match superclass.clone() {
                RuntimeValue::Callable(Callable::Class { class: superclass }) => Some(superclass),
                _ => return Err(RuntimeError::BadSuperType(superclass)),
            }
        } else {
            None
        };

        env.define(class_statement.name.lexeme.clone(), RuntimeValue::Nil);
        let methods: HashMap<String, Callable> = class_statement
            .methods
            .iter()
            .map(|m| {
                (
                    m.name.lexeme.clone(),
                    Callable::Function {
                        decl: m.clone(),
                        closure: env.clone(),
                        is_initialiser: m.name.lexeme == "init",
                    },
                )
            })
            .collect();

        let klass = LoxClass::new(class_statement.name.lexeme.clone(), methods, superclass);
        env.assign(
            &class_statement.name.lexeme,
            RuntimeValue::Callable(Callable::Class {
                class: Rc::new(klass),
            }),
        )?;
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_interpreter() {
        let operator = Rc::new(Token::new(TokenType::Star, "*".to_owned(), None, 0, 0));
        let inner_operator = Rc::new(Token::new(TokenType::Minus, "-".to_owned(), None, 0, 0));
        let expr = Expr::Binary(Binary {
            operator: operator.clone(),
            left: Box::new(Expr::Unary(Unary {
                operator: inner_operator.clone(),
                right: Box::new(Expr::Literal(Literal(ParsedValue::Number(123.)))),
            })),
            right: Box::new(Expr::Grouping(Grouping(Box::new(Expr::Literal(Literal(
                ParsedValue::Number(45.67),
            )))))),
        });

        let mut interpreter = Interpreter::new();
        let root_env = Rc::new(Environment::new(None));
        let val = interpreter.evaluate(&expr, &root_env).unwrap();
        assert_eq!(val, RuntimeValue::Number(-123.0 * 45.67))
    }
}
