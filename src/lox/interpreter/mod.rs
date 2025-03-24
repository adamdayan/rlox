use std::{cell::RefCell, rc::Rc};

use crate::lox::ast::{Expr, ExprVisitor};

use super::{
    ast::{
        Assign, Binary, Block, Call, Function, Grouping, If, Literal, Logical, PrintExpression,
        PureExpression, Return, Stmt, StmtVisitor, Unary, Variable, VariableDeclaration, While,
    },
    callable::Callable,
    environment::Environment,
    scanner::tokens::{ParsedValue, Token, TokenType},
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum RuntimeError<'t> {
    #[error("Invalid operator: {0}. Line {1}", operator.lexeme, operator.line)]
    InvalidOperator { operator: Token },
    #[error("Invalid operand: {:?}. Line {}", val, operator.line)]
    InvalidOperand {
        operator: Token,
        val: RuntimeValue<'t>,
    },
    #[error("Invalid operands: {:?} {:?}. Line {}", left_val, right_val, operator.line)]
    InvalidOperands {
        operator: Token,
        left_val: RuntimeValue<'t>,
        right_val: RuntimeValue<'t>,
    },
    #[error("Undefined variable: {0}", name)]
    UndefinedVariable { name: String },
    #[error("Cannot call non-callable value: {:?}", value)]
    CallNonCallable { value: RuntimeValue<'t> },
    #[error("Provided {0} arguments for functoin with arity {1}")]
    WrongArgsNum(usize, usize),
    // TODO: replace this HORRIBLE hack with std::core::ops::ControlFlow once its stabilised
    /// Used as a hack to extract return values from deep in the call stack
    #[error("NOT A REAL ERROR")]
    Return(RuntimeValue<'t>),
}

/// Values computed at runtime.
#[derive(Clone, Debug, PartialEq)]
pub enum RuntimeValue<'t> {
    Boolean(bool),
    String(String),
    Number(f32),
    Callable(Callable<'t>),
    Nil,
}

impl From<ParsedValue> for RuntimeValue<'_> {
    fn from(val: ParsedValue) -> Self {
        match val {
            ParsedValue::Boolean(v) => RuntimeValue::Boolean(v),
            ParsedValue::String(v) => RuntimeValue::String(v),
            ParsedValue::Number(v) => RuntimeValue::Number(v),
            ParsedValue::Nil => RuntimeValue::Nil,
        }
    }
}

pub struct Interpreter {
    had_runtime_error: bool,
}

impl<'t> Interpreter {
    pub fn new() -> Self {
        Self {
            had_runtime_error: false,
        }
    }

    pub fn interpret(&mut self, statements: &[Stmt<'t>]) -> Result<(), RuntimeError<'t>> {
        let root_env = Rc::new(RefCell::new(Environment::new(None)));

        // define native functions in the global environment
        // add clock() method
        root_env.borrow_mut().define(
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

        for statement in statements {
            self.execute(statement, &root_env)?;
        }

        Ok(())
    }

    fn execute(
        &mut self,
        statement: &Stmt<'t>,
        env: &Rc<RefCell<Environment<'t>>>,
    ) -> Result<(), RuntimeError<'t>> {
        self.visit_statement(statement, env)
    }

    pub fn execute_block(
        &mut self,
        statements: &[Stmt<'t>],
        env: &Rc<RefCell<Environment<'t>>>,
    ) -> Result<(), RuntimeError<'t>> {
        for stmt in statements {
            {
                self.visit_statement(stmt, env)?;
            }
        }
        Ok(())
    }

    fn evaluate(
        &mut self,
        expr: &Expr<'t>,
        env: &Rc<RefCell<Environment<'t>>>,
    ) -> Result<RuntimeValue<'t>, RuntimeError<'t>> {
        self.visit_expr(expr, env)
    }
}

fn is_truthy(val: &RuntimeValue) -> bool {
    match val {
        RuntimeValue::Nil => false,
        RuntimeValue::Boolean(val) => *val,
        _ => true,
    }
}

/// Visitor pattern that evaluates expressions
impl<'t> ExprVisitor<'t, Result<RuntimeValue<'t>, RuntimeError<'t>>> for Interpreter {
    fn visit_expr(
        &mut self,
        expr: &Expr<'t>,
        env: &Rc<RefCell<Environment<'t>>>,
    ) -> Result<RuntimeValue<'t>, RuntimeError<'t>> {
        match expr {
            Expr::Binary(binary) => self.visit_binary(binary, env),
            Expr::Unary(unary) => self.visit_unary(unary, env),
            Expr::Grouping(grouping) => self.visit_grouping(grouping, env),
            Expr::Literal(literal) => self.visit_literal(literal, env),
            Expr::Variable(variable) => self.visit_variable(variable, env),
            Expr::Assign(assign) => self.visit_assign(assign, env),
            Expr::Logical(logic) => self.visit_logical(logic, env),
            Expr::Call(call) => self.visit_call(call, env),
        }
    }

    fn visit_literal(
        &mut self,
        literal: &Literal<'t>,
        _env: &Rc<RefCell<Environment<'t>>>,
    ) -> Result<RuntimeValue<'t>, RuntimeError<'t>> {
        // NOTE: clone seems very wasteful in the string case. But I think Value (not &Valye) is required because I could end up
        // constructing new Values at runtime e.g. concat, adding etc?
        Ok(literal.0.clone().into())
    }

    fn visit_grouping(
        &mut self,
        grouping: &Grouping<'t>,
        env: &Rc<RefCell<Environment<'t>>>,
    ) -> Result<RuntimeValue<'t>, RuntimeError<'t>> {
        self.visit_expr(&grouping.0, env)
    }

    fn visit_unary(
        &mut self,
        unary: &Unary<'t>,
        env: &Rc<RefCell<Environment<'t>>>,
    ) -> Result<RuntimeValue<'t>, RuntimeError<'t>> {
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
        binary: &Binary<'t>,
        env: &Rc<RefCell<Environment<'t>>>,
    ) -> Result<RuntimeValue<'t>, RuntimeError<'t>> {
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
                (RuntimeValue::String(left_str), RuntimeValue::String(right_str)) => {
                    Ok(RuntimeValue::String(left_str.to_owned() + right_str))
                }
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
        variable: &Variable<'t>,
        env: &Rc<RefCell<Environment<'t>>>,
    ) -> Result<RuntimeValue<'t>, RuntimeError<'t>> {
        env.borrow().get(&variable.name.lexeme)
    }

    fn visit_assign(
        &mut self,
        assign: &Assign<'t>,
        env: &Rc<RefCell<Environment<'t>>>,
    ) -> Result<RuntimeValue<'t>, RuntimeError<'t>> {
        let val = self.evaluate(&assign.value, env)?;
        env.borrow_mut().assign(&assign.name.lexeme, val.clone())?;
        Ok(val)
    }

    fn visit_logical(
        &mut self,
        logical: &Logical<'t>,
        env: &Rc<RefCell<Environment<'t>>>,
    ) -> Result<RuntimeValue<'t>, RuntimeError<'t>> {
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
        call: &Call<'t>,
        env: &Rc<RefCell<Environment<'t>>>,
    ) -> Result<RuntimeValue<'t>, RuntimeError<'t>> {
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
}

impl<'t> StmtVisitor<'t, Result<(), RuntimeError<'t>>> for Interpreter {
    fn visit_statement(
        &mut self,
        statement: &Stmt<'t>,
        env: &Rc<RefCell<Environment<'t>>>,
    ) -> Result<(), RuntimeError<'t>> {
        match statement {
            Stmt::Expression(expr) => self.visit_expression_statement(expr, env),
            Stmt::Print(value) => self.visit_print_statement(value, env),
            Stmt::VariableDeclaration(decl) => self.visit_variable_declaration(decl, env),
            Stmt::Block(block) => self.visit_block(block, env),
            Stmt::If(if_statement) => self.visit_if(if_statement, env),
            Stmt::While(while_statement) => self.visit_while(while_statement, env),
            Stmt::Function(function_statement) => self.visit_function(function_statement, env),
            Stmt::Return(return_statement) => self.visit_return(return_statement, env),
        }
    }

    fn visit_expression_statement(
        &mut self,
        expression: &PureExpression<'t>,
        env: &Rc<RefCell<Environment<'t>>>,
    ) -> Result<(), RuntimeError<'t>> {
        let _value = self.evaluate(&expression.0, env)?;
        Ok(())
    }

    fn visit_print_statement(
        &mut self,
        print_expression: &PrintExpression<'t>,
        env: &Rc<RefCell<Environment<'t>>>,
    ) -> Result<(), RuntimeError<'t>> {
        let value = self.evaluate(&print_expression.0, env)?;
        println!("{:?}", value);
        Ok(())
    }

    fn visit_variable_declaration(
        &mut self,
        variable_declaration: &VariableDeclaration<'t>,
        env: &Rc<RefCell<Environment<'t>>>,
    ) -> Result<(), RuntimeError<'t>> {
        let val = match &variable_declaration.initialiser {
            None => RuntimeValue::Nil,
            Some(init) => self.evaluate(init, env)?,
        };
        env.borrow_mut()
            .define(variable_declaration.name.lexeme.clone(), val);
        Ok(())
    }

    fn visit_block(
        &mut self,
        block: &Block<'t>,
        env: &Rc<RefCell<Environment<'t>>>,
    ) -> Result<(), RuntimeError<'t>> {
        let block_env = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));
        self.execute_block(&block.inner, env)
    }

    fn visit_if(
        &mut self,
        if_statement: &If<'t>,
        env: &Rc<RefCell<Environment<'t>>>,
    ) -> Result<(), RuntimeError<'t>> {
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
        while_statement: &While<'t>,
        env: &Rc<RefCell<Environment<'t>>>,
    ) -> Result<(), RuntimeError<'t>> {
        while is_truthy(&(self.evaluate(&while_statement.condition, env)?)) {
            self.visit_statement(&while_statement.body, env)?;
        }
        Ok(())
    }

    fn visit_function(
        &mut self,
        function_statement: &Function<'t>,
        env: &Rc<RefCell<Environment<'t>>>,
    ) -> Result<(), RuntimeError<'t>> {
        let func = Callable::Function {
            // NOTE: do I really need to clone here()
            decl: function_statement.clone(),
            // functions "close-over" the environment in which they're declared
            closure: env.clone(),
        };
        env.borrow_mut().define(
            function_statement.name.lexeme.clone(),
            RuntimeValue::Callable(func),
        );
        Ok(())
    }

    fn visit_return(
        &mut self,
        return_statement: &Return<'t>,
        env: &Rc<RefCell<Environment<'t>>>,
    ) -> Result<(), RuntimeError<'t>> {
        let val = if let Some(expr) = &return_statement.val {
            self.evaluate(&expr, env)?
        } else {
            RuntimeValue::Nil
        };
        // we use the HORRIBLE hack of a fake Return error to more easily return through the call
        // stack. Will replace with std::ops::core::ControlFlow as soon as its stabilised
        Err(RuntimeError::Return(val))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_interpreter() {
        let operator = Token::new(TokenType::Star, "*".to_owned(), None, 0);
        let inner_operator = Token::new(TokenType::Minus, "-".to_owned(), None, 0);
        let expr = Expr::Binary(Binary {
            operator: &operator,
            left: Box::new(Expr::Unary(Unary {
                operator: &inner_operator,
                right: Box::new(Expr::Literal(Literal(&ParsedValue::Number(123.)))),
            })),
            right: Box::new(Expr::Grouping(Grouping(Box::new(Expr::Literal(Literal(
                &ParsedValue::Number(45.67),
            )))))),
        });

        let mut interpreter = Interpreter::new();
        let root_env = Rc::new(RefCell::new(Environment::new(None)));
        let val = interpreter.evaluate(&expr, &root_env).unwrap();
        assert_eq!(val, RuntimeValue::Number(-123.0 * 45.67))
    }
}
