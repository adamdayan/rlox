use std::{cell::RefCell, rc::Rc};

use crate::lox::ast::{Expr, ExprVisitor};

use super::{
    ast::{
        Assign, Binary, Block, Grouping, If, Literal, Logical, PrintExpression, PureExpression,
        Stmt, StmtVisitor, Unary, Variable, VariableDeclaration,
    },
    environment::Environment,
    scanner::tokens::{Token, TokenType, Value},
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("Invalid operator: {0}. Line {1}", operator.lexeme, operator.line)]
    InvalidOperator { operator: Token },
    #[error("Invalid operand: {:?}. Line {}", val, operator.line)]
    InvalidOperand { operator: Token, val: Value },
    #[error("Invalid operands: {:?} {:?}. Line {}", left_val, right_val, operator.line)]
    InvalidOperands {
        operator: Token,
        left_val: Value,
        right_val: Value,
    },
    #[error("Undefined variable: {0}", name)]
    UndefinedVariable { name: String },
}

pub struct Interpreter {
    had_runtime_error: bool,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            had_runtime_error: false,
        }
    }

    pub fn interpret(&mut self, statements: &[Stmt]) -> Result<(), RuntimeError> {
        let root_env = Rc::new(RefCell::new(Environment::new(None)));
        for statement in statements {
            self.execute(statement, &root_env)?;
        }

        Ok(())
    }

    fn execute(
        &mut self,
        statement: &Stmt,
        env: &Rc<RefCell<Environment>>,
    ) -> Result<(), RuntimeError> {
        self.visit_statement(statement, env)
    }

    fn evaluate(
        &mut self,
        expr: &Expr,
        env: &Rc<RefCell<Environment>>,
    ) -> Result<Value, RuntimeError> {
        self.visit_expr(expr, env)
    }

    fn is_truthy(&self, val: &Value) -> bool {
        match val {
            Value::Nil => false,
            Value::Boolean(val) => *val,
            _ => true,
        }
    }
}

/// Visitor pattern that evaluates expressions
impl ExprVisitor<Result<Value, RuntimeError>> for Interpreter {
    fn visit_expr(
        &mut self,
        expr: &Expr,
        env: &Rc<RefCell<Environment>>,
    ) -> Result<Value, RuntimeError> {
        match expr {
            Expr::Binary(binary) => self.visit_binary(binary, env),
            Expr::Unary(unary) => self.visit_unary(unary, env),
            Expr::Grouping(grouping) => self.visit_grouping(grouping, env),
            Expr::Literal(literal) => self.visit_literal(literal, env),
            Expr::Variable(variable) => self.visit_variable(variable, env),
            Expr::Assign(assign) => self.visit_assign(assign, env),
            Expr::Logical(logic) => self.visit_logical(logic, env),
        }
    }

    fn visit_literal(
        &mut self,
        literal: &Literal,
        env: &Rc<RefCell<Environment>>,
    ) -> Result<Value, RuntimeError> {
        // NOTE: clone seems very wasteful in the string case. But I think Value (not &Valye) is required because I could end up
        // constructing new Values at runtime e.g. concat, adding etc?
        Ok(literal.0.clone())
    }

    fn visit_grouping(
        &mut self,
        grouping: &Grouping,
        env: &Rc<RefCell<Environment>>,
    ) -> Result<Value, RuntimeError> {
        self.visit_expr(&grouping.0, env)
    }

    fn visit_unary(
        &mut self,
        unary: &Unary,
        env: &Rc<RefCell<Environment>>,
    ) -> Result<Value, RuntimeError> {
        let inner_value = self.visit_expr(&unary.right, env)?;
        match (&unary.operator.token_type, inner_value) {
            (TokenType::Minus, Value::Number(num)) => Ok(Value::Number(-num)),
            // "-" operator with non-Number inner_value is invalid
            (TokenType::Minus, val) => Err(RuntimeError::InvalidOperand {
                operator: unary.operator.clone(),
                val,
            }),
            (TokenType::Bang, val) => Ok(Value::Boolean(!self.is_truthy(&val))),
            // no other operator types are valid for a unary expression
            (_, _) => Err(RuntimeError::InvalidOperator {
                operator: unary.operator.clone(),
            }),
        }
    }

    fn visit_binary(
        &mut self,
        binary: &Binary,
        env: &Rc<RefCell<Environment>>,
    ) -> Result<Value, RuntimeError> {
        let left_value = self.visit_expr(&binary.left, env)?;
        let right_value = self.visit_expr(&binary.right, env)?;

        match &binary.operator.token_type {
            // subtraction
            TokenType::Minus => match (&left_value, &right_value) {
                (Value::Number(left_num), Value::Number(right_num)) => {
                    Ok(Value::Number(left_num - right_num))
                }
                (_, _) => Err(RuntimeError::InvalidOperands {
                    operator: binary.operator.clone(),
                    left_val: left_value,
                    right_val: right_value,
                }),
            },
            // division
            TokenType::Slash => match (&left_value, &right_value) {
                (Value::Number(left_num), Value::Number(right_num)) => {
                    Ok(Value::Number(left_num / right_num))
                }
                (_, _) => Err(RuntimeError::InvalidOperands {
                    operator: binary.operator.clone(),
                    left_val: left_value,
                    right_val: right_value,
                }),
            },
            // multiplication
            TokenType::Star => match (&left_value, &right_value) {
                (Value::Number(left_num), Value::Number(right_num)) => {
                    Ok(Value::Number(left_num * right_num))
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
                (Value::Number(left_num), Value::Number(right_num)) => {
                    Ok(Value::Number(left_num + right_num))
                }
                // concatenation
                (Value::String(left_str), Value::String(right_str)) => {
                    Ok(Value::String(left_str.to_owned() + right_str))
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
        variable: &Variable,
        env: &Rc<RefCell<Environment>>,
    ) -> Result<Value, RuntimeError> {
        env.borrow().get(&variable.name.lexeme)
    }

    fn visit_assign(
        &mut self,
        assign: &Assign,
        env: &Rc<RefCell<Environment>>,
    ) -> Result<Value, RuntimeError> {
        let val = self.evaluate(&assign.value, env)?;
        env.borrow_mut().assign(&assign.name.lexeme, val.clone())?;
        Ok(val)
    }

    fn visit_logical(
        &mut self,
        logical: &Logical,
        env: &Rc<RefCell<Environment>>,
    ) -> Result<Value, RuntimeError> {
        let left = self.evaluate(&logical.left, env)?;
        // check Or's shortcircuit
        if logical.operator.token_type == TokenType::Or {
            if self.is_truthy(&left) {
                return Ok(left);
            }
        } else {
            // check And's shortcircuit
            if !self.is_truthy(&left) {
                return Ok(left);
            }
        }
        self.evaluate(&logical.right, env)
    }
}

impl StmtVisitor<Result<(), RuntimeError>> for Interpreter {
    fn visit_statement(
        &mut self,
        statement: &Stmt,
        env: &Rc<RefCell<Environment>>,
    ) -> Result<(), RuntimeError> {
        match statement {
            Stmt::Expression(expr) => self.visit_expression_statement(expr, env),
            Stmt::Print(value) => self.visit_print_statement(value, env),
            Stmt::VariableDeclaration(decl) => self.visit_variable_declaration(decl, env),
            Stmt::Block(block) => self.visit_block(block, env),
            Stmt::If(if_statement) => self.visit_if(if_statement, env),
        }
    }

    fn visit_expression_statement(
        &mut self,
        expression: &PureExpression,
        env: &Rc<RefCell<Environment>>,
    ) -> Result<(), RuntimeError> {
        let _value = self.evaluate(&expression.0, env)?;
        Ok(())
    }

    fn visit_print_statement(
        &mut self,
        print_expression: &PrintExpression,
        env: &Rc<RefCell<Environment>>,
    ) -> Result<(), RuntimeError> {
        let value = self.evaluate(&print_expression.0, env)?;
        println!("{value}");
        Ok(())
    }

    fn visit_variable_declaration(
        &mut self,
        variable_declaration: &VariableDeclaration,
        env: &Rc<RefCell<Environment>>,
    ) -> Result<(), RuntimeError> {
        let val = match &variable_declaration.initialiser {
            None => Value::Nil,
            Some(init) => self.evaluate(init, env)?,
        };
        env
        .borrow_mut()
        .define(variable_declaration.name.lexeme.clone(), val);
        Ok(())
    }

    fn visit_block(
        &mut self,
        block: &Block,
        env: &Rc<RefCell<Environment>>,
    ) -> Result<(), RuntimeError> {
        let block_env = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));
        for stmt in &block.inner {
            {
                self.visit_statement(stmt, &block_env)?;
            }
        }
        Ok(())
    }

    fn visit_if(
        &mut self,
        if_statement: &If,
        env: &Rc<RefCell<Environment>>,
    ) -> Result<(), RuntimeError> {
        let cond = self.evaluate(&if_statement.condition, env)?;
        // execute the right branch
        if self.is_truthy(&cond) {
            self.visit_statement(&if_statement.then_branch, env)?;
        } else {
            // we can only execute the else statement if there is one
            if let Some(else_branch) = &if_statement.else_branch {
                self.visit_statement(else_branch, env)?;
            }
        }
        Ok(())
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
                right: Box::new(Expr::Literal(Literal(&Value::Number(123.)))),
            })),
            right: Box::new(Expr::Grouping(Grouping(Box::new(Expr::Literal(Literal(
                &Value::Number(45.67),
            )))))),
        });

        let mut interpreter = Interpreter::new();
        let root_env = Rc::new(RefCell::new(Environment::new(None)));
        let val = interpreter.evaluate(&expr, &root_env).unwrap();
        assert_eq!(val, Value::Number(-123.0 * 45.67))
    }
}
