use crate::lox::ast::{Expr, ExprVisitor};

use super::{
    ast::{
        Binary, Grouping, Literal, PrintExpression, PureExpression, Stmt, StmtVisitor, Unary,
        Variable, VariableDeclaration,
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
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            had_runtime_error: false,
            environment: Environment::new(),
        }
    }

    pub fn interpret(&mut self, statements: &[Stmt]) -> Result<(), RuntimeError> {
        for statement in statements {
            self.execute(statement)?;
        }

        Ok(())
    }

    fn execute(&mut self, statement: &Stmt) -> Result<(), RuntimeError> {
        self.visit_statement(statement)
    }

    fn evaluate(&self, expr: &Expr) -> Result<Value, RuntimeError> {
        self.visit_expr(expr)
    }

    fn is_truthy(&self, val: Value) -> bool {
        match val {
            Value::Nil => false,
            Value::Boolean(val) => val,
            _ => true,
        }
    }
}

/// Visitor pattern that evaluates expressions
impl ExprVisitor<Result<Value, RuntimeError>> for Interpreter {
    fn visit_expr(&self, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::Binary(binary) => self.visit_binary(binary),
            Expr::Unary(unary) => self.visit_unary(unary),
            Expr::Grouping(grouping) => self.visit_grouping(grouping),
            Expr::Literal(literal) => self.visit_literal(literal),
            Expr::Variable(variable) => self.visit_variable(variable),
        }
    }

    fn visit_literal(&self, literal: &Literal) -> Result<Value, RuntimeError> {
        // NOTE: clone seems very wasteful in the string case. But I think Value (not &Valye) is required because I could end up
        // constructing new Values at runtime e.g. concat, adding etc?
        Ok(literal.0.clone())
    }

    fn visit_grouping(&self, grouping: &Grouping) -> Result<Value, RuntimeError> {
        self.visit_expr(&grouping.0)
    }

    fn visit_unary(&self, unary: &Unary) -> Result<Value, RuntimeError> {
        let inner_value = self.visit_expr(&unary.right)?;
        match (&unary.operator.token_type, inner_value) {
            (TokenType::Minus, Value::Number(num)) => Ok(Value::Number(-num)),
            // "-" operator with non-Number inner_value is invalid
            (TokenType::Minus, val) => Err(RuntimeError::InvalidOperand {
                operator: unary.operator.clone(),
                val,
            }),
            (TokenType::Bang, val) => Ok(Value::Boolean(!self.is_truthy(val))),
            // no other operator types are valid for a unary expression
            (_, _) => Err(RuntimeError::InvalidOperator {
                operator: unary.operator.clone(),
            }),
        }
    }

    fn visit_binary(&self, binary: &Binary) -> Result<Value, RuntimeError> {
        let left_value = self.visit_expr(&binary.left)?;
        let right_value = self.visit_expr(&binary.right)?;

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

    fn visit_variable(&self, variable: &Variable) -> Result<Value, RuntimeError> {
        self.environment.get(&variable.name.lexeme)
    }
}

impl StmtVisitor<Result<(), RuntimeError>> for Interpreter {
    fn visit_statement(&mut self, statement: &Stmt) -> Result<(), RuntimeError> {
        match statement {
            Stmt::Expression(expr) => self.visit_expression_statement(expr),
            Stmt::Print(value) => self.visit_print_statement(value),
            Stmt::VariableDeclaration(decl) => self.visit_variable_declaration(decl),
        }
    }

    fn visit_expression_statement(&self, expression: &PureExpression) -> Result<(), RuntimeError> {
        let _value = self.evaluate(&expression.0)?;
        Ok(())
    }

    fn visit_print_statement(
        &self,
        print_expression: &PrintExpression,
    ) -> Result<(), RuntimeError> {
        let value = self.evaluate(&print_expression.0)?;
        println!("{value}");
        Ok(())
    }

    fn visit_variable_declaration(
        &mut self,
        variable_declaration: &VariableDeclaration,
    ) -> Result<(), RuntimeError> {
        Ok(self.environment.define(
            variable_declaration.name.lexeme.clone(),
            match &variable_declaration.initialiser {
                None => Value::Nil,
                Some(init) => self.evaluate(init)?,
            },
        ))
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

        let interpreter = Interpreter::new();
        let val = interpreter.evaluate(&expr).unwrap();
        assert_eq!(val, Value::Number(-123.0 * 45.67))
    }
}
