use crate::lox::scanner::tokens::Value;

use super::{Assign, Binary, Expr, ExprVisitor, Grouping, Literal, Unary, Variable};

pub struct Printer;
impl Printer {
    pub fn print(&mut self, expr: &Expr) -> String {
        self.visit_expr(expr)
    }
    fn parenthesize(&mut self, name: &str, expressions: Vec<&Expr>) -> String {
        let mut s = format!("({}", name);
        for expr in expressions {
            s.push_str(&format!(" {}", &self.visit_expr(expr)));
        }
        s + ")"
    }
}

impl ExprVisitor<String> for Printer {
    fn visit_expr(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::Binary(binary) => self.visit_binary(binary),
            Expr::Unary(unary) => self.visit_unary(unary),
            Expr::Literal(literal) => self.visit_literal(literal),
            Expr::Grouping(grouping) => self.visit_grouping(grouping),
            Expr::Variable(variable) => self.visit_variable(variable),
            Expr::Assign(assign) => self.visit_assign(assign),
        }
    }

    fn visit_binary(&mut self, binary: &Binary) -> String {
        self.parenthesize(&binary.operator.lexeme, vec![&binary.left, &binary.right])
    }

    fn visit_unary(&mut self, unary: &Unary) -> String {
        self.parenthesize(&unary.operator.lexeme, vec![&unary.right])
    }

    fn visit_grouping(&mut self, grouping: &Grouping) -> String {
        self.parenthesize("group", vec![&grouping.0])
    }

    fn visit_literal(&mut self, literal: &Literal) -> String {
        match &literal.0 {
            Value::String(val) => val.clone(),
            Value::Number(val) => val.to_string(),
            Value::Boolean(val) => val.to_string(),
            Value::Nil => "nil".to_owned(),
        }
    }

    fn visit_variable(&mut self, _variable: &Variable) -> String {
        todo!()
    }

    fn visit_assign(&mut self, _assign: &Assign) -> String {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use crate::lox::{
        ast::{Binary, Expr, Grouping, Literal, Unary},
        scanner::tokens::{Token, TokenType, Value},
    };

    use super::Printer;

    #[test]
    fn test_ast_printer() {
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
        let mut printer = Printer;
        let out = printer.print(&expr);
        println!("{out}");
        assert!(out == "(* (- 123) (group 45.67))")
    }
}
