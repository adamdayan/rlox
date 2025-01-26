use crate::lox::scanner::tokens::LiteralValue;

use super::{Binary, Expr, Grouping, Literal, Unary, Visitor};

pub struct Printer;
impl Printer {
    pub fn print(&self, expr: &Expr) -> String {
        self.visit_expr(expr)
    }
    fn parenthesize(&self, name: &str, expressions: Vec<&Expr>) -> String {
        let mut s = String::from(format!("({}", name));
        for expr in expressions {
            s.push_str(&format!(" {}", &self.visit_expr(expr)));
        }
        s + ")"
    }
}

impl Visitor<String> for Printer {
    fn visit_expr(&self, expr: &Expr) -> String {
        match expr {
            Expr::Binary(binary) => self.visit_binary(binary),
            Expr::Unary(unary) => self.visit_unary(unary),
            Expr::Literal(literal) => self.visit_literal(literal),
            Expr::Grouping(grouping) => self.visit_grouping(grouping),
        }
    }

    fn visit_binary(&self, binary: &Binary) -> String {
        self.parenthesize(&binary.operator.lexeme, vec![&binary.left, &binary.right])
    }

    fn visit_unary(&self, unary: &Unary) -> String {
        self.parenthesize(&unary.operator.lexeme, vec![&unary.right])
    }

    fn visit_grouping(&self, grouping: &Grouping) -> String {
        self.parenthesize("group", vec![&grouping.0])
    }

    fn visit_literal(&self, literal: &Literal) -> String {
        match &literal.0 {
            LiteralValue::String(val) => val.clone(),
            LiteralValue::Number(val) => val.to_string(),
            LiteralValue::Nil => "nil".to_owned(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lox::{
        ast::{Binary, Expr, Grouping, Literal, Unary},
        scanner::tokens::{LiteralValue, Token, TokenType},
    };

    use super::Printer;

    #[test]
    fn test_ast_printer() {
        let expr = Expr::Binary(Binary {
            operator: Token::new(TokenType::Star, "*".to_owned(), None, 0),
            left: Box::new(Expr::Unary(Unary {
                operator: Token::new(TokenType::Minus, "-".to_owned(), None, 0),
                right: Box::new(Expr::Literal(Literal(LiteralValue::Number(123.)))),
            })),
            right: Box::new(Expr::Grouping(Grouping(Box::new(Expr::Literal(Literal(
                LiteralValue::Number(45.67),
            )))))),
        });
        let printer = Printer;
        let out = printer.print(&expr);
        println!("{out}");
        assert!(out == "(* (- 123) (group 45.67))")
    }
}
