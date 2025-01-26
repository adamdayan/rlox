use super::scanner::tokens::{LiteralValue, Token};

pub mod printer;

pub enum Expr {
    Binary(Binary),
    Unary(Unary),
    Grouping(Grouping),
    Literal(Literal),
}
pub struct Binary {
    operator: Token,
    left: Box<Expr>,
    right: Box<Expr>,
}

pub struct Unary {
    operator: Token,
    right: Box<Expr>,
}

pub struct Grouping(Box<Expr>);

pub struct Literal(LiteralValue);

pub trait Visitor<T> {
    // NOTE: would it be better to make these associated functions without &self?
    fn visit_expr(&self, expr: &Expr) -> T;
    fn visit_binary(&self, binary: &Binary) -> T;
    fn visit_unary(&self, unary: &Unary) -> T;
    fn visit_literal(&self, literal: &Literal) -> T;
    fn visit_grouping(&self, grouping: &Grouping) -> T;
}
