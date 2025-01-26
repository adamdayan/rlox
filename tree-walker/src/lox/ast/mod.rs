use super::scanner::tokens::{LiteralValue, Token};

pub enum Expr {
    Binary(Binary),
    Unary(Unary),
    Grouping(Grouping),
    Literal(LiteralValue),
}
pub struct Binary {
    operator: Token,
    left: Box<Expr>,
    right: Box<Expr>,
}

pub struct Unary {
    operator: Token,
    left: Box<Expr>,
}

pub struct Grouping(Box<Expr>);

pub struct Literal(LiteralValue);

mod visit {
    use super::*;

    pub trait Visitor<T> {
        fn visit_binary(binary: Binary) -> T;
        fn visit_expr(expr: Expr) -> T;
        fn visit_unary(unary: Unary) -> T;
        fn visit_literal(literal: Literal) -> T;
    }
}
