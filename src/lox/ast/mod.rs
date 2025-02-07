use super::scanner::tokens::{LiteralValue, Token};

pub mod printer;

pub enum Expr<'a> {
    Binary(Binary<'a>),
    Unary(Unary<'a>),
    Grouping(Grouping<'a>),
    Literal(Literal),
}

pub struct Binary<'a> {
    operator: &'a Token,
    left: Box<Expr<'a>>,
    right: Box<Expr<'a>>,
}

impl<'a> Binary<'a> {
    pub fn new(operator: &'a Token, left: Box<Expr<'a>>, right: Box<Expr<'a>>) -> Self {
        Binary {
            operator,
            left,
            right,
        }
    }
}

pub struct Unary<'a> {
    operator: &'a Token,
    right: Box<Expr<'a>>,
}

impl<'a> Unary<'a> {
    pub fn new(operator: &'a Token, right: Box<Expr<'a>>) -> Self {
        Self { operator, right }
    }
}

pub struct Grouping<'a>(pub Box<Expr<'a>>);

pub struct Literal(pub LiteralValue);

// TODO: make this Derive-able
pub trait ExprVisitor<T> {
    // NOTE: would it be better to make these associated functions without &self?
    fn visit_expr(&self, expr: &Expr) -> T;
    fn visit_binary(&self, binary: &Binary) -> T;
    fn visit_unary(&self, unary: &Unary) -> T;
    fn visit_literal(&self, literal: &Literal) -> T;
    fn visit_grouping(&self, grouping: &Grouping) -> T;
}
