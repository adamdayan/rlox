use super::scanner::tokens::{LiteralValue, Token};

pub mod printer;

pub enum Expr<'t> {
    Binary(Binary<'t>),
    Unary(Unary<'t>),
    Grouping(Grouping<'t>),
    Literal(Literal<'t>),
}

pub struct Binary<'t> {
    operator: &'t Token,
    left: Box<Expr<'t>>,
    right: Box<Expr<'t>>,
}

impl<'t> Binary<'t> {
    pub fn new(operator: &'t Token, left: Box<Expr<'t>>, right: Box<Expr<'t>>) -> Self {
        Binary {
            operator,
            left,
            right,
        }
    }
}

pub struct Unary<'t> {
    operator: &'t Token,
    right: Box<Expr<'t>>,
}

impl<'t> Unary<'t> {
    pub fn new(operator: &'t Token, right: Box<Expr<'t>>) -> Self {
        Self { operator, right }
    }
}

pub struct Grouping<'t>(pub Box<Expr<'t>>);

pub struct Literal<'t>(pub &'t LiteralValue);

// TODO: make this Derive-able
pub trait ExprVisitor<T> {
    // NOTE: would it be better to make these associated functions without &self?
    fn visit_expr(&self, expr: &Expr) -> T;
    fn visit_binary(&self, binary: &Binary) -> T;
    fn visit_unary(&self, unary: &Unary) -> T;
    fn visit_literal(&self, literal: &Literal) -> T;
    fn visit_grouping(&self, grouping: &Grouping) -> T;
}
