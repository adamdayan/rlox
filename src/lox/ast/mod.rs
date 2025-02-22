use super::scanner::tokens::{Token, Value};

pub mod printer;

/// Represents a statement that has a side effect
#[derive(Debug, Clone)]
pub enum Stmt<'t> {
    Expression(PureExpression<'t>),
    Print(PrintExpression<'t>),
    VariableDeclaration(VariableDeclaration<'t>),
}

#[derive(Debug, Clone)]
pub struct PrintExpression<'t>(pub Expr<'t>);

#[derive(Debug, Clone)]
pub struct PureExpression<'t>(pub Expr<'t>);

#[derive(Debug, Clone)]
pub struct VariableDeclaration<'t> {
    name: &'t Token,
    initialiser: Option<Expr<'t>>,
}

impl<'t> VariableDeclaration<'t> {
    pub fn new(name: &'t Token, initialiser: Option<Expr<'t>>) -> Self {
        Self { name, initialiser }
    }
}

pub trait StmtVisitor<T> {
    fn visit_statement(&self, statement: &Stmt) -> T;
    // NOTE: arguably don't need these 2 methods at all because they just take Stmt
    fn visit_expression_statement(&self, expression: &PureExpression) -> T;
    fn visit_print_statement(&self, print_expression: &PrintExpression) -> T;
    fn visit_variable_declaration(&self, variable_declaration: &VariableDeclaration) -> T;
}

/// Represents an expression that evaluates to a value
#[derive(Debug, Clone)]
pub enum Expr<'t> {
    Binary(Binary<'t>),
    Unary(Unary<'t>),
    Grouping(Grouping<'t>),
    Literal(Literal<'t>),
    Variable(Variable<'t>),
}

#[derive(Debug, Clone)]
pub struct Binary<'t> {
    pub operator: &'t Token,
    pub left: Box<Expr<'t>>,
    pub right: Box<Expr<'t>>,
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

#[derive(Debug, Clone)]
pub struct Unary<'t> {
    pub operator: &'t Token,
    pub right: Box<Expr<'t>>,
}

impl<'t> Unary<'t> {
    pub fn new(operator: &'t Token, right: Box<Expr<'t>>) -> Self {
        Self { operator, right }
    }
}

#[derive(Debug, Clone)]
pub struct Grouping<'t>(pub Box<Expr<'t>>);

#[derive(Debug, Clone)]
pub struct Literal<'t>(pub &'t Value);

#[derive(Debug, Clone)]
pub struct Variable<'t> {
    pub name: &'t Token,
}
impl<'t> Variable<'t> {
    pub fn new(name: &'t Token) -> Self {
        Self { name }
    }
}

// TODO: make this Derive-able
pub trait ExprVisitor<T> {
    // NOTE: would it be better to make these associated functions without &self?
    fn visit_expr(&self, expr: &Expr) -> T;
    fn visit_binary(&self, binary: &Binary) -> T;
    fn visit_unary(&self, unary: &Unary) -> T;
    fn visit_literal(&self, literal: &Literal) -> T;
    fn visit_grouping(&self, grouping: &Grouping) -> T;
    fn visit_variable(&self, variable: &Variable) -> T;
}
