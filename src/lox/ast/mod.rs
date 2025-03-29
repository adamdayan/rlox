use std::rc::Rc;

use super::{
    environment::Environment,
    scanner::tokens::{ParsedValue, Token},
};

pub mod printer;

/// Represents a statement that has a side effect
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<'t> {
    Expression(PureExpression<'t>),
    Print(PrintExpression<'t>),
    VariableDeclaration(VariableDeclaration<'t>),
    Block(Block<'t>),
    If(If<'t>),
    While(While<'t>),
    Function(Function<'t>),
    Return(Return<'t>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrintExpression<'t>(pub Expr<'t>);

#[derive(Debug, Clone, PartialEq)]
pub struct PureExpression<'t>(pub Expr<'t>);

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration<'t> {
    pub name: &'t Token,
    pub initialiser: Option<Expr<'t>>,
}

impl<'t> VariableDeclaration<'t> {
    pub fn new(name: &'t Token, initialiser: Option<Expr<'t>>) -> Self {
        Self { name, initialiser }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block<'t> {
    pub inner: Vec<Stmt<'t>>,
}

impl<'t> Block<'t> {
    pub fn new(inner: Vec<Stmt<'t>>) -> Self {
        Self { inner }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct If<'t> {
    pub condition: Expr<'t>,
    pub then_branch: Box<Stmt<'t>>,
    pub else_branch: Option<Box<Stmt<'t>>>,
}

impl<'t> If<'t> {
    pub fn new(
        condition: Expr<'t>,
        then_branch: Box<Stmt<'t>>,
        else_branch: Option<Box<Stmt<'t>>>,
    ) -> Self {
        Self {
            condition,
            then_branch,
            else_branch,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct While<'t> {
    pub condition: Expr<'t>,
    pub body: Box<Stmt<'t>>,
}

impl<'t> While<'t> {
    pub fn new(condition: Expr<'t>, body: Box<Stmt<'t>>) -> Self {
        Self { condition, body }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function<'t> {
    pub name: &'t Token,
    pub params: Vec<&'t Token>,
    pub body: Vec<Stmt<'t>>,
}

impl<'t> Function<'t> {
    pub fn new(name: &'t Token, params: Vec<&'t Token>, body: Vec<Stmt<'t>>) -> Self {
        Self { name, params, body }
    }
}

// NOTE: check if I actually need this
/// used to distinguish between different varieties of callable
#[derive(Debug, Clone)]
pub enum CallableType {
    Function,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return<'t> {
    keyword: &'t Token,
    pub val: Option<Expr<'t>>,
}

impl<'t> Return<'t> {
    pub fn new(keyword: &'t Token, val: Option<Expr<'t>>) -> Self {
        Self { keyword, val }
    }
}

pub trait StmtVisitor<'t, T> {
    fn visit_statement(&mut self, statement: &Stmt<'t>, env: &Rc<Environment<'t>>) -> T;

    // NOTE: arguably don't need these 2 methods at all because they just take Stmt
    fn visit_expression_statement(
        &mut self,
        expression: &PureExpression<'t>,
        env: &Rc<Environment<'t>>,
    ) -> T;

    fn visit_print_statement(
        &mut self,
        print_expression: &PrintExpression<'t>,
        env: &Rc<Environment<'t>>,
    ) -> T;

    fn visit_variable_declaration(
        &mut self,
        variable_declaration: &VariableDeclaration<'t>,
        env: &Rc<Environment<'t>>,
    ) -> T;

    fn visit_block(&mut self, block: &Block<'t>, env: &Rc<Environment<'t>>) -> T;

    fn visit_if(&mut self, if_statement: &If<'t>, env: &Rc<Environment<'t>>) -> T;

    fn visit_while(&mut self, while_statement: &While<'t>, env: &Rc<Environment<'t>>) -> T;

    fn visit_function(&mut self, function_statement: &Function<'t>, env: &Rc<Environment<'t>>)
        -> T;

    fn visit_return(&mut self, return_statement: &Return<'t>, env: &Rc<Environment<'t>>) -> T;
}

/// Represents an expression that evaluates to a value
#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'t> {
    Assign(Assign<'t>),
    Logical(Logical<'t>),
    Binary(Binary<'t>),
    Unary(Unary<'t>),
    Grouping(Grouping<'t>),
    Literal(Literal<'t>),
    Variable(Variable<'t>),
    Call(Call<'t>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Logical<'t> {
    pub operator: &'t Token,
    pub left: Box<Expr<'t>>,
    pub right: Box<Expr<'t>>,
}

impl<'t> Logical<'t> {
    pub fn new(operator: &'t Token, left: Box<Expr<'t>>, right: Box<Expr<'t>>) -> Self {
        Logical {
            operator,
            left,
            right,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct Unary<'t> {
    pub operator: &'t Token,
    pub right: Box<Expr<'t>>,
}

impl<'t> Unary<'t> {
    pub fn new(operator: &'t Token, right: Box<Expr<'t>>) -> Self {
        Self { operator, right }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Grouping<'t>(pub Box<Expr<'t>>);

#[derive(Debug, Clone, PartialEq)]
pub struct Literal<'t>(pub &'t ParsedValue);

#[derive(Debug, Clone, PartialEq)]
pub struct Variable<'t> {
    pub name: &'t Token,
}
impl<'t> Variable<'t> {
    pub fn new(name: &'t Token) -> Self {
        Self { name }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call<'t> {
    pub callee: Box<Expr<'t>>,
    paren: &'t Token,
    pub arguments: Vec<Expr<'t>>,
}
impl<'t> Call<'t> {
    pub fn new(callee: Box<Expr<'t>>, paren: &'t Token, arguments: Vec<Expr<'t>>) -> Self {
        Self {
            callee,
            paren,
            arguments,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assign<'t> {
    pub name: &'t Token,
    pub value: Box<Expr<'t>>,
}
impl<'t> Assign<'t> {
    pub fn new(name: &'t Token, value: Box<Expr<'t>>) -> Self {
        Self { name, value }
    }
}

// TODO: make this Derive-able
pub trait ExprVisitor<'t, T> {
    // NOTE: would it be better to make these associated functions without &self?
    fn visit_expr(&mut self, expr: &Expr<'t>, env: &Rc<Environment<'t>>) -> T;
    fn visit_binary(&mut self, binary: &Binary<'t>, env: &Rc<Environment<'t>>) -> T;
    fn visit_unary(&mut self, unary: &Unary<'t>, env: &Rc<Environment<'t>>) -> T;
    fn visit_literal(&mut self, literal: &Literal<'t>, env: &Rc<Environment<'t>>) -> T;
    fn visit_grouping(&mut self, grouping: &Grouping<'t>, env: &Rc<Environment<'t>>) -> T;
    fn visit_variable(&mut self, variable: &Variable<'t>, env: &Rc<Environment<'t>>) -> T;
    fn visit_assign(&mut self, assign: &Assign<'t>, env: &Rc<Environment<'t>>) -> T;
    fn visit_logical(&mut self, or: &Logical<'t>, env: &Rc<Environment<'t>>) -> T;
    fn visit_call(&mut self, callee: &Call<'t>, env: &Rc<Environment<'t>>) -> T;
}
