use std::rc::Rc;

use super::{
    environment::Environment,
    scanner::tokens::{ParsedValue, Token},
};

pub mod printer;

/// Represents a statement that has a side effect
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expression(PureExpression),
    Print(PrintExpression),
    VariableDeclaration(VariableDeclaration),
    Block(Block),
    Class(Class),
    If(If),
    While(While),
    Function(Function),
    Return(Return),
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrintExpression(pub Expr);

#[derive(Debug, Clone, PartialEq)]
pub struct PureExpression(pub Expr);

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub name: Rc<Token>,
    pub initialiser: Option<Expr>,
}

impl VariableDeclaration {
    pub fn new(name: Rc<Token>, initialiser: Option<Expr>) -> Self {
        Self { name, initialiser }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub inner: Vec<Stmt>,
}

impl Block {
    pub fn new(inner: Vec<Stmt>) -> Self {
        Self { inner }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub condition: Expr,
    pub then_branch: Box<Stmt>,
    pub else_branch: Option<Box<Stmt>>,
}

impl If {
    pub fn new(condition: Expr, then_branch: Stmt, else_branch: Option<Stmt>) -> Self {
        Self {
            condition,
            then_branch: Box::new(then_branch),
            else_branch: else_branch.map(Box::new),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct While {
    pub condition: Expr,
    pub body: Box<Stmt>,
}

impl While {
    pub fn new(condition: Expr, body: Stmt) -> Self {
        Self {
            condition,
            body: Box::new(body),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Rc<Token>,
    pub params: Vec<Rc<Token>>,
    pub body: Vec<Stmt>,
}

impl Function {
    pub fn new(name: Rc<Token>, params: Vec<Rc<Token>>, body: Vec<Stmt>) -> Self {
        Self { name, params, body }
    }
}

// NOTE: check if I actually need this
/// used to distinguish between different varieties of callable
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FunctionType {
    Function,
    Method,
    Initialiser,
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    keyword: Rc<Token>,
    pub val: Option<Expr>,
}

impl Return {
    pub fn new(keyword: Rc<Token>, val: Option<Expr>) -> Self {
        Self { keyword, val }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub name: Rc<Token>,
    pub methods: Vec<Function>,
    pub superclass: Option<Expr>,
}

impl Class {
    pub fn new(name: Rc<Token>, methods: Vec<Function>, superclass: Option<Expr>) -> Self {
        Self {
            name,
            methods,
            superclass,
        }
    }
}

pub trait StmtVisitor<T> {
    fn visit_statement(&mut self, statement: &Stmt, env: &Rc<Environment>) -> T;

    // NOTE: arguably don't need these 2 methods at all because they just take Stmt
    fn visit_expression_statement(
        &mut self,
        expression: &PureExpression,
        env: &Rc<Environment>,
    ) -> T;

    fn visit_print_statement(
        &mut self,
        print_expression: &PrintExpression,
        env: &Rc<Environment>,
    ) -> T;

    fn visit_variable_declaration(
        &mut self,
        variable_declaration: &VariableDeclaration,
        env: &Rc<Environment>,
    ) -> T;

    fn visit_block(&mut self, block: &Block, env: &Rc<Environment>) -> T;

    fn visit_if(&mut self, if_statement: &If, env: &Rc<Environment>) -> T;

    fn visit_while(&mut self, while_statement: &While, env: &Rc<Environment>) -> T;

    fn visit_function(&mut self, function_statement: &Function, env: &Rc<Environment>) -> T;

    fn visit_return(&mut self, return_statement: &Return, env: &Rc<Environment>) -> T;
    fn visit_class(&mut self, class: &Class, env: &Rc<Environment>) -> T;
}

/// Represents an expression that evaluates to a value
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Assign(Assign),
    Logical(Logical),
    Binary(Binary),
    Unary(Unary),
    Grouping(Grouping),
    Literal(Literal),
    Variable(Variable),
    Call(Call),
    Get(Get),
    Set(Set),
    This(This),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Logical {
    pub operator: Rc<Token>,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

impl Logical {
    pub fn new(operator: Rc<Token>, left: Expr, right: Expr) -> Self {
        Logical {
            operator,
            left: Box::new(left),
            right: Box::new(right),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub operator: Rc<Token>,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

impl Binary {
    pub fn new(operator: Rc<Token>, left: Expr, right: Expr) -> Self {
        Binary {
            operator,
            left: Box::new(left),
            right: Box::new(right),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub operator: Rc<Token>,
    pub right: Box<Expr>,
}

impl Unary {
    pub fn new(operator: Rc<Token>, right: Expr) -> Self {
        Self {
            operator,
            right: Box::new(right),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Grouping(pub Box<Expr>);

impl Grouping {
    pub fn new(expr: Expr) -> Self {
        Self(Box::new(expr))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal(pub ParsedValue);

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Variable {
    pub name: Rc<Token>,
}
impl Variable {
    pub fn new(name: Rc<Token>) -> Self {
        Self { name }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub callee: Box<Expr>,
    paren: Rc<Token>,
    pub arguments: Vec<Expr>,
}
impl Call {
    pub fn new(callee: Expr, paren: Rc<Token>, arguments: Vec<Expr>) -> Self {
        Self {
            callee: Box::new(callee),
            paren,
            arguments,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    pub name: Rc<Token>,
    pub value: Box<Expr>,
}
impl Assign {
    pub fn new(name: Rc<Token>, value: Expr) -> Self {
        Self {
            name,
            value: Box::new(value),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Get {
    pub object: Box<Expr>,
    pub name: Rc<Token>,
}

impl Get {
    pub fn new(object: Expr, name: Rc<Token>) -> Self {
        Self {
            object: Box::new(object),
            name,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Set {
    pub object: Box<Expr>,
    pub name: Rc<Token>,
    pub val: Box<Expr>,
}

impl Set {
    pub fn new(object: Box<Expr>, name: Rc<Token>, val: Expr) -> Self {
        Self {
            object,
            name,
            val: Box::new(val),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct This(pub Rc<Token>);

// TODO: make this Derive-able
pub trait ExprVisitor<T> {
    // NOTE: would it be better to make these associated functions without &self?
    fn visit_expr(&mut self, expr: &Expr, env: &Rc<Environment>) -> T;
    fn visit_binary(&mut self, binary: &Binary, env: &Rc<Environment>) -> T;
    fn visit_unary(&mut self, unary: &Unary, env: &Rc<Environment>) -> T;
    fn visit_literal(&mut self, literal: &Literal, env: &Rc<Environment>) -> T;
    fn visit_grouping(&mut self, grouping: &Grouping, env: &Rc<Environment>) -> T;
    fn visit_variable(&mut self, variable: &Variable, env: &Rc<Environment>) -> T;
    fn visit_assign(&mut self, assign: &Assign, env: &Rc<Environment>) -> T;
    fn visit_logical(&mut self, or: &Logical, env: &Rc<Environment>) -> T;
    fn visit_call(&mut self, callee: &Call, env: &Rc<Environment>) -> T;
    fn visit_get(&mut self, get: &Get, env: &Rc<Environment>) -> T;
    fn visit_set(&mut self, set: &Set, env: &Rc<Environment>) -> T;
    fn visit_this(&mut self, this: &This, env: &Rc<Environment>) -> T;
}
