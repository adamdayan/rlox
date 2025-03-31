use std::collections::HashMap;
use std::hash::{Hash, Hasher};

use super::interpreter::RuntimeError;
use super::{
    ast::{
        Assign, Binary, Block, Call, Expr, Function, Grouping, If, Literal, Logical,
        PrintExpression, PureExpression, Return, Stmt, StmtVisitor, Unary, Variable,
        VariableDeclaration, While,
    },
    interpreter::Interpreter,
    scanner::tokens::Token,
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ResolutionError {
    #[error("No scopes on stack")]
    NoScopes,
    #[error("{0} declared but not yet defined")]
    UndefinedVariable(String),
}

/// holds resolvable items and a bool representing whether they've been defined (i.e are ready to
/// use)
struct Scope(HashMap<String, bool>);

impl Scope {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
}

pub struct Resolver<'i, 't, 'r> {
    interpreter: &'i mut Interpreter<'t, 'r>,
    scopes: Vec<Scope>,
}

impl<'i, 't, 'r> Resolver<'i, 't, 'r>
where
    'i: 'r,
{
    pub fn resolve(&mut self, statements: &'r [Stmt<'t>]) -> Result<(), ResolutionError> {
        for statement in statements {
            self.resolve_statement(statement)?
        }
        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(Scope::new())
    }

    fn end_scope(&mut self) -> Result<(), ResolutionError> {
        match self.scopes.pop() {
            Some(_) => Ok(()),
            None => Err(ResolutionError::NoScopes),
        }
    }

    fn declare(&mut self, name: &Token) {
        if let Some(cur_scope) = self.scopes.last_mut() {
            // if a resolvable has only been declared and not defined it is not yet ready to be used
            cur_scope.0.insert(name.lexeme.clone(), false);
        }
    }

    fn define(&mut self, name: &'i Token) {
        if let Some(cur_scope) = self.scopes.last_mut() {
            // after definition, the resolvable is ready to use
            cur_scope.0.insert(name.lexeme.clone(), true);
        }
    }

    fn resolve_statement(&mut self, statement: &'i Stmt<'t>) -> Result<(), ResolutionError> {
        match statement {
            Stmt::VariableDeclaration(decl) => self.resolve_var_declaration(decl),
            Stmt::While(while_stmt) => self.resolve_while(while_stmt),
            Stmt::Expression(expr) => self.resolve_expression_statement(expr),
            Stmt::Block(block) => self.resolve_block(block),
            Stmt::If(if_stmt) => self.resolve_if_statement(if_stmt),
            Stmt::Print(print) => self.resolve_print(print),
            Stmt::Return(ret) => self.resolve_return(ret),
            Stmt::Function(func) => self.resolve_function_statement(func),
        }
    }

    fn resolve_block(&mut self, block: &'i Block<'t>) -> Result<(), ResolutionError> {
        self.begin_scope();
        self.resolve(&block.inner)?;
        self.end_scope()?;
        Ok(())
    }

    fn resolve_var_declaration(
        &mut self,
        decl: &'i VariableDeclaration<'t>,
    ) -> Result<(), ResolutionError> {
        self.declare(decl.name);
        if let Some(init) = decl.initialiser.as_ref() {
            self.resolve_expression(init)?;
        }
        self.define(decl.name);
        Ok(())
    }

    fn resolve_function_statement(
        &mut self,
        function: &'i Function<'t>,
    ) -> Result<(), ResolutionError> {
        self.declare(function.name);
        self.define(function.name);
        self.resolve_function(function)
    }

    fn resolve_function(&mut self, function: &'i Function<'t>) -> Result<(), ResolutionError> {
        self.begin_scope();
        for param in function.params.iter() {
            self.declare(param);
            self.define(param);
        }
        self.resolve(&function.body)?;
        self.end_scope()
    }

    fn resolve_expression_statement(
        &mut self,
        expr: &'i PureExpression<'t>,
    ) -> Result<(), ResolutionError> {
        self.resolve_expression(&expr.0)
    }

    fn resolve_if_statement(&mut self, if_statement: &'i If<'t>) -> Result<(), ResolutionError> {
        self.resolve_expression(&if_statement.condition)?;
        self.resolve_statement(&if_statement.then_branch)?;
        if let Some(else_branch) = &if_statement.else_branch {
            self.resolve_statement(else_branch)?;
        }
        Ok(())
    }

    fn resolve_print(&mut self, print: &'i PrintExpression<'t>) -> Result<(), ResolutionError> {
        self.resolve_expression(&print.0)?;
        Ok(())
    }

    fn resolve_return(&mut self, return_statement: &'i Return<'t>) -> Result<(), ResolutionError> {
        if let Some(ret) = &return_statement.val {
            self.resolve_expression(ret)?;
        }
        Ok(())
    }

    fn resolve_while(&mut self, while_statement: &'i While<'t>) -> Result<(), ResolutionError> {
        self.resolve_expression(&while_statement.condition)?;
        self.resolve_statement(&while_statement.body)?;
        Ok(())
    }

    fn resolve_expression(&mut self, expression: &'i Expr<'t>) -> Result<(), ResolutionError> {
        match expression {
            Expr::Variable(var) => self.resolve_variable(var),
            Expr::Assign(assign) => self.resolve_assign(assign),
            Expr::Logical(logical) => self.resolve_logical(logical),
            Expr::Unary(unary) => self.resolve_unary(unary),
            Expr::Grouping(grouping) => self.resolve_grouping(grouping),
            Expr::Literal(literal) => self.resolve_literal(literal),
            Expr::Call(call) => self.resolve_call(call),
            Expr::Binary(binary) => self.resolve_binary(binary),
        }
    }

    fn resolve_variable(&mut self, var: &'i Variable<'t>) -> Result<(), ResolutionError> {
        if let Some(cur_scope) = self.scopes.last() {
            if let Some(is_var_ready) = cur_scope.0.get(&var.name.lexeme) {
                // prevent a variable referring to itself (or a shadowed var of the same name) in
                // its definition
                if *is_var_ready {
                    return Err(ResolutionError::UndefinedVariable(var.name.lexeme.clone()));
                }
            }
        }
        // TODO: work out how to pass Expr without taking an Expr; or the equivalent
        self.resolve_local(Resolvable::Variable(var))
    }

    /// resolves a resolvable and stores the number of scope hops required to find it in the
    /// Interpreter
    fn resolve_local(&mut self, resolvable: Resolvable<'t, 'r>) -> Result<(), ResolutionError> {
        // find the number of scope hops to the variable
        for (idx, _) in self.scopes.iter().rev().enumerate() {
            return Ok(self
                .interpreter
                .resolve(resolvable, self.scopes.len() - (idx + 1)));
        }
        // if we don't find the variable in a scope, we assume it's a global
        Ok(())
    }

    fn resolve_assign(&mut self, assign: &'i Assign<'t>) -> Result<(), ResolutionError> {
        self.resolve_expression(&assign.value)?;
        self.resolve_local(Resolvable::Assign(assign))
    }

    fn resolve_binary(&mut self, binary: &'i Binary<'t>) -> Result<(), ResolutionError> {
        self.resolve_expression(&binary.left)?;
        self.resolve_expression(&binary.right)?;
        Ok(())
    }

    fn resolve_call(&mut self, call: &'i Call<'t>) -> Result<(), ResolutionError> {
        self.resolve_expression(&call.callee)?;
        for arg in call.arguments.iter() {
            self.resolve_expression(arg)?;
        }
        Ok(())
    }

    fn resolve_grouping(&mut self, grouping: &'i Grouping<'t>) -> Result<(), ResolutionError> {
        self.resolve_expression(&grouping.0)?;
        Ok(())
    }

    fn resolve_literal(&mut self, _: &'i Literal<'t>) -> Result<(), ResolutionError> {
        Ok(())
    }

    fn resolve_logical(&mut self, logical: &'i Logical<'t>) -> Result<(), ResolutionError> {
        self.resolve_expression(&logical.left)?;
        self.resolve_expression(&logical.right)?;
        Ok(())
    }

    fn resolve_unary(&mut self, unary: &'i Unary<'t>) -> Result<(), ResolutionError> {
        self.resolve_expression(&unary.right)?;
        Ok(())
    }
}

/// wraps Expr inner types that are resolvable
#[derive(Debug, Clone)]
pub enum Resolvable<'t, 'r> {
    Variable(&'r Variable<'t>),
    Assign(&'r Assign<'t>),
    // Function(&'r Function<'t>),
}

// needed for use in HashMap
impl<'t, 'r> PartialEq for Resolvable<'t, 'r> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Resolvable::Variable(our_var), Resolvable::Variable(other_var)) => {
                our_var.name == other_var.name
            }
            (Resolvable::Assign(our_assign), Resolvable::Assign(other_assign)) => {
                our_assign.name == other_assign.name
            }
            // (Resolvable::Function(our_func), Resolvable::Function(other_func)) => {
            //     our_func.name == other_func.name
            // }
            _ => false,
        }
    }
}

impl<'t, 'r> TryInto<Resolvable<'t, 'r>> for &'r Expr<'t> {
    type Error = RuntimeError<'t>;

    fn try_into(self) -> Result<Resolvable<'t, 'r>, RuntimeError<'t>> {
        match self {
            Expr::Variable(var) => Ok(Resolvable::Variable(&var)),
            Expr::Assign(assign) => Ok(Resolvable::Assign(&assign)),
            // Expr::Function(func) => Ok(Resolvable::Function(&func)),
            expr => Err(RuntimeError::UnresolvableExpression(expr.clone())),
        }
    }
}

impl<'t, 'r> Eq for Resolvable<'t, 'r> {}

impl<'t, 'r> Hash for Resolvable<'t, 'r> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Resolvable::Variable(var) => var.name.hash(state),
            Resolvable::Assign(assign) => assign.name.hash(state),
            // Resolvable::Function(func) => func.name.hash(state),
        }
    }
}
