use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use super::ast::{Class, FunctionType, Get, Set, This};
use super::interpreter::RuntimeError;
use super::{
    ast::{
        Assign, Binary, Block, Call, Expr, Function, Grouping, If, Literal, Logical,
        PrintExpression, PureExpression, Return, Stmt, Unary, Variable, VariableDeclaration, While,
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
    // TODO: add token in here to get line num
    #[error("Can't return from top-level code")]
    ReturnOutOfFunction,
    #[error("Can't return value from initialiser")]
    ReturnInInit,
    #[error("Class can't inherit from itself")]
    CyclicalInheritance,
    #[error("Superclass name must be a Variable")]
    BadSuperType,
}

/// holds resolvable items and a bool representing whether they've been defined (i.e are ready to
/// use)
struct Scope(HashMap<String, bool>);

impl Scope {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
}

pub struct Resolver {
    scopes: Vec<Scope>,
    current_function: FunctionType,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            scopes: vec![],
            current_function: FunctionType::None,
        }
    }

    pub fn resolve(
        &mut self,
        statements: &[Stmt],
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolutionError> {
        for statement in statements {
            self.resolve_statement(statement, interpreter)?
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

    fn declare(&mut self, name: Rc<Token>) {
        if let Some(cur_scope) = self.scopes.last_mut() {
            // if a resolvable has only been declared and not defined it is not yet ready to be used
            cur_scope.0.insert(name.lexeme.clone(), false);
        }
    }

    fn define(&mut self, name: Rc<Token>) {
        if let Some(cur_scope) = self.scopes.last_mut() {
            // after definition, the resolvable is ready to use
            cur_scope.0.insert(name.lexeme.clone(), true);
        }
    }

    fn resolve_statement(
        &mut self,
        statement: &Stmt,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolutionError> {
        match statement {
            Stmt::VariableDeclaration(decl) => self.resolve_var_declaration(decl, interpreter),
            Stmt::While(while_stmt) => self.resolve_while(while_stmt, interpreter),
            Stmt::Expression(expr) => self.resolve_expression_statement(expr, interpreter),
            Stmt::Block(block) => self.resolve_block(block, interpreter),
            Stmt::If(if_stmt) => self.resolve_if_statement(if_stmt, interpreter),
            Stmt::Print(print) => self.resolve_print(print, interpreter),
            Stmt::Return(ret) => self.resolve_return(ret, interpreter),
            Stmt::Function(func) => self.resolve_function_statement(func, interpreter),
            Stmt::Class(class) => self.resolve_class_statement(class, interpreter),
        }
    }

    fn resolve_block(
        &mut self,
        block: &Block,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolutionError> {
        self.begin_scope();
        self.resolve(&block.inner, interpreter)?;
        self.end_scope()?;
        Ok(())
    }

    fn resolve_var_declaration(
        &mut self,
        decl: &VariableDeclaration,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolutionError> {
        self.declare(decl.name.clone());
        if let Some(init) = decl.initialiser.as_ref() {
            self.resolve_expression(init, interpreter)?;
        }
        self.define(decl.name.clone());
        Ok(())
    }

    fn resolve_function_statement(
        &mut self,
        function: &Function,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolutionError> {
        self.declare(function.name.clone());
        self.define(function.name.clone());
        self.resolve_function(function, FunctionType::Function, interpreter)
    }

    fn resolve_function(
        &mut self,
        function: &Function,
        func_type: FunctionType,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolutionError> {
        let enclosing_func = self.current_function;
        self.current_function = func_type;
        self.begin_scope();
        for param in function.params.iter() {
            self.declare(param.clone());
            self.define(param.clone());
        }
        self.resolve(&function.body, interpreter)?;
        self.end_scope()?;
        self.current_function = enclosing_func;
        Ok(())
    }

    fn resolve_expression_statement(
        &mut self,
        expr: &PureExpression,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolutionError> {
        self.resolve_expression(&expr.0, interpreter)
    }

    fn resolve_if_statement(
        &mut self,
        if_statement: &If,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolutionError> {
        self.resolve_expression(&if_statement.condition, interpreter)?;
        self.resolve_statement(&if_statement.then_branch, interpreter)?;
        if let Some(else_branch) = &if_statement.else_branch {
            self.resolve_statement(else_branch, interpreter)?;
        }
        Ok(())
    }

    fn resolve_print(
        &mut self,
        print: &PrintExpression,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolutionError> {
        self.resolve_expression(&print.0, interpreter)?;
        Ok(())
    }

    fn resolve_return(
        &mut self,
        return_statement: &Return,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolutionError> {
        if self.current_function == FunctionType::None {
            return Err(ResolutionError::ReturnOutOfFunction);
        }
        if let Some(ret) = &return_statement.val {
            if self.current_function == FunctionType::Initialiser {
                return Err(ResolutionError::ReturnInInit);
            }
            self.resolve_expression(ret, interpreter)?;
        }
        Ok(())
    }

    fn resolve_while(
        &mut self,
        while_statement: &While,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolutionError> {
        self.resolve_expression(&while_statement.condition, interpreter)?;
        self.resolve_statement(&while_statement.body, interpreter)?;
        Ok(())
    }

    fn resolve_expression(
        &mut self,
        expression: &Expr,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolutionError> {
        match expression {
            Expr::Variable(var) => self.resolve_variable(var, interpreter),
            Expr::Assign(assign) => self.resolve_assign(assign, interpreter),
            Expr::Logical(logical) => self.resolve_logical(logical, interpreter),
            Expr::Unary(unary) => self.resolve_unary(unary, interpreter),
            Expr::Grouping(grouping) => self.resolve_grouping(grouping, interpreter),
            Expr::Literal(literal) => self.resolve_literal(literal, interpreter),
            Expr::Call(call) => self.resolve_call(call, interpreter),
            Expr::Binary(binary) => self.resolve_binary(binary, interpreter),
            Expr::Get(get) => self.resolve_get(get, interpreter),
            Expr::Set(set) => self.resolve_set(set, interpreter),
            Expr::This(this) => self.resolve_this(this, interpreter),
        }
    }

    fn resolve_variable(
        &mut self,
        var: &Variable,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolutionError> {
        if let Some(cur_scope) = self.scopes.last() {
            if let Some(is_var_ready) = cur_scope.0.get(&var.name.lexeme) {
                // prevent a variable referring to itself (or a shadowed var of the same name) in
                // its definition
                if !*is_var_ready {
                    return Err(ResolutionError::UndefinedVariable(var.name.lexeme.clone()));
                }
            }
        }
        // TODO: work out how to pass Expr without taking an Expr; or the equivalent
        self.resolve_local(Resolvable::Variable(var.clone()), interpreter)
    }

    /// resolves a resolvable and stores the number of scope hops required to find it in the
    /// Interpreter
    fn resolve_local(
        &mut self,
        resolvable: Resolvable,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolutionError> {
        // find the number of scope hops to the variable
        for (idx, scope) in self.scopes.iter().rev().enumerate() {
            if scope.0.contains_key(&resolvable.get_name()) {
                interpreter.resolve(resolvable, self.scopes.len() - (idx + 1));
                return Ok(());
            };
        }
        // if we don't find the variable in a scope, we assume it's a global
        Ok(())
    }

    fn resolve_assign(
        &mut self,
        assign: &Assign,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolutionError> {
        self.resolve_expression(&assign.value, interpreter)?;
        self.resolve_local(Resolvable::Assign(assign.clone()), interpreter)
    }

    fn resolve_get(
        &mut self,
        get: &Get,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolutionError> {
        // only need to resolve the object, not the property name because any name can be set
        self.resolve_expression(&get.object, interpreter)?;
        Ok(())
    }

    fn resolve_set(
        &mut self,
        set: &Set,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolutionError> {
        // only need to resolve the object and the value, not the property name because any name can be set
        self.resolve_expression(&set.object, interpreter)?;
        self.resolve_expression(&set.val, interpreter)?;
        Ok(())
    }

    fn resolve_this(
        &mut self,
        this: &This,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolutionError> {
        self.resolve_local(Resolvable::This(this.clone()), interpreter)
    }

    fn resolve_binary(
        &mut self,
        binary: &Binary,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolutionError> {
        self.resolve_expression(&binary.left, interpreter)?;
        self.resolve_expression(&binary.right, interpreter)?;
        Ok(())
    }

    fn resolve_class_statement(
        &mut self,
        class: &Class,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolutionError> {
        self.declare(class.name.clone());
        self.define(class.name.clone());
        if let Some(superclass) = &class.superclass {
            if let Expr::Variable(superclass_var) = superclass {
                if superclass_var.name.lexeme == class.name.lexeme {
                    return Err(ResolutionError::CyclicalInheritance);
                }
                self.resolve_variable(superclass_var, interpreter)?;
            } else {
                return Err(ResolutionError::BadSuperType);
            }
        }

        // insert "this" in class scope
        self.begin_scope();
        self.scopes
            .last_mut()
            .expect("must have a scope")
            .0
            .insert("this".to_owned(), true);

        for method in class.methods.iter() {
            self.resolve_function(
                method,
                if method.name.lexeme == "init" {
                    FunctionType::Initialiser
                } else {
                    FunctionType::Method
                },
                interpreter,
            )?;
        }
        let _ = self.end_scope();

        Ok(())
    }

    fn resolve_call(
        &mut self,
        call: &Call,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolutionError> {
        self.resolve_expression(&call.callee, interpreter)?;
        for arg in call.arguments.iter() {
            self.resolve_expression(arg, interpreter)?;
        }
        Ok(())
    }

    fn resolve_grouping(
        &mut self,
        grouping: &Grouping,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolutionError> {
        self.resolve_expression(&grouping.0, interpreter)?;
        Ok(())
    }

    fn resolve_literal(
        &mut self,
        _: &Literal,
        _interpreter: &mut Interpreter,
    ) -> Result<(), ResolutionError> {
        Ok(())
    }

    fn resolve_logical(
        &mut self,
        logical: &Logical,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolutionError> {
        self.resolve_expression(&logical.left, interpreter)?;
        self.resolve_expression(&logical.right, interpreter)?;
        Ok(())
    }

    fn resolve_unary(
        &mut self,
        unary: &Unary,
        interpreter: &mut Interpreter,
    ) -> Result<(), ResolutionError> {
        self.resolve_expression(&unary.right, interpreter)?;
        Ok(())
    }
}

/// wraps Expr inner types that are resolvable
#[derive(Debug, Clone)]
pub enum Resolvable {
    // should really be Rc<Variable>/Rc<Assign> but cba to change all the function signatures
    Variable(Variable),
    Assign(Assign),
    This(This),
}

impl Resolvable {
    pub fn get_name(&self) -> String {
        match self {
            Self::Variable(var) => var.name.lexeme.clone(),
            Self::Assign(assign) => assign.name.lexeme.clone(),
            Self::This(this) => this.0.lexeme.clone(),
        }
    }
}

// needed for use in HashMap
impl PartialEq for Resolvable {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Resolvable::Variable(our_var), Resolvable::Variable(other_var)) => {
                our_var.name == other_var.name
            }
            (Resolvable::Assign(our_assign), Resolvable::Assign(other_assign)) => {
                our_assign.name == other_assign.name
            }
            (Resolvable::This(our_this), Resolvable::This(their_this)) => {
                our_this.0 == their_this.0
            }
            _ => false,
        }
    }
}

impl TryInto<Resolvable> for Expr {
    type Error = RuntimeError;

    fn try_into(self) -> Result<Resolvable, RuntimeError> {
        match self {
            Expr::Variable(var) => Ok(Resolvable::Variable(var)),
            Expr::Assign(assign) => Ok(Resolvable::Assign(assign)),
            expr => Err(RuntimeError::UnresolvableExpression(expr.clone())),
        }
    }
}

impl Eq for Resolvable {}

impl Hash for Resolvable {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Resolvable::Variable(var) => var.name.hash(state),
            Resolvable::Assign(assign) => assign.name.hash(state),
            Resolvable::This(this) => this.0.hash(state),
        }
    }
}
