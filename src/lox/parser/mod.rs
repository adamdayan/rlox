use std::{collections::HashSet, rc::Rc};
use thiserror::Error;

use super::{
    ast::{
        Assign, Binary, Block, Call, Class, Expr, Function, Grouping, If, Literal, Logical,
        PrintExpression, PureExpression, Return, Stmt, Unary, Variable, VariableDeclaration, While,
    },
    scanner::tokens::{ParsedValue, Token, TokenType},
};

const MAX_ARG_COUNT: usize = 255;

// TODO: add line numbers in all of these?
#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Tried to access idx {0} in array of len {0}")]
    OutOfBounds(usize, usize),
    #[error("Token stream prematurely terminated")]
    PrematureTermination,
    #[error("Missing literal value in a literal")]
    MissingLiteral,
    #[error("Invalid token type: {0:?}. Expected one of {1:?}")]
    InvalidTokenType(TokenType, Vec<TokenType>),
    #[error("Invalid assignment target: {0}")]
    InvalidAssignmentTarget(Rc<Token>),
    #[error("{0} greater than max argument count {MAX_ARG_COUNT}")]
    TooManyArguments(usize),
    #[error("Invalid Stmt type: {0:?}")]
    InvalidStmtType(String),
}

pub struct Parser<'t> {
    current: usize,
    // TODO: make tokens an iterator rather than a slice
    tokens: &'t [Rc<Token>],
}

// lifetime constraint enforces that the Tokens live longer than the Parser
impl<'t: 't, 'p> Parser<'t> {
    pub fn new(tokens: &'t [Rc<Token>]) -> Self {
        Parser { current: 0, tokens }
    }

    /// parses a [`Token`] stream into a list of [`Stmt`]s
    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            statements.push(self.declaration()?);
        }
        Ok(statements)
    }

    /// advance our current position in the token stream by 1
    fn advance(&'p mut self) {
        self.current += 1;
    }

    /// get the next token without advancing
    fn peek(&'p self) -> Option<Rc<Token>> {
        self.tokens.get(self.current).cloned()
    }

    /// get the previous token
    fn previous(&'p self) -> Result<Rc<Token>, ParseError> {
        Ok(self.tokens[self.current - 1].clone())
    }

    // check if current token is of target_token_type
    fn check(&'p self, target_token_type: TokenType) -> bool {
        if let Some(tok) = self.peek() {
            return tok.token_type == target_token_type;
        }
        false
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len() - 1
    }

    /// if the next token is in targets, advance and return true otherwise return false
    fn match_token(&'p mut self, targets: HashSet<TokenType>) -> bool {
        if let Some(peeked) = self.peek() {
            if targets.contains(&peeked.token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    /// parse an expression
    fn expression(&'p mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    /// consume the current token if it's of the correct type and return it
    fn consume(&'p mut self, token_type: TokenType) -> Result<Rc<Token>, ParseError> {
        if !self.match_token(HashSet::from([token_type.clone()])) {
            match self.peek() {
                Some(invalid_token) => {
                    return Err(ParseError::InvalidTokenType(
                        invalid_token.token_type.clone(),
                        vec![token_type],
                    ))
                }
                None => return Err(ParseError::OutOfBounds(self.current, self.tokens.len())),
            }
        }
        self.previous()
    }

    /// parse an assignment.
    fn assignment(&'p mut self) -> Result<Expr, ParseError> {
        let expr = self.or()?;

        // use lookahead for an "=" token to determine if this is an assignment
        if self.match_token(HashSet::from([TokenType::Equal])) {
            // NOTE: is this the equals sign or the var that we are setting??
            let equals = self.previous()?;
            // assignment is right-associative so recursively call assignment
            let val = self.assignment()?;

            // only variables are valid assignment targets
            if let Expr::Variable(var) = expr {
                return Ok(Expr::Assign(Assign::new(var.name, val)));
            } else {
                return Err(ParseError::InvalidAssignmentTarget(equals.clone()));
            }
        }
        Ok(expr)
    }

    /// parse an or statement or anything of higher precedence
    fn or(&'p mut self) -> Result<Expr, ParseError> {
        let mut expr = self.and()?;

        while self.match_token(HashSet::from([TokenType::Or])) {
            let operator = self.previous()?;
            let right = self.and()?;
            expr = Expr::Logical(Logical::new(operator, expr, right));
        }
        Ok(expr)
    }

    /// parse an or statement or anything of higher precedence
    fn and(&'p mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;

        while self.match_token(HashSet::from([TokenType::Or])) {
            let operator = self.previous()?;
            let right = self.equality()?;
            expr = Expr::Logical(Logical::new(operator, expr, right));
        }
        Ok(expr)
    }

    /// parse an equality or anything of higher precedence
    fn equality(&'p mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;
        while self.match_token(HashSet::from([TokenType::EqualEqual, TokenType::BangEqual])) {
            let operator = self.previous()?;
            let other = self.comparison()?;
            expr = Expr::Binary(Binary::new(operator, expr, other));
        }
        Ok(expr)
    }

    /// parse a comparison or anything of higher precedence
    fn comparison(&'p mut self) -> Result<Expr, ParseError> {
        let mut expr = self.term()?;

        while self.match_token(HashSet::from([
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ])) {
            let operator = self.previous()?;
            let other = self.term()?;
            expr = Expr::Binary(Binary::new(operator, expr, other));
        }
        Ok(expr)
    }

    /// parse a term or anything of higher precedence
    fn term(&'p mut self) -> Result<Expr, ParseError> {
        let mut expr = self.factor()?;

        while self.match_token(HashSet::from([TokenType::Plus, TokenType::Minus])) {
            let operator = self.previous()?;
            let other = self.factor()?;
            expr = Expr::Binary(Binary::new(operator, expr, other));
        }
        Ok(expr)
    }

    /// parse a factor or anything of higher precedence
    fn factor(&'p mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;

        while self.match_token(HashSet::from([TokenType::Slash, TokenType::Star])) {
            let operator = self.previous()?;
            let other = self.unary()?;
            expr = Expr::Binary(Binary::new(operator, expr, other));
        }
        Ok(expr)
    }

    /// parse a unary expression or anything of higher precedence
    fn unary(&'p mut self) -> Result<Expr, ParseError> {
        if self.match_token(HashSet::from([
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Bang,
        ])) {
            let operator = self.previous()?;
            let primary = self.primary()?;
            return Ok(Expr::Unary(Unary::new(operator, primary)));
        }
        self.call()
    }

    fn call(&'p mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;

        // we don't just match on LeftParen in the while because later we may need to handle other
        // call types
        loop {
            if self.match_token(HashSet::from([TokenType::LeftParen])) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }

    /// gather up a function call's arguments and combine it with the callee into a Call
    fn finish_call(&'p mut self, callee: Expr) -> Result<Expr, ParseError> {
        let mut arguments = vec![];
        if !self.check(TokenType::RightParen) {
            // do while equivalent
            loop {
                if arguments.len() >= MAX_ARG_COUNT {
                    return Err(ParseError::TooManyArguments(arguments.len()));
                }
                arguments.push(self.expression()?);
                if !self.match_token(HashSet::from([TokenType::Comma])) {
                    break;
                }
            }
        }
        let paren = self.consume(TokenType::RightParen)?;
        Ok(Expr::Call(Call::new(callee, paren, arguments)))
    }

    /// parse a primary expression
    fn primary(&'p mut self) -> Result<Expr, ParseError> {
        let tok = self.peek().ok_or(ParseError::PrematureTermination)?;
        match &tok.token_type {
            TokenType::String
            | TokenType::Number
            | TokenType::True
            | TokenType::False
            | TokenType::Nil => {
                self.current += 1;
                Ok(Expr::Literal(Literal(
                    tok.literal.clone().ok_or(ParseError::MissingLiteral)?,
                )))
            }
            TokenType::LeftParen => {
                let expr = self.expression()?;
                self.consume(TokenType::RightParen)?;
                Ok(Expr::Grouping(Grouping::new(expr)))
            }
            TokenType::Identifier => {
                self.current += 1;
                Ok(Expr::Variable(Variable::new(tok)))
            }
            tok_type => Err(ParseError::InvalidTokenType(
                tok_type.clone(),
                vec![
                    TokenType::String,
                    TokenType::Number,
                    TokenType::True,
                    TokenType::False,
                    TokenType::Nil,
                    TokenType::LeftParen,
                    TokenType::Identifier,
                ],
            )),
        }
    }

    /// parses a statement or anything of higher precedence
    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if self.match_token(HashSet::from([TokenType::For])) {
            return self.for_statement();
        } else if self.match_token(HashSet::from([TokenType::If])) {
            return self.if_statement();
        } else if self.match_token(HashSet::from([TokenType::While])) {
            return self.while_statement();
        } else if self.match_token(HashSet::from([TokenType::Print])) {
            return self.print_statement();
        } else if self.match_token(HashSet::from([TokenType::Return])) {
            return self.return_statement();
        } else if self.match_token(HashSet::from([TokenType::LeftBrace])) {
            return self.block();
        }
        self.expression_statement()
    }

    fn return_statement(&mut self) -> Result<Stmt, ParseError> {
        let keyword = self.previous()?;
        let val = if self.check(TokenType::Semicolon) {
            None
        } else {
            Some(self.expression()?)
        };
        self.consume(TokenType::Semicolon)?;
        Ok(Stmt::Return(Return::new(keyword, val)))
    }

    /// parses a Block or anything of higher precedence
    fn block(&mut self) -> Result<Stmt, ParseError> {
        let mut inner_statements = Vec::new();
        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            inner_statements.push(self.declaration()?);
        }
        self.consume(TokenType::RightBrace)?;
        Ok(Stmt::Block(Block::new(inner_statements)))
    }

    /// parses a while statement
    fn while_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(TokenType::LeftParen)?;
        let cond = self.expression()?;
        self.consume(TokenType::RightParen)?;
        let body = self.statement()?;

        Ok(Stmt::While(While::new(cond, body)))
    }

    /// for statement and desugar it into a while loop
    fn for_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(TokenType::LeftParen)?;

        // parse initialiser
        let initialiser = if self.match_token(HashSet::from([TokenType::Semicolon])) {
            None
        } else if self.match_token(HashSet::from([TokenType::Var])) {
            Some(self.variable_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        // parse condition
        let condition = if self.check(TokenType::Semicolon) {
            None
        } else {
            Some(self.expression()?)
        };
        self.consume(TokenType::Semicolon)?;

        // parse the post body action
        let post = if self.check(TokenType::RightParen) {
            None
        } else {
            Some(self.expression()?)
        };
        self.consume(TokenType::RightParen)?;

        let mut body = self.statement()?;

        // desugar into while loop

        // if we have a post statement, create a block of the bodyy then the post
        if let Some(post) = post {
            body = Stmt::Block(Block::new(vec![
                body,
                Stmt::Expression(PureExpression(post)),
            ]));
        };

        // if there is a condition, use it as a While condition
        let while_statement = if let Some(cond) = condition {
            Stmt::While(While::new(cond, body))
        } else {
            // otherwise, cheat by adding a condition that always evaluates to true
            Stmt::While(While::new(
                Expr::Literal(Literal(ParsedValue::Boolean(true))),
                body,
            ))
        };

        // if there is an initialiser, prepend it to the while statement
        let var_while = if let Some(initialiser) = initialiser {
            Stmt::Block(Block::new(vec![initialiser, while_statement]))
        } else {
            while_statement
        };

        Ok(var_while)
    }

    /// parses a print statement
    fn print_statement(&mut self) -> Result<Stmt, ParseError> {
        let value = self.expression()?;
        self.consume(TokenType::Semicolon)?;
        Ok(Stmt::Print(PrintExpression(value)))
    }

    /// parses an expression statement
    fn expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon)?;
        Ok(Stmt::Expression(PureExpression(expr)))
    }

    /// parse an if statement
    fn if_statement(&mut self) -> Result<Stmt, ParseError> {
        let _ = self.consume(TokenType::LeftParen)?;

        let condition = self.expression()?;
        let _ = self.consume(TokenType::RightParen)?;
        let then_branch = self.statement()?;
        let else_branch = if self.match_token(HashSet::from([TokenType::Else])) {
            Some(self.statement()?)
        } else {
            None
        };
        Ok(Stmt::If(If::new(condition, then_branch, else_branch)))
    }

    /// parses a declaration or anything of higher precedence
    fn declaration(&mut self) -> Result<Stmt, ParseError> {
        if self.match_token(HashSet::from([TokenType::Var])) {
            self.variable_declaration()
        } else if self.match_token(HashSet::from([TokenType::Fun])) {
            self.function()
        } else if self.match_token(HashSet::from([TokenType::Class])) {
            self.class_declaration()
        } else {
            self.statement()
        }
        // TODO: need to add synchronise() logic here
    }

    fn class_declaration(&mut self) -> Result<Stmt, ParseError> {
        let name = self.consume(TokenType::Identifier)?;
        self.consume(TokenType::LeftBrace)?;
        let mut methods = vec![];
        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            methods.push(self.function()?);
        }

        self.consume(TokenType::RightBrace)?;

        Ok(Stmt::Class(Class::new(name, methods)))
    }

    fn function(&mut self) -> Result<Stmt, ParseError> {
        let name = self.consume(TokenType::Identifier)?;
        self.consume(TokenType::LeftParen)?;
        let mut params = vec![];
        if !self.check(TokenType::RightParen) {
            loop {
                if params.len() >= MAX_ARG_COUNT {
                    return Err(ParseError::TooManyArguments(params.len()));
                }
                params.push(self.consume(TokenType::Identifier)?);
                if !self.match_token(HashSet::from([TokenType::Comma])) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen)?;
        self.consume(TokenType::LeftBrace)?;
        let body = match self.block()? {
            Stmt::Block(block) => block.inner,
            other => return Err(ParseError::InvalidStmtType(format!("{:?}", other))),
        };
        Ok(Stmt::Function(Function::new(name, params, body)))
    }

    /// parses a variable declaration
    fn variable_declaration(&mut self) -> Result<Stmt, ParseError> {
        let name = self.consume(TokenType::Identifier)?;
        let initialiser = if self.match_token(HashSet::from([TokenType::Equal])) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(TokenType::Semicolon)?;

        Ok(Stmt::VariableDeclaration(VariableDeclaration::new(
            name,
            initialiser,
        )))
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::lox::scanner::tokens::ParsedValue;

    use super::*;

    #[test]
    fn test_parser() {
        let toks = vec![
            Rc::new(Token::new(
                TokenType::Number,
                "3".to_string(),
                Some(ParsedValue::Number(3.0)),
                0,
                0,
            )),
            Rc::new(Token::new(TokenType::Minus, "-".to_string(), None, 0, 0)),
            Rc::new(Token::new(
                TokenType::Number,
                "2".to_string(),
                Some(ParsedValue::Number(2.0)),
                0,
                0,
            )),
            Rc::new(Token::new(
                TokenType::Semicolon,
                ";".to_string(),
                None,
                0,
                0,
            )),
        ];

        let mut parser = Parser::new(&toks);
        let _ast = parser.parse().unwrap();
        // TODO: fix AST Printer and this test
        // let printer = Printer;

        // let out = printer.print(&ast);
        // println!("{out}");
        // assert_eq!(out, "(- 3 2)");
    }
}
