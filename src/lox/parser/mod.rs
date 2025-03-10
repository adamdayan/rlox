use std::collections::HashSet;
use thiserror::Error;

use super::{
    ast::{
        Assign, Binary, Block, Expr, If, Literal, Logical, PrintExpression, PureExpression, Stmt,
        Unary, Variable, VariableDeclaration, While,
    },
    scanner::tokens::{Token, TokenType, Value},
};

// TODO: add line numbers in all of these?
#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Tried to access idx {0} in array of len {0}")]
    OutOfBounds(usize, usize),
    #[error("Token stream prematurely terminated")]
    PrematureTermination,
    #[error("Missing literal value in a literal")]
    MissingLiteral,
    #[error("Invalid token type: {0:?}")]
    InvalidTokenType(TokenType),
    #[error("Invalid assignment target: {0}")]
    InvalidAssignmentTarget(Token),
}

pub struct Parser<'t> {
    current: usize,
    // TODO: make tokens an iterator rather than a slice
    tokens: &'t [Token],
}

// lifetime constraint enforces that the Tokens live longer than the Parser
impl<'t: 't, 'p> Parser<'t> {
    pub fn new(tokens: &'t [Token]) -> Self {
        Parser { current: 0, tokens }
    }

    /// parses a [`Token`] stream into a list of [`Stmt`]s
    pub fn parse(&mut self) -> Result<Vec<Stmt<'t>>, ParseError> {
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
    fn peek(&'p self) -> Option<&'t Token> {
        self.tokens.get(self.current)
    }

    /// get the previous token
    fn previous(&'p self) -> Result<&'t Token, ParseError> {
        Ok(&self.tokens[self.current - 1])
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
    fn expression(&'p mut self) -> Result<Expr<'t>, ParseError> {
        self.assignment()
    }

    fn consume(&'p mut self, token_type: TokenType) -> Result<&'t Token, ParseError> {
        if !self.match_token(HashSet::from([token_type])) {
            match self.peek() {
                Some(invalid_token) => {
                    return Err(ParseError::InvalidTokenType(
                        invalid_token.token_type.clone(),
                    ))
                }
                None => return Err(ParseError::OutOfBounds(self.current, self.tokens.len())),
            }
        }
        self.previous()
    }

    /// parse an assignment.
    fn assignment(&'p mut self) -> Result<Expr<'t>, ParseError> {
        let expr = self.or()?;

        // use lookahead for an "=" token to determine if this is an assignment
        if self.match_token(HashSet::from([TokenType::Equal])) {
            // NOTE: is this the equals sign or the var that we are setting??
            let equals = self.previous()?;
            // assignment is right-associative so recursively call assignment
            let val = self.assignment()?;

            // only variables are valid assignment targets
            if let Expr::Variable(var) = expr {
                return Ok(Expr::Assign(Assign::new(var.name, Box::new(val))));
            } else {
                return Err(ParseError::InvalidAssignmentTarget(equals.clone()));
            }
        }
        Ok(expr)
    }

    /// parse an or statement or anything of higher precedence
    fn or(&'p mut self) -> Result<Expr<'t>, ParseError> {
        let mut expr = self.and()?;

        while self.match_token(HashSet::from([TokenType::Or])) {
            let operator = self.previous()?;
            let right = self.and()?;
            expr = Expr::Logical(Logical::new(operator, Box::new(expr), Box::new(right)));
        }
        Ok(expr)
    }

    /// parse an or statement or anything of higher precedence
    fn and(&'p mut self) -> Result<Expr<'t>, ParseError> {
        let mut expr = self.equality()?;

        while self.match_token(HashSet::from([TokenType::Or])) {
            let operator = self.previous()?;
            let right = self.equality()?;
            expr = Expr::Logical(Logical::new(operator, Box::new(expr), Box::new(right)));
        }
        Ok(expr)
    }

    /// parse an equality or anything of higher precedence
    fn equality(&'p mut self) -> Result<Expr<'t>, ParseError> {
        let mut expr = self.comparison()?;
        while self.match_token(HashSet::from([TokenType::EqualEqual, TokenType::BangEqual])) {
            let operator = self.previous()?;
            let other = self.comparison()?;
            expr = Expr::Binary(Binary::new(operator, Box::new(expr), Box::new(other)));
        }
        Ok(expr)
    }

    /// parse a comparison or anything of higher precedence
    fn comparison(&'p mut self) -> Result<Expr<'t>, ParseError> {
        let mut expr = self.term()?;

        while self.match_token(HashSet::from([
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ])) {
            let operator = self.previous()?;
            let other = self.term()?;
            expr = Expr::Binary(Binary::new(operator, Box::new(expr), Box::new(other)));
        }
        Ok(expr)
    }

    /// parse a term or anything of higher precedence
    fn term(&'p mut self) -> Result<Expr<'t>, ParseError> {
        let mut expr = self.factor()?;

        while self.match_token(HashSet::from([TokenType::Plus, TokenType::Minus])) {
            let operator = self.previous()?;
            let other = self.factor()?;
            expr = Expr::Binary(Binary::new(operator, Box::new(expr), Box::new(other)));
        }
        Ok(expr)
    }

    /// parse a factor or anything of higher precedence
    fn factor(&'p mut self) -> Result<Expr<'t>, ParseError> {
        let mut expr = self.unary()?;

        while self.match_token(HashSet::from([TokenType::Slash, TokenType::Star])) {
            let operator = self.previous()?;
            let other = self.unary()?;
            expr = Expr::Binary(Binary::new(operator, Box::new(expr), Box::new(other)));
        }
        Ok(expr)
    }

    /// parse a unary expression or anything of higher precedence
    fn unary(&'p mut self) -> Result<Expr<'t>, ParseError> {
        if self.match_token(HashSet::from([
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Bang,
        ])) {
            let operator = self.previous()?;
            let primary = self.primary()?;
            return Ok(Expr::Unary(Unary::new(operator, Box::new(primary))));
        }
        self.primary()
    }

    /// parse a primary expression
    fn primary(&'p mut self) -> Result<Expr<'t>, ParseError> {
        let tok = self.peek().ok_or(ParseError::PrematureTermination)?;
        match &tok.token_type {
            TokenType::String
            | TokenType::Number
            | TokenType::True
            | TokenType::False
            | TokenType::Nil => {
                self.current += 1;
                Ok(Expr::Literal(Literal(
                    tok.literal.as_ref().ok_or(ParseError::MissingLiteral)?,
                )))
            }
            TokenType::LeftParen => {
                let expr = self.expression()?;
                self.consume(TokenType::RightParen)?;
                Ok(expr)
            }
            TokenType::Identifier => {
                self.current += 1;
                Ok(Expr::Variable(Variable::new(tok)))
            }
            tok_type => Err(ParseError::InvalidTokenType(tok_type.clone())),
        }
    }

    /// parses a statement or anything of higher precedence
    fn statement(&mut self) -> Result<Stmt<'t>, ParseError> {
        if self.match_token(HashSet::from([TokenType::For])) {
            return self.for_statement();
        } else if self.match_token(HashSet::from([TokenType::If])) {
            return self.if_statement();
        } else if self.match_token(HashSet::from([TokenType::While])) {
            return self.while_statement();
        } else if self.match_token(HashSet::from([TokenType::Print])) {
            return self.print_statement();
        } else if self.match_token(HashSet::from([TokenType::LeftBrace])) {
            return self.block();
        }
        self.expression_statement()
    }

    /// parses a Block or anything of higher precedence
    fn block(&mut self) -> Result<Stmt<'t>, ParseError> {
        let mut inner_statements = Vec::new();
        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            inner_statements.push(self.declaration()?);
        }
        self.consume(TokenType::RightBrace)?;
        Ok(Stmt::Block(Block::new(inner_statements)))
    }

    /// parses a while statement
    fn while_statement(&mut self) -> Result<Stmt<'t>, ParseError> {
        self.consume(TokenType::LeftParen)?;
        let cond = self.expression()?;
        self.consume(TokenType::RightParen)?;
        let body = self.statement()?;

        Ok(Stmt::While(While::new(cond, Box::new(body))))
    }

    /// for statement and desugar it into a while loop
    fn for_statement(&mut self) -> Result<Stmt<'t>, ParseError> {
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
            Stmt::While(While::new(cond, Box::new(body)))
        } else {
            // otherwise, cheat by adding a condition that always evaluates to true
            Stmt::While(While::new(
                Expr::Literal(Literal(&Value::Boolean(true))),
                Box::new(body),
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
    fn print_statement(&mut self) -> Result<Stmt<'t>, ParseError> {
        let value = self.expression()?;
        self.consume(TokenType::Semicolon)?;
        Ok(Stmt::Print(PrintExpression(value)))
    }

    /// parses an expression statement
    fn expression_statement(&mut self) -> Result<Stmt<'t>, ParseError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon)?;
        Ok(Stmt::Expression(PureExpression(expr)))
    }

    /// parse an if statement
    fn if_statement(&mut self) -> Result<Stmt<'t>, ParseError> {
        let _ = self.consume(TokenType::LeftParen)?;

        let condition = self.expression()?;
        let _ = self.consume(TokenType::RightParen)?;
        let then_branch = Box::new(self.statement()?);
        let else_branch = if self.match_token(HashSet::from([TokenType::Else])) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };
        Ok(Stmt::If(If::new(condition, then_branch, else_branch)))
    }

    /// parses a declaration or anything of higher precedence
    fn declaration(&mut self) -> Result<Stmt<'t>, ParseError> {
        if self.match_token(HashSet::from([TokenType::Var])) {
            self.variable_declaration()
        } else {
            self.statement()
        }
        // TODO: need to add synchronise() logic here
    }

    /// parses a variable declaration
    fn variable_declaration(&mut self) -> Result<Stmt<'t>, ParseError> {
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
    use crate::lox::scanner::tokens::Value;

    use super::*;

    #[test]
    fn test_parser() {
        let toks = vec![
            Token::new(
                TokenType::Number,
                "3".to_string(),
                Some(Value::Number(3.0)),
                0,
            ),
            Token::new(TokenType::Minus, "-".to_string(), None, 0),
            Token::new(
                TokenType::Number,
                "2".to_string(),
                Some(Value::Number(2.0)),
                0,
            ),
            Token::new(TokenType::Semicolon, ";".to_string(), None, 0),
        ];

        let mut parser = Parser::new(&toks);
        let ast = parser.parse().unwrap();
        // TODO: fix AST Printer and this test
        // let printer = Printer;

        // let out = printer.print(&ast);
        // println!("{out}");
        // assert_eq!(out, "(- 3 2)");
    }
}
