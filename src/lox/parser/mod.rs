use std::collections::HashSet;
use thiserror::Error;

use super::{
    ast::{Binary, Expr, Literal, Unary},
    scanner::tokens::{LiteralValue, Token, TokenType},
};

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Tried to access idx {0} in array of len {0}")]
    OutOfBounds(usize, usize),
    #[error("Token stream prematurely terminated")]
    PrematureTermination,
    #[error("Missing literal value in a literal")]
    MissingLiteral,
    #[error("Invalid token type")]
    InvalidTokenType,
}

pub struct Parser<'t> {
    current: usize,
    // TODO: make tokens an iterator rather than a slice
    tokens: &'t [Token],
}

// enforce that the Tokens live longer than the Parser
impl<'t: 't, 'p> Parser<'t> {
    pub fn new(tokens: &'t [Token]) -> Self {
        Parser { current: 0, tokens }
    }

    pub fn parse(&mut self) -> Result<Expr<'t>, ParseError> {
        self.expression()
    }

    /// advance our current position in the token stream by 1
    fn advance(&'p mut self) {
        self.current += 1;
    }

    /// get the next token without advancing
    fn peek(&'p self) -> Option<&'t Token> {
        self.tokens.get(self.current)
    }

    fn previous(&'p self) -> Result<&'t Token, ParseError> {
        Ok(&self.tokens[self.current - 1])
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
        self.equality()
    }

    fn consume(&'p mut self, token_type: TokenType) -> Result<(), ParseError> {
        if !self.match_token(HashSet::from([token_type])) {
            return Err(ParseError::InvalidTokenType);
        }
        Ok(())
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
        match tok.token_type {
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
                return Ok(expr);
            }
            _ => Err(ParseError::InvalidTokenType),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lox::ast::printer::Printer;

    use super::*;

    #[test]
    fn test_parser() {
        let toks = vec![
            Token::new(
                TokenType::Number,
                "3".to_string(),
                Some(LiteralValue::Number(3.0)),
                0,
            ),
            Token::new(TokenType::Minus, "-".to_string(), None, 0),
            Token::new(
                TokenType::Number,
                "2".to_string(),
                Some(LiteralValue::Number(2.0)),
                0,
            ),
        ];

        let mut parser = Parser::new(&toks);
        let ast = parser.parse().unwrap();
        let printer = Printer;
        let out = printer.print(&ast);
        println!("{out}");
        assert_eq!(out, "(- 3 2)");
    }
}
