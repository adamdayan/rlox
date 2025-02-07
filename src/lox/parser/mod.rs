use std::collections::HashSet;

use super::{
    ast::{Binary, Expr, Literal},
    scanner::tokens::{LiteralValue, Token, TokenType},
};

pub struct Parser<'t> {
    current: usize,
    tokens: &'t [Token],
}

impl<'t> Parser<'t> {
    /// advance our current position in the token stream by 1
    fn advance(&mut self) {
        self.current += 1;
    }

    /// get the next token without advancing
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    /// if the next token is in targets, advance and return true otherwise return false
    fn match_token(&mut self, targets: HashSet<TokenType>) -> bool {
        if let Some(peeked) = self.peek() {
            if targets.contains(&peeked.token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    /// parse an expression
    fn expression(&mut self) -> Expr {
        self.equality()
    }

    fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();
        while {
            let is_matched =
                self.match_token(HashSet::from([TokenType::EqualEqual, TokenType::BangEqual]));
            is_matched
        } {
            let operator = self.previous();
            let other = self.comparison();
            expr = Expr::Binary(Binary::new(operator, Box::new(expr), Box::new(other)));
        }
        expr
    }

    fn comparison(&mut self) -> Expr {}
}
