use once_cell::sync::Lazy;
use std::collections::HashMap;

use anyhow::Result;
use tokens::{Literal, Token, TokenType};

pub mod tokens;

static RESERVED_KEYWORDS: Lazy<HashMap<&str, TokenType>> = Lazy::new(|| {
    HashMap::from([
        ("and", TokenType::And),
        ("class", TokenType::Class),
        ("else", TokenType::Else),
        ("false", TokenType::False),
        ("for", TokenType::For),
        ("fun", TokenType::Fun),
        ("if", TokenType::If),
        ("nil", TokenType::Nil),
        ("or", TokenType::Or),
        ("print", TokenType::Print),
        ("return", TokenType::Return),
        ("super", TokenType::Super),
        ("this", TokenType::This),
        ("true", TokenType::True),
        ("var", TokenType::Var),
        ("while", TokenType::While),
    ])
});

// NOTE: do we want an enum variant for each error type?
#[derive(Debug)]
pub enum ScanError {
    LexicalError {
        line: u32,
        location: String,
        message: String,
    },
    UnknownToken {
        line: u32,
        token: String,
    },
}

impl std::error::Error for ScanError {}

impl std::fmt::Display for ScanError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ScanError::LexicalError {
                line,
                location,
                message,
            } => write!(f, "[{}] Error {}: {}", line, location, message),
            ScanError::UnknownToken { line, token } => {
                write!(f, "[{}] Unknown token: {}", line, token)
            }
        }
    }
}

fn is_digit(c: char) -> bool {
    if c.is_ascii_digit() {
        return true;
    }
    false
}

fn is_alpha(c: char) -> bool {
    if c.is_ascii_lowercase() || c.is_ascii_uppercase() || c == '_' {
        return true;
    }
    false
}

fn is_alphanumeric(c: char) -> bool {
    is_digit(c) || is_alpha(c)
}

pub struct Scanner {
    source: Vec<char>,
    tokens: Vec<Token>,
    // first character in current lexeme
    start: u32,
    // current character
    current: u32,
    // current line
    line: u32,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Scanner {
            source: source.chars().collect(),
            tokens: vec![],
            start: 0,
            current: 0,
            line: 0,
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len().try_into().unwrap()
    }

    fn advance(&mut self) -> char {
        let c = self.source[self.current as usize];
        self.current += 1;
        c
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.source[self.current as usize]
    }

    fn peek_next(&self) -> char {
        let next = (self.current + 1) as usize;
        if next >= self.source.len() {
            return '\0';
        }
        self.source[next]
    }

    fn match_next(&mut self, target: char) -> bool {
        if self.source[(self.current + 1) as usize] == target {
            self.current += 1;
            return true;
        }
        false
    }

    // converts current lexeme from slice of chars to String
    fn current_to_string(&self) -> String {
        String::from_iter(&self.source[self.start as usize..self.current as usize])
    }

    fn add_token(&mut self, token_type: TokenType, literal: Option<Literal>) {
        let lexeme = self.current_to_string();
        self.tokens
            .push(Token::new(token_type, lexeme, literal, self.line));
    }

    fn scan_identifier(&mut self) {
        while is_alphanumeric(self.peek()) {
            self.advance();
        }

        let val = self.current_to_string();
        if let Some(keyword_type) = RESERVED_KEYWORDS.get(val.as_str()) {
            self.add_token(keyword_type.clone(), None);
        } else {
            self.add_token(TokenType::Identifier, None);
        }
    }

    fn scan_token(&mut self) -> Result<()> {
        let c = self.advance();
        match c {
            // single-character tokens
            '(' => self.add_token(TokenType::LeftParen, None),
            ')' => self.add_token(TokenType::RightParen, None),
            '{' => self.add_token(TokenType::LeftBrace, None),
            '}' => self.add_token(TokenType::RightBrace, None),
            ',' => self.add_token(TokenType::Comma, None),
            '.' => self.add_token(TokenType::Dot, None),
            '-' => self.add_token(TokenType::Minus, None),
            '+' => self.add_token(TokenType::Plus, None),
            ';' => self.add_token(TokenType::Semicolon, None),
            '*' => self.add_token(TokenType::Star, None),

            // one-or-two character tokens
            '!' => {
                // NOTE: couldn't call if inside add_token because of double &mut
                if self.match_next('=') {
                    self.add_token(TokenType::BangEqual, None)
                } else {
                    self.add_token(TokenType::Bang, None)
                };
            }
            '=' => {
                if self.match_next('=') {
                    self.add_token(TokenType::EqualEqual, None)
                } else {
                    self.add_token(TokenType::Equal, None)
                };
            }
            '>' => {
                if self.match_next('=') {
                    self.add_token(TokenType::GreaterEqual, None)
                } else {
                    self.add_token(TokenType::Greater, None)
                };
            }
            '<' => {
                if self.match_next('=') {
                    self.add_token(TokenType::LessEqual, None)
                } else {
                    self.add_token(TokenType::Less, None)
                };
            }

            '/' => {
                // double slash means comment, which we should ignore
                if self.match_next('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash, None)
                }
            }

            // string literals
            '\'' => {
                while self.peek() != '\'' && !self.is_at_end() {
                    if self.peek() == '\n' {
                        self.line += 1;
                    }
                    self.advance();
                }

                if self.is_at_end() {
                    return Err(ScanError::LexicalError {
                        line: self.line,
                        location: self.current_to_string(),
                        message: "Unterminated string".to_string(),
                    }
                    .into());
                }
                // account for closing quote mark
                self.advance();
                let val = String::from_iter(
                    &self.source[(self.start + 1) as usize..(self.current - 1) as usize],
                );
                self.add_token(TokenType::String, Some(Literal::String(val)));
            }

            // increment line number on new-line
            '\n' => self.line += 1,
            // ignore irrelevant chars
            '\r' | ' ' | '\t' => {}
            _ => {
                // handle numbers
                if is_digit(c) {
                    while is_digit(self.peek()) {
                        self.advance();
                    }

                    // look for fractional part
                    if self.match_next('.') {
                        while is_digit(self.peek_next()) {
                            self.advance();
                        }
                    }
                    let val = self.current_to_string().parse::<f32>().unwrap();
                    self.add_token(TokenType::Number, Some(Literal::Number(val)))
                } else if is_alpha(c) {
                    // handle keywords and identifiers
                    self.scan_identifier();
                } else {
                    // any other character is invalid
                    return Err(ScanError::UnknownToken {
                        line: self.line,
                        token: self.current_to_string(),
                    }
                    .into());
                }
            }
        }
        Ok(())
    }

    pub fn scan_tokens(&mut self) -> Result<&Vec<Token>> {
        // iterate through source until we've scanned all tokens
        while !self.is_at_end() {
            self.start = self.current;
            match self.scan_token() {
                Ok(()) => continue,
                // if we hit an error, print it but carry on scanning so we catch as many errors as
                // possible in 1 pass
                Err(e) => print!("{e}"),
            }
        }

        self.tokens
            .push(Token::new(TokenType::Eof, String::new(), None, self.line));
        // NOTE: do I really want to be returning a ref to the Token vec? Who should own it
        Ok(&self.tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::Scanner;
    use crate::lox::scanner::tokens::TokenType;

    #[test]
    fn test_parentheses() {
        let source = String::from("( );");
        let mut scanner = Scanner::new(source);
        scanner.scan_tokens().unwrap();

        let real = vec![TokenType::LeftParen, TokenType::RightParen];
        for (scanned, real) in scanner.tokens.iter().zip(real.iter()) {
            assert!(scanned.token_type == *real);
        }
    }
}
