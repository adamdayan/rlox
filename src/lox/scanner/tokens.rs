use core::option::Option;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TokenType {
    // single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // one-or-two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // NOTE: it might be better to just include the lit value inside the TokenType::Literal enum types rather than
    // have a whole separate Literal enum for it
    // literals
    Identifier,
    String,
    Number,

    // keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    // misc
    Eof,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Boolean(bool),
    String(String),
    Number(f32),
    Nil,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Boolean(val) => write!(f, "{val}"),
            Value::String(val) => write!(f, "{val}"),
            Value::Number(val) => {
                if val % 1.0 == 0.0 {
                    return write!(f, "{:.0}", val);
                }
                write!(f, "{val}")
            }
            Value::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: Option<Value>,
    pub line: u32,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, literal: Option<Value>, line: u32) -> Self {
        Token {
            token_type,
            lexeme,
            literal,
            line,
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.literal {
            Some(lit) => write!(f, "{:?} {} {}", self.token_type, self.lexeme, lit),
            None => write!(f, "{:?} {}", self.token_type, self.lexeme),
        }
    }
}
