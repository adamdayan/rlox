use anyhow::Result;
use tokens::Token;

mod tokens;

// NOTE: do we want an enum variant for each error type?
#[derive(Debug)]
pub struct ScanError {
    line: u32,
    location: String,
    message: String,
}

impl std::error::Error for ScanError {}

impl std::fmt::Display for ScanError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}] Error {}: {}",
            self.line, self.location, self.message
        )
    }
}

pub struct Scanner {
    source: String,
    tokens: Vec<Token>,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Scanner {
            source,
            tokens: vec![],
        }
    }

    // NOTE: does this need to be mut?
    pub fn scan_tokens(&mut self) -> Result<Vec<Token>> {
        return Ok(vec![]);
    }
}
