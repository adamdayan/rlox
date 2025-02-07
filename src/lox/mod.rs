use anyhow::Result;
use std::fs::read_to_string;
use std::io;
use std::path::Path;

use parser::Parser;
use scanner::Scanner;

pub mod ast;
pub mod parser;
pub mod scanner;

pub fn run_file(path: &Path) -> Result<()> {
    let source = read_to_string(path)?;
    run(source)?;
    Ok(())
}

pub fn run_prompt() -> Result<()> {
    let mut line = String::new();
    let stdin = io::stdin();
    while stdin.read_line(&mut line).is_ok() {
        match run(line) {
            Ok(_) => {}
            Err(e) => print!("{e}"),
        };
        // NOTE: is there a more idiomatic way to do this?
        line = String::new();
    }
    Ok(())
}

fn run(source: String) -> Result<()> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens()?;

    for token in tokens {
        print!("{:?}", token)
    }
    Ok(())
}
