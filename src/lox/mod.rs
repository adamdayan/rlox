use anyhow::Result;
use parser::Parser;
use std::fs::read_to_string;
use std::io::{self, Write};
use std::path::Path;

use environment::Environment;
use interpreter::Interpreter;
use scanner::Scanner;

pub mod ast;
pub mod environment;
pub mod interpreter;
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
    print!("lox > ");
    io::stdout().flush()?;
    while stdin.read_line(&mut line).is_ok() {
        match run(line) {
            Ok(_) => {}
            Err(e) => println!("{e}"),
        };
        // NOTE: is there a more idiomatic way to do this?
        line = String::new();
        print!("lox > ");
        io::stdout().flush()?;
    }
    Ok(())
}

fn run(source: String) -> Result<()> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens()?;

    let mut parser = Parser::new(tokens);
    let statements = parser.parse()?;

    let mut interpreter = Interpreter::new();
    interpreter.interpret(&statements)?;

    Ok(())
}
