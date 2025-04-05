use anyhow::Result;
use parser::Parser;
use std::fs::read_to_string;
use std::io::{self, Write};
use std::path::Path;

use interpreter::Interpreter;
use resolver::Resolver;
use scanner::Scanner;

pub mod ast;
pub mod callable;
pub mod environment;
pub mod interpreter;
pub mod parser;
mod resolver;
pub mod scanner;

pub fn run_file(path: &Path) -> Result<()> {
    let source = read_to_string(path)?;
    let mut interpreter = Interpreter::new();
    run(source, &mut interpreter)?;
    Ok(())
}

pub fn run_prompt() -> Result<()> {
    let mut line = String::new();
    let stdin = io::stdin();
    print!("lox > ");
    io::stdout().flush()?;
    let mut interpreter = Interpreter::new();

    while stdin.read_line(&mut line).is_ok() {
        match run(line, &mut interpreter) {
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

fn run(source: String, interpreter: &mut Interpreter) -> Result<()> {
    let scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens()?;
    let mut parser = Parser::new(&tokens);
    // TODO: fix error handling
    let statements = parser.parse().unwrap();
    let mut resolver = Resolver::new();
    resolver.resolve(&statements, interpreter)?;

    if let Err(e) = interpreter.interpret(&statements) {
        println!("Error: {e}");
    }

    Ok(())
}
