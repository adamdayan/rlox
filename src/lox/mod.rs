use anyhow::Result;
use parser::Parser;
use scanner::tokens::Token;
use std::cell::RefCell;
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
    let mut scanner = Scanner::new();
    run(source, &mut interpreter, &mut scanner)?;
    Ok(())
}

pub fn run_prompt() -> Result<()> {
    let mut line = String::new();
    let stdin = io::stdin();
    print!("lox > ");
    io::stdout().flush()?;
    let mut interpreter = Interpreter::new();
    let mut tokens = RefCell::new(vec![]);

    let prev_idx = 0;

    while stdin.read_line(&mut line).is_ok() {
        let scanner = Scanner::new(line);

        {
            let temp_tokens = tokens.get_mut();
            temp_tokens.append(&mut scanner.scan_tokens()?);
        }

        {
            let temp_tokens = tokens.borrow();
            match run(&temp_tokens[prev_idx..], &mut interpreter) {
                Ok(_) => {}
                Err(e) => println!("{e}"),
            };
        }
        // NOTE: is there a more idiomatic way to do this?
        line = String::new();
        print!("lox > ");
        io::stdout().flush()?;
    }
    drop(interpreter);
    Ok(())
}

fn run<'t>(tokens: &'t [Token], interpreter: &mut Interpreter<'t, '_>) -> Result<()> {
    let mut parser = Parser::new(tokens);
    let statements = parser.parse()?;

    if let Err(e) = interpreter.interpret(&statements) {
        println!("Error: {e}");
    }

    Ok(())
}
