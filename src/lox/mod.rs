use anyhow::Result;
use std::fs::read_to_string;
use std::io;
use std::path::Path;

use scanner::Scanner;

mod scanner;

pub fn run_file(path: &Path) -> Result<()> {
    let source = read_to_string(path)?;
    run(source)?;
    return Ok(());
}

pub fn run_prompt() -> Result<()> {
    let mut line = String::new();
    let stdin = io::stdin();
    while let Ok(_) = stdin.read_line(&mut line) {
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
    return Ok(());
}
