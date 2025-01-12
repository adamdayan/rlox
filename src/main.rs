use anyhow::{anyhow, Result};
use lox::{run_file, run_prompt};
use std::{env, path::Path};

mod lox;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        print!("usage: rlox [script]");
        return Err(anyhow!("Invalid arguments"));
    } else if args.len() == 1 {
        run_file(Path::new(&args[0]))?;
    } else {
        // NOTE: do we want to return a Result from run_prompt?
        run_prompt();
    }

    Ok(())
}
