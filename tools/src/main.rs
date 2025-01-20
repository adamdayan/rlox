use anyhow::Result;
use std::path::PathBuf;

use generate_ast::define_ast;
mod generate_ast;

fn main() -> Result<()> {
    define_ast(
        PathBuf::from("/home/adam/play/test_out"),
        "Expr".to_string(),
        vec![
            "Binary : Expr left, Token operator, Expr right",
            "Grouping: Expr expression",
            "Literal : Object value",
            "Unary : Token operator, Expr right",
        ],
    )
}
