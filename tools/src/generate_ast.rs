use anyhow::Result;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

pub fn define_ast(output_dir: PathBuf, base_name: String, types: Vec<&str>) -> Result<()> {
    let path = output_dir.join(base_name.clone().to_lowercase() + ".rs");
    let mut ast_file = File::create(path)?;

    ast_file.write_all(format!("pub struct {base_name} {{\n").as_bytes())?;
    ast_file.write_all("}\n".as_bytes())?;

    Ok(())
}
