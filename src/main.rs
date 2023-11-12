mod lox;

extern crate lox_derive;

use std::error::Error;
use crate::lox::{run_prompt, run_file, Token};
use clap;
use clap::Parser;

#[derive(clap::Parser)]
struct CommandLine {
    script: Option<std::path::PathBuf>
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = CommandLine::parse();

    if let Some(path) = args.script {
        run_file(path)?;
    }
    else {
        run_prompt()?;
    }
    Ok(())
}