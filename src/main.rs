mod lox;
mod scanner;
mod parser;
mod interpreter;
mod resolver;

use std::error::Error;
use crate::lox::{run_prompt, run_file, RunErrorType};
use clap;
use clap::Parser;

#[derive(clap::Parser)]
struct CommandLine {
    script: Option<std::path::PathBuf>
}

type ErrorCode = i32;

fn process_status_code(status: Result<(), RunErrorType>) {
    match status {
        Ok(_) => {}
        Err(x) => {
            match x {
                RunErrorType::Scanner(msgs) => {
                    for (line, msg) in msgs {
                        eprintln!("[line {}] Error: {}", line, msg);
                    }
                    std::process::exit(65);
                }
                RunErrorType::Parser(msgs) => {
                    for err in msgs {
                        let (t, b) = err;
                        eprintln!("[line {}] {}", t.line, b);
                    }
                    std::process::exit(65);
                }
                RunErrorType::Resolver(msg) => {
                    eprintln!("[line {}] {}", msg.line, msg.message);
                    std::process::exit(65);
                }
                RunErrorType::Interpreter(msg) => {
                    eprintln!("{}", msg);
                    std::process::exit(70);
                }
                RunErrorType::IOError(msg) => {
                    eprintln!("{}", msg);
                    std::process::exit(70);
                }
            }
        }
    }
}

fn main() {
    let args = CommandLine::parse();

    let status = if let Some(path) = args.script {
        run_file(path)
    }
    else {
        run_prompt()
    };
    process_status_code(status);
}