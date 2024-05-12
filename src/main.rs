mod lox;
mod scanner;
mod parser;
mod interpreter;
mod resolver;

use crate::lox::{run_prompt, run_file, RunErrorType};
use clap::Parser;

#[derive(clap::Parser)]
struct CommandLine {
    script: Option<std::path::PathBuf>
}

fn process_status_code(status: Result<(), RunErrorType>) {
    match status {
        Ok(_) => {}
        Err(x) => {
            match x {
                RunErrorType::ScannerParser(scan_errors, parse_errors) => {
                    for (line, msg) in scan_errors {
                        eprintln!("[line {}] Error: {}", line, msg);
                    }
                    for err in parse_errors {
                        let (t, b) = err;
                        eprintln!("[line {}] {}", t.line, b);
                    }
                    std::process::exit(65);
                }
                RunErrorType::Resolver(msg_list) => {
                    for msg in msg_list {
                        eprintln!("[line {}] {}", msg.line, msg.message);
                    }
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