
use std::path::Path;

use crate::{Lexer, parser::parse, typechecker::TypeCheckerState};

/* this module exists to make running scripts easier. */

pub struct ExtraInfo {
    pub file_name: String,
    pub debug: bool,
}

fn private_run_script(script_file: &str) -> Result<(), String> {
    let source = match std::fs::read_to_string(script_file) {
        Ok(source) => source,
        Err(e) => return Err(format!("Error reading file: {}", e)),
    };
    let mut lexer = Lexer::new(source, None);
    lexer.lex();
    println!("tokens: {:#?}", lexer.tokens);

    let result = parse(lexer.tokens, ExtraInfo {
        file_name: script_file.to_owned(),
        debug: false,
    });
    let checker_state = TypeCheckerState::new(result);
    let optimized = crate::typechecker::walk_ast(&checker_state);
    println!("ast: {:#?}", optimized);

    Ok(())
}

pub fn execute(path: String) -> Result<(), String> {
    let path = Path::new(&path);
    if path.is_file() {
        return private_run_script(path.to_str().unwrap());
    } else if path.is_dir() {
        let mut files = Vec::new();
        for entry in std::fs::read_dir(path).unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.is_file() {
                files.push(path);
            }
        }
        for file in files {
            private_run_script(file.to_str().unwrap())?;
        }
        return Ok(());
    } else {
        return Err(format!("{} is not a file or directory", path.to_str().unwrap()));
    }
}