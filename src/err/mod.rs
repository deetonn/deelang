use crate::tokenizer::SourceLocation;


pub fn display_err(location: &SourceLocation, message: &str) {
    println!("{}:{}:{}: {}", location.file, location.line, location.column, message);
}

pub fn display_warning(location: &SourceLocation, message: &str) {
    println!("{}:{}:{}: warning: {}", location.file, location.line, location.column, message);
}

pub fn display_fatal(location: &SourceLocation, message: &str) -> ! {
    /* if no file (location.file) exists, just print a basic message */
    /* first, check if the file actually exists on disk */
    // The current path will be cwd\location.file
    let cwd = std::env::current_dir().unwrap();
    let mut file = String::new();
    file += cwd.to_str().unwrap();
    file += if cfg!(windows) { "\\" } else { "/" };
    file += location.file.as_str();
    let real_location = SourceLocation {
        file,
        line: location.line,
        column: location.column
    };

    if std::path::Path::new(&real_location.file).exists() {
        /* if it does, read the file and print the line */
        let file = match std::fs::read_to_string(&real_location.file) {
            Ok(file) => file,
            Err(e) => {
                println!("{}:{}:{}: fatal error: {}", real_location.file, real_location.line, real_location.column, message);
                println!("(skipped showing source due to error: {} -- {})", e, real_location.file);
                std::process::exit(1);
            }
        };
        let lines: Vec<&str> = file.split("\n").collect();
        println!("{}:{}:{}: fatal error: {}", real_location.file, real_location.line, real_location.column, message);
        println!("{} | {}", real_location.line, lines[real_location.line as usize]);
        println!("{} | {}^", real_location.line, "^".repeat(real_location.column as usize));
    } else {
        /* if it doesn't, just print the message */
        println!("{}:{}:{}: fatal error: {}", real_location.file, real_location.line, real_location.column, message);
    }
    if cfg!(debug_assertions) {
        // This exists for the stacktrace
        panic!("debug panic");
    }
    std::process::exit(1);
}