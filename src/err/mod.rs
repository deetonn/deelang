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
    let location = SourceLocation {
        file: cwd.join(&location.file).to_str().unwrap().to_string(),
        line: location.line,
        column: location.column
    };

    if std::path::Path::new(&location.file).exists() {
        /* if it does, read the file and print the line */
        let file = std::fs::read_to_string(&location.file).unwrap();
        let lines: Vec<&str> = file.split("\n").collect();
        println!("{}:{}:{}: fatal error: {}", location.file, location.line, location.column, message);
        println!("{} | {}", location.line, lines[location.line as usize - 1]);
        println!("{} | {}^", location.line, " ".repeat(location.column as usize - 1));
    } else {
        /* if it doesn't, just print the message */
        println!("{}:{}:{}: fatal error: {}", location.file, location.line, location.column, message);
    }
    std::process::exit(1);
}