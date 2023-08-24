pub mod tokenizer;
pub mod exec;
pub mod parser;
pub mod err;
pub mod typechecker;

use tokenizer::lexer::Lexer;
use exec::execute;

fn main() {
    let _ = execute("test.dl".to_owned());
}
