use parser::ast::Locate;

pub mod parser;
pub mod syntax;
pub mod util;
pub mod tyck;

type Error = Diagnostic;

pub struct Diagnostic {
    pos: Locate,
    msg: String,
}

fn main() {
    println!("Hello, world!");
}
