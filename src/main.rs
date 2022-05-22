use chumsky::prelude::*;
mod parser;
mod ast;
mod ast_printer;

fn main() {
    match parser::parser().parse("<>") {
        Ok(p) => println!("{}", ast_printer::string_of_program(p)),
        Err(errs) => errs.into_iter().for_each(|e| println!("{:?}", e)),
    }
}