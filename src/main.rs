use chumsky::{prelude::*, Stream};
use std::{fs,env};
mod ast;
mod lexer;
mod parser;
mod ast_printer;


fn main() {
    let src = fs::read_to_string(env::args().nth(1).expect("Expected file argument"))
        .expect("failed to read file");
    let (tokens, errs) = lexer::lexer().parse_recovery(src.as_str());

    let parse_errs = if let Some(tokens) = tokens {
        let len = src.chars().count();
        let (prog, parse_errs) = 
            parser::parser().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

        if let Some(funcs) = 
            prog.filter(|_| errs.len() + parse_errs.len() == 0) {
                println!("{}", ast_printer::string_of_program(funcs));
        }
        parse_errs
    } else {
        Vec::new()
    };
    if parse_errs.len() > 0 {
        println!("{:?}", parse_errs);
    }
}