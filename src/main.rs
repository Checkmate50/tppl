use chumsky::{prelude::*, Stream};
mod ast;
mod lexer;
mod parser;
mod ast_printer;


fn main() {
    let src = "x <- true";
    let (tokens, mut errs) = lexer::lexer().parse_recovery(src);

    let parse_errs = if let Some(tokens) = tokens {
        let len = src.chars().count();
        let (prog, mut parse_errs) = 
            parser::parser().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

        if let Some(funcs) = 
            prog.filter(|_| errs.len() + parse_errs.len() == 0) {
                println!("{}", ast_printer::string_of_program(funcs.0));
        }
        parse_errs
    } else {
        Vec::new()
    };
}