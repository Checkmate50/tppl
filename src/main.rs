use chumsky::{prelude::*, Stream};
use std::{env, fs};
mod ast;
mod ast_printer;
pub mod builtins;
pub mod errors;
pub mod interpreter;
mod lexer;
mod parser;
pub mod typecheck;
pub mod types;

fn main() {
    let src = fs::read_to_string(env::args().nth(1).expect("Expected file argument"))
        .expect("failed to read file");
    let (tokens, lex_errs) = lexer::lexer().parse_recovery(src.as_str());

    if lex_errs.len() > 0 {
        println!("lexer errors {:?}", lex_errs);
    } else {
        let parse_errs = if let Some(tokens) = tokens {
            let len = src.chars().count();
            let (prog, parse_errs) = parser::parser()
                .parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));
            if let Some(funcs) = prog
                .clone()
                .filter(|_| lex_errs.len() + parse_errs.len() == 0)
            {
                println!("{}", ast_printer::string_of_program(funcs));

                let program_result = match typecheck::infer_program(prog.unwrap()) {
                    Ok(ty_prog) => match interpreter::exec_program(ty_prog.clone()) {
                        Ok(_) => {
                            // println!("{:?}", ty_prog);
                            println!("Executed");
                            Ok(())
                        }
                        Err(e) => Err(errors::Error::Dynamic(e)),
                    },
                    Err(e) => Err(errors::Error::Static(e)),
                };
                match program_result {
                    Ok(()) => (),
                    Err(e) => println!("{:?}", e),
                };
            }
            parse_errs
        } else {
            println!("huh");
            Vec::new()
        };
        if parse_errs.len() > 0 {
            println!("parser errors {:?}", parse_errs);
        }
    }
}
