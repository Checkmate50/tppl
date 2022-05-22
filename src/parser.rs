use chumsky::prelude::*;
use crate::ast;

pub fn parser() -> impl Parser<char, ast::Program, Error = Simple<char>> {
    recursive(|bf| choice((
        just('<').to(ast::Instr::Left),
        just('>').to(ast::Instr::Right),
        just('+').to(ast::Instr::Incr),
        just('-').to(ast::Instr::Decr),
        just(',').to(ast::Instr::Read),
        just('.').to(ast::Instr::Write),
        bf.delimited_by(just('['), just(']')).map(ast::Instr::Loop),
    ))
        .repeated())
}