use core::fmt;
use chumsky::prelude::*;

// https://github.com/zesterer/chumsky/blob/master/examples/nano_rust.rs is pretty useful for this

pub type Span = std::ops::Range<usize>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    NEWLINE,
    THREEDASH,
    EQUAL,
    NEXT,
    LARROW,
    FUTURE,
    TRUE,
    FALSE,
    Op(String),
    Number(String),
    Var(String)
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::NEWLINE => write!(f, "NEW"),
            Token::THREEDASH => write!(f, "THREEDASH"),
            Token::EQUAL => write!(f, "="),
            Token::NEXT => write!(f, "<X"),
            Token::LARROW => write!(f, "<-"),
            Token::FUTURE => write!(f, "<.."),
            Token::TRUE => write!(f, "true"),
            Token::FALSE => write!(f, "false"),
            Token::Op(s) => write!(f, "Op {}", s),
            Token::Number(n) => write!(f, "Num {}", n),
            Token::Var(s) => write!(f, "Var {}", s) 
        }
    }
}

// Boring old lexer stuff
// https://github.com/zesterer/chumsky/blob/master/examples/nano_rust.rs is pretty much copied tbch
pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> + Clone {
    let num = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
        .collect::<String>()
        .map(Token::Number);

    // Before the op set for THREEDASH
    let multiop =
        just("\n").to(Token::NEWLINE)
        .or(just("---").to(Token::THREEDASH))
        .or(just("==").to(Token::Op("==".to_owned())))
        .or(just("=").to(Token::EQUAL))
        .or(just("<X").to(Token::NEXT))
        .or(just("<-").to(Token::LARROW))
        .or(just("<..").to(Token::FUTURE));

    let op = one_of("+-*/&|!<>")
        .repeated()
        .exactly(1)
        .collect::<String>()
        .map(Token::Op);

    let ident = 
            text::ident().map(|ident: String| match ident.as_str() {
        "true"  =>  Token::TRUE,
        "false" =>  Token::FALSE,
        _       =>  Token::Var(ident),
    });

    let token = num
        .or(num)
        .or(multiop)
        .or(op)
        .or(ident)
        .recover_with(skip_then_retry_until([]));

    let comment = just("//").then(take_until(just('\n'))).padded();

    let whitespace = just(" ").or(just("\t"));

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(comment.repeated())
        .padded_by(whitespace.repeated())
        .repeated()
}