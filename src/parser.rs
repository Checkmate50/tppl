use chumsky::prelude::*;
use crate::ast;
use crate::lexer;

pub type Spanned<T> = (T, lexer::Span);

pub fn parser() -> impl Parser<lexer::Token, Spanned<ast::Program>, Error = Simple<lexer::Token>> + Clone {
    use lexer::Token;
    // let expr = recursive(|expr| {
    //     // use ast::Expr;
    //     // let atom = just(Token::Number)
    //     //         .map(|n: i64| Expr::EConst(ast::Const::Number(n)))
    //     //     .or();

    //     // let unary = just(Token::MINUS)
    //     //     .repeated()
    //     //     .then(atom)
    //     //     .foldr(|_op, rhs| Expr::EUnop(ast::Unop::Neg, Box::new(rhs)));
    // });

    let cmd = recursive(|cmd| {
        use ast::Command;
        let ident = select! { Token::Var(ident) => ident.clone() };
        let statement = ident
            .then_ignore(just(Token::EQUAL))
            .map(|x| Command::Global(x, ast::Expr::Input));

        statement
            .clone()
            .then(just(Token::NEWLINE))
            .foldl(|a, b| {
            let span = a.1.start..b.1.end;
            (a, span)
        })
    });
    cmd.then_ignore(end())
}