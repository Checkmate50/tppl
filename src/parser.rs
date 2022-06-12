use chumsky::prelude::*;
use crate::ast;
use crate::lexer;

pub fn parser() -> impl Parser<lexer::Token, ast::Program, Error = Simple<lexer::Token>> + Clone {
    use lexer::Token;
    let expr = recursive(|_| {
        use ast::Expr;
        let val = select! {
            Token::Number(n) => Expr::EConst(ast::Const::Number(n.parse().unwrap())),
            Token::TRUE => Expr::EConst(ast::Const::Bool(true)),
            Token::FALSE => Expr::EConst(ast::Const::Bool(false)),
            Token::Var(x) => Expr::EVar(x),
            Token::INPUT => Expr::Input
        }.labelled("value");

        // unary operations ! and -
        let unary = just(Token::Op("-".to_owned())).to(ast::Unop::Neg)
            .or(just(Token::Op("!".to_owned())).to(ast::Unop::Not))
            .repeated()
            .then(val)
            .foldr(|op, rhs| Expr::EUnop(op, Box::new(rhs)));

        // operations * and /
        let timesdiv = 
        unary.clone()
        .then(
            just(Token::Op("*".to_owned())).to(ast::Binop::Times)
            .or(just(Token::Op("/".to_owned())).to(ast::Binop::Div))
            .then(unary)
            .repeated()
        )
        .foldl(|lhs, (op, rhs)| Expr::EBinop(op, Box::new(lhs), Box::new(rhs)));

        // operations + and -
        let plusminus = 
        timesdiv.clone()
        .then(
            just(Token::Op("+".to_owned())).to(ast::Binop::Plus)
            .or(just(Token::Op("-".to_owned())).to(ast::Binop::Minus))
            .then(timesdiv)
            .repeated()
        )
        .foldl(|lhs, (op, rhs)| Expr::EBinop(op, Box::new(lhs), Box::new(rhs)));

        // operations & and |
        let andor = 
        plusminus.clone()
        .then(
            just(Token::Op("&".to_owned())).to(ast::Binop::And)
            .or(just(Token::Op("|".to_owned())).to(ast::Binop::Or))
            .then(plusminus)
            .repeated()
        )
        .foldl(|lhs, (op, rhs)| Expr::EBinop(op, Box::new(lhs), Box::new(rhs)));

        // operations <!> and <!!>
        let until = 
        andor.clone()
        .then(
            just(Token::Op("<!>".to_owned())).to(ast::Binop::Until)
            .or(just(Token::Op("<!!>".to_owned())).to(ast::Binop::SUntil))
            .then(andor)
            .repeated()
        )
        .foldl(|lhs, (op, rhs)| Expr::EBinop(op, Box::new(lhs), Box::new(rhs)));

        until
    });

    let cmd = recursive(|_| {
        use ast::Command;
        let ident = select! { Token::Var(ident) => ident.clone() };

        // ---
        let timestep = just(Token::THREEDASH).to(ast::Command::Timestep);

        // =
        let global = 
            ident
            .then_ignore(just(Token::EQUAL))
            .then(expr.clone())
            .map(|(x, e)| Command::Global(x, e));

        // <X
        let next = 
            ident
            .then_ignore(just(Token::NEXT))
            .then(expr.clone())
            .map(|(x, e)| Command::Next(x, e));

        // <-
        let update = 
            ident
            .then_ignore(just(Token::LARROW))
            .then(expr.clone())
            .map(|(x, e)| Command::Update(x, e));

        // <..
        let finally = 
            ident
            .then_ignore(just(Token::FUTURE))
            .then(expr.clone())
            .map(|(x, e)| Command::Finally(x, e));

        let print = 
            just(Token::PRINT)
            .then(expr.clone())
            .map(|(_, e)| ast::Command::Print(e));

        let newline = just(Token::NEWLINE);

        let statement = 
            timestep.clone()
            .or(global.clone())
            .or(next.clone())
            .or(update.clone())
            .or(finally.clone())
            .or(print.clone())
            .padded_by(newline.repeated())
            .repeated();

        statement
    });
    cmd.then_ignore(end())
}