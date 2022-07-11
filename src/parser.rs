use crate::ast;
use crate::lexer;
use chumsky::prelude::*;

fn expr_parser() -> impl Parser<lexer::Token, ast::Expr, Error = Simple<lexer::Token>> + Clone {
    use ast::Expr;
    use lexer::Token;
    recursive(|expr| {
        let val = select! {
            Token::Number(n) => Expr::EConst(ast::Const::Number(n.parse().unwrap())),
            Token::Float(f) => Expr::EConst(ast::Const::Float(f.parse().unwrap())),
            Token::TRUE => Expr::EConst(ast::Const::Bool(true)),
            Token::FALSE => Expr::EConst(ast::Const::Bool(false)),
            Token::Var(x) => Expr::EVar(x),
            Token::INPUT => Expr::EInput
        }
        .labelled("value");

        let ident = select! { Token::Var(ident) => ident.clone() };
        let call = ident
            .then(
                expr.clone()
                    .separated_by(just(Token::COMMA))
                    .allow_trailing()
                    .delimited_by(just(Token::LPAREN), just(Token::RPAREN)),
            )
            .map(|(f, args)| Expr::ECall(f, args));

        let atom = call
            .or(val)
            .or(expr.delimited_by(just(Token::LPAREN), just(Token::RPAREN)));

        let unary = just(Token::Op("-".to_owned()))
            .to(ast::Unop::Neg)
            .or(just(Token::Op("!".to_owned())).to(ast::Unop::Not))
            .repeated()
            .then(atom)
            .foldr(|op, rhs| Expr::EUnop(op, Box::new(rhs)));

        // operations * and /
        let timesdiv = unary
            .clone()
            .then(
                just(Token::Op("*".to_owned()))
                    .to(ast::Binop::Times)
                    .or(just(Token::Op("/".to_owned())).to(ast::Binop::Div))
                    .then(unary)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| Expr::EBinop(op, Box::new(lhs), Box::new(rhs)));

        // operations + and -
        let plusminus = timesdiv
            .clone()
            .then(
                just(Token::Op("+".to_owned()))
                    .to(ast::Binop::Plus)
                    .or(just(Token::Op("-".to_owned())).to(ast::Binop::Minus))
                    .then(timesdiv)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| Expr::EBinop(op, Box::new(lhs), Box::new(rhs)));

        // operations & and |
        let andor = plusminus
            .clone()
            .then(
                just(Token::Op("&".to_owned()))
                    .to(ast::Binop::And)
                    .or(just(Token::Op("|".to_owned())).to(ast::Binop::Or))
                    .then(plusminus)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| Expr::EBinop(op, Box::new(lhs), Box::new(rhs)));

        // operations <!> and <!!>
        let until = andor
            .clone()
            .then(
                just(Token::Op("<!>".to_owned()))
                    .to(ast::Binop::Until)
                    .or(just(Token::Op("<!!>".to_owned())).to(ast::Binop::SUntil))
                    .then(andor)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| Expr::EBinop(op, Box::new(lhs), Box::new(rhs)));

        until.labelled("expr")
    })
}

fn assignment_parser(
) -> impl Parser<lexer::Token, ast::Command, Error = Simple<lexer::Token>> + Clone {
    use ast::Command;
    use lexer::Token;

    let ident = select! { Token::Var(ident) => ident.clone() };

    let prop_target = ident.then(
        ident
            .separated_by(just(Token::COMMA))
            .delimited_by(just(Token::LPAREN), just(Token::RPAREN)), // .at_most(1),
    );

    let assign_wrapper = select! {
        Token::EQUAL => Command::Global as fn(String, ast::Expr) -> ast::Command,
        Token::NEXT => Command::Next as fn(String, ast::Expr) -> ast::Command,
        Token::LARROW => Command::Update as fn(String, ast::Expr) -> ast::Command,
        Token::FUTURE => Command::Finally as fn(String, ast::Expr) -> ast::Command
    };

    let assignment = choice((
        ident
            .then(assign_wrapper)
            .then(expr_parser())
            .map(|((name, wrapper), expr)| wrapper(name, expr)),
        prop_target.then(assign_wrapper).then(expr_parser()).map(
            |(((p_name, p_args), wrapper), expr)| {
                let expr = ast::Expr::EPred(p_name.clone(), p_args, Box::new(expr));
                wrapper(p_name, expr)
            },
        ),
    ));

    assignment
}

pub fn command_parser(
) -> impl Parser<lexer::Token, ast::Command, Error = Simple<lexer::Token>> + Clone {
    use lexer::Token;

    recursive(|_| {
        use ast::Command;
        // ---
        let timestep = just(Token::THREEDASH).to(Command::Timestep);

        // assertion of a non-assignment command (e.g., a print) is non-sensical.
        let assertion = just(Token::ASSERT)
            .ignore_then(assignment_parser())
            .map(|c| Command::Assert(Box::new(c)));

        let print = just(Token::PRINT)
            .ignore_then(expr_parser())
            .map(Command::Print);

        let statement = timestep
            .clone()
            .or(assertion)
            .or(assignment_parser())
            .or(print.clone());

        statement
    })
}

pub fn parser() -> impl Parser<lexer::Token, ast::Program, Error = Simple<lexer::Token>> + Clone {
    use lexer::Token;

    let newline = just(Token::NEWLINE).or(just(Token::COMMENT));

    newline
        .clone()
        .repeated()
        .ignore_then(command_parser().separated_by(newline.clone().repeated().at_least(1)))
        .then_ignore(newline.repeated())
        .then_ignore(end())
}
