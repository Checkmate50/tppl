use crate::ast;

pub fn string_of_const(c : ast::Const) -> String {
    use ast::Const;
    match c {
        Const::Bool(b) => b.to_string(),
        Const::Number(n) => n.to_string()
    }
}

pub fn string_of_binop(b : ast::Binop) -> String {
    use ast::Binop;
    match b {
        Binop::And => "&".to_owned(),
        Binop::Or => "|".to_owned(),
        Binop::Plus => "+".to_owned(),
        Binop::Minus => "-".to_owned(),
        Binop::Times => "*".to_owned(),
        Binop::Div => "/".to_owned(),
        Binop::Until => "<!>".to_owned(),
        Binop::SUntil => "<!!>".to_owned(),
    }
}

pub fn string_of_unop(u : ast::Unop) -> String {
    use ast::Unop;
    match u {
        Unop::Neg => "-".to_owned(),
        Unop::Not => "not".to_owned()
    }
}

pub fn string_of_expr(e : ast::Expr) -> String {
    use ast::Expr;
    match e {
        Expr::EBinop(b, e1, e2) => 
            string_of_expr(*e1) + " " + &string_of_binop(b) + " " + &string_of_expr(*e2),
        Expr::EConst(c) => string_of_const(c),
        Expr::EUnop(u, e) => string_of_unop(u) + &string_of_expr(*e),
        Expr::EVar(s) => s,
        Expr::Input => "input".to_owned()
    }
}

pub fn string_of_command(cmd : ast::Command) -> String {
    use ast::Command;
    match cmd {
        Command::Timestep => "---".to_owned(),
        Command::Global(v, e) => v + " = " + &string_of_expr(e), 
        Command::Next(v, e) => v + " <X " + &string_of_expr(e),
        Command::Update(v, e) => v + " <- " + &string_of_expr(e),
        Command::Finally(v, e) => v + " <.. " + &string_of_expr(e),
        Command::Print(e) => "print ".to_owned() + &string_of_expr(e)
    }
}

pub fn string_of_program(p : ast::Program) -> String {
    let mut result = "Program\n".to_owned();
    for cmd in p {
        result += &(string_of_command(cmd) + "\n");
    }
    result
}