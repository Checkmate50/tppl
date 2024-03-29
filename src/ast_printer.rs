use crate::ast;

pub fn string_of_distribution(d: ast::Distribution) -> String {
    use ast::Distribution;
    match d {
        Distribution::Uniform(low, high) => format!(
            "Uniform({}, {})",
            string_of_const(*low),
            string_of_const(*high)
        ),
        Distribution::Normal(mean, std_dev) => format!(
            "Normal({}, {})",
            string_of_const(*mean),
            string_of_const(*std_dev)
        ),
        Distribution::List(consts) => format!(
            "List({}, ...)",
            consts
                .iter()
                .map(f64::to_string)
                .take(3)
                .collect::<Vec<String>>()
                .join(", ")
        ),
    }
}

fn string_of_const(c: ast::Const) -> String {
    use ast::Const;
    match c {
        Const::Bool(b) => b.to_string(),
        Const::Number(n) => n.to_string(),
        Const::Float(f) => f.to_string(),
        Const::Pdf(d) => string_of_distribution(d),
    }
}

fn string_of_binop(b: ast::Binop) -> String {
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

fn string_of_unop(u: ast::Unop) -> String {
    use ast::Unop;
    match u {
        Unop::Neg => "-".to_owned(),
        Unop::Not => "not".to_owned(),
    }
}

pub fn string_of_expr(e: ast::Expr) -> String {
    use ast::Expr;
    match e {
        Expr::Binop(b, e1, e2) => {
            string_of_expr(*e1) + " " + &string_of_binop(b) + " " + &string_of_expr(*e2)
        }
        Expr::Const(c) => string_of_const(c),
        Expr::Unop(u, e) => string_of_unop(u) + &string_of_expr(*e),
        Expr::Var(s) => s,
        Expr::Input => "input".to_owned(),
        Expr::Call(name, args) => format!(
            "{}({})",
            name,
            args.into_iter()
                .map(string_of_expr)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Expr::Pred(_name, _args, body) => string_of_expr(*body),
    }
}

fn string_of_target(name: String, e: ast::Expr) -> String {
    match e {
        ast::Expr::Pred(_name, args, _body) => name + "(" + &args.join(",") + ")",
        _ => name,
    }
}

fn string_of_command(cmd: ast::Command) -> String {
    use ast::Command;
    match cmd {
        Command::Timestep => "---".to_owned(),
        Command::Global(v, e) => string_of_target(v, e.clone()) + " = " + &string_of_expr(e),
        Command::Next(v, e) => string_of_target(v, e.clone()) + " <X " + &string_of_expr(e),
        Command::Update(v, e) => string_of_target(v, e.clone()) + " <- " + &string_of_expr(e),
        Command::Finally(v, e) => string_of_target(v, e.clone()) + " <.. " + &string_of_expr(e),
        Command::Print(e) => "print ".to_owned() + &string_of_expr(e),
        Command::Assert(c) => "assert ".to_owned() + &string_of_command(*c),
        Command::Dist(e) => "dist ".to_owned() + &string_of_expr(e),
    }
}

pub fn string_of_program(p: ast::Program) -> String {
    let mut result = "Program\n".to_owned();
    for cmd in p {
        result += &(string_of_command(cmd) + "\n");
    }
    result
}
