#[derive(Clone)]
pub enum Const {
    Bool(bool),
    Number(i64),
}

pub type Var = String;

#[derive(Clone)]
pub enum Binop {
    Plus,
    Minus,
    Times,
    Div,
    Or,
    And,
    Until,
    SUntil,
}

#[derive(Clone)]
pub enum Unop {
    Neg,
    Not
}

#[derive(Clone)]
pub enum Expr {
    EConst(Const),
    EVar(Var),
    EBinop(Binop, Box<Expr>, Box<Expr>),
    EUnop(Unop, Box<Expr>),
    Input,
}

#[derive(Clone)]
pub enum Command {
    Timestep,
    Global(Var, Expr),
    Next(Var, Expr),
    Update(Var, Expr),
    Finally(Var, Expr),
    Print(Expr),
}

pub type Program = Vec<Command>;