use std::collections::{HashMap, HashSet};

use crate::types;

#[derive(Clone, Hash, Debug, PartialEq)]
pub enum Const {
    Bool(bool),
    Number(i64),
    // pdf
}

pub type Var = String;

#[derive(Clone, Hash, Debug, PartialEq)]
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

#[derive(Clone, Hash, Debug, PartialEq)]
pub enum Unop {
    Neg,
    Not,
}

#[derive(Clone, Hash, Debug, PartialEq)]
pub enum Expr {
    EConst(Const),
    EVar(Var),
    EBinop(Binop, Box<Expr>, Box<Expr>),
    EUnop(Unop, Box<Expr>),
    EInput,
    ECall(Var, Vec<Expr>),
    EPred(Var, Vec<Var>, Box<Expr>),
}

#[derive(Clone, Hash, Debug, PartialEq)]
pub enum Command {
    Timestep,
    Global(Var, Expr),
    Next(Var, Expr),
    Update(Var, Expr),
    Finally(Var, Expr),
    Print(Expr),
}

pub type Program = Vec<Command>;

#[derive(Clone, Hash, Debug, PartialEq)]
pub enum TypedExpr {
    TEConst(Const, types::Type),
    TEVar(Var, types::Type),
    TEBinop(Binop, Box<TypedExpr>, Box<TypedExpr>, types::Type),
    TEUnop(Unop, Box<TypedExpr>, types::Type),
    TEInput(types::Type),
    TECall(Var, Vec<TypedExpr>, types::Type),
    // Infer type of parameters?
    TEPred(Var, Vec<Var>, Box<TypedExpr>, types::Type),
}

#[derive(Clone, Hash, Debug, PartialEq)]
pub enum TypedCommand {
    TGlobal(Var, TypedExpr),
    TNext(Var, TypedExpr),
    TUpdate(Var, TypedExpr),
    TFinally(Var, TypedExpr),
    TPrint(TypedExpr),
}

// `y` is the free var in `f(x) = x * y`
pub type FreeVars = HashSet<Var>;
// only counts `current` since those are the only immediately available.
pub type DefVars = HashSet<Var>;

pub type TypedTimeBlock = Vec<TypedCommand>;

#[derive(Clone, Debug)]
pub struct TypedProgram {
    pub code: Vec<TypedTimeBlock>,
    pub udep_map: HashMap<u64, TypedExpr>,
}

pub fn type_of_typedexpr(e: TypedExpr) -> types::Type {
    match e {
        TypedExpr::TEConst(_, t) => t,
        TypedExpr::TEVar(_, t) => t,
        TypedExpr::TEBinop(_, _, _, t) => t,
        TypedExpr::TEUnop(_, _, t) => t,
        TypedExpr::TEInput(t) => t,
        TypedExpr::TECall(_, _, t) => t,
        TypedExpr::TEPred(_, _, _, t) => t,
    }
}
