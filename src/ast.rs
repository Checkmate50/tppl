use std::collections::HashSet;

use crate::types;


#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum Const {
    Bool(bool),
    Number(i64),
}

pub type Var = String;

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
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
#[derive(Debug)]
#[derive(PartialEq)]
pub enum Unop {
    Neg,
    Not
}

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum Expr {
    EConst(Const),
    EVar(Var),
    EBinop(Binop, Box<Expr>, Box<Expr>),
    EUnop(Unop, Box<Expr>),
    Input,
}

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum Command {
    Timestep,
    Global(Var, Expr),
    Next(Var, Expr),
    Update(Var, Expr),
    Finally(Var, Expr),
    Print(Expr),
}

pub type Program = Vec<Command>;


#[derive(Clone)]
#[derive(Debug)]
pub enum TypedExpr {
    TEConst(Const, types::Type),
    TEVar(Var, types::Type),
    TEBinop(Binop, Box<TypedExpr>, Box<TypedExpr>, types::Type),
    TEUnop(Unop, Box<TypedExpr>, types::Type),
    TInput(types::Type),
}


#[derive(Clone)]
#[derive(Debug)]
pub enum TypedCommand {
    TGlobal(Var, TypedExpr),
    TNext(Var, TypedExpr),
    TUpdate(Var, TypedExpr),
    TFinally(Var, TypedExpr),
    TPrint(TypedExpr),
}

// `y` is the free var in `f(x) = x * y` 
pub type FreeVars = HashSet<Var>;
// only counts Global/Finally since those are the only immediately available.
pub type DefVars = HashSet<Var>;

pub type TypedTimeBlock = Vec<TypedCommand>;

pub type TypedProgram = Vec<TypedTimeBlock>;