use std::collections::{HashMap, HashSet};

use crate::{errors, types};

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

pub fn type_of_constant(c: Const) -> types::Type {
    let temporal_undefined = types::TemporalType {
        when_available: types::TemporalAvailability::Undefined,
        when_dissipates: types::TemporalPersistency::Undefined,
        is_until: None,
    };

    match c {
        Const::Number(_) => types::Type(temporal_undefined, types::SimpleType::Int),
        Const::Bool(_) => types::Type(temporal_undefined, types::SimpleType::Bool),
    }
}

pub fn new_texpr(
    e: TypedExpr,
    constraint: types::Type,
    msg: String,
) -> Result<TypedExpr, errors::ConstrainError> {
    use types::constrain;

    match e.clone() {
        TypedExpr::TEConst(c, old_type) => {
            if c == Const::Bool(false) && constraint.get_simpl().is_option() {
                Ok(e)
            } else {
                let t = constrain(&old_type, &constraint, msg)?;
                Ok(TypedExpr::TEConst(c, t))
            }
        }
        TypedExpr::TEVar(v, old_type) => {
            let t = constrain(&old_type, &constraint, msg)?;
            Ok(TypedExpr::TEVar(v, t))
        }
        TypedExpr::TEBinop(b, e1, e2, old_type) => {
            let t = constrain(&old_type, &constraint, msg)?;
            Ok(TypedExpr::TEBinop(b, Box::new(*e1), Box::new(*e2), t))
        }
        TypedExpr::TEUnop(u, e1, old_type) => {
            let t = constrain(&old_type, &constraint, msg)?;
            Ok(TypedExpr::TEUnop(u, Box::new(*e1), t))
        }
        TypedExpr::TEInput(old_type) => {
            let t = constrain(&old_type, &constraint, msg)?;
            Ok(TypedExpr::TEInput(t))
        }
        TypedExpr::TECall(name, args, old_type) => {
            let t = constrain(&old_type, &constraint, msg)?;
            Ok(TypedExpr::TECall(name, args, t))
        }
        TypedExpr::TEPred(name, args, body, old_type) => {
            let t = constrain(&old_type, &constraint, msg)?;
            Ok(TypedExpr::TEPred(name, args, body, t))
        }
    }
}

pub fn strip_types_off_texpr(te: TypedExpr) -> Expr {
    match te {
        TypedExpr::TEConst(c, _) => Expr::EConst(c),
        TypedExpr::TEVar(v, _) => Expr::EVar(v),
        TypedExpr::TEBinop(b, e1, e2, _) => Expr::EBinop(
            b,
            Box::new(strip_types_off_texpr(*e1)),
            Box::new(strip_types_off_texpr(*e2)),
        ),
        TypedExpr::TEUnop(u, e1, _) => Expr::EUnop(u, Box::new(strip_types_off_texpr(*e1))),
        TypedExpr::TEInput(_) => Expr::EInput,
        TypedExpr::TECall(name, args, _) => Expr::ECall(
            name,
            args.into_iter()
                .map(strip_types_off_texpr)
                .collect::<Vec<Expr>>(),
        ),
        TypedExpr::TEPred(name, args, body, _) => {
            Expr::EPred(name, args, Box::new(strip_types_off_texpr(*body)))
        }
    }
}
