use std::collections::{HashMap, HashSet};

use crate::{errors, types};

#[derive(Clone, Debug, PartialEq)]
pub enum Distribution {
    Uniform(Box<Const>, Box<Const>), // Uniform(lower: f64, upper: f64)
    Normal(Box<Const>, Box<Const>),  // Normal(mean: f64, std_dev: f64)
    List(Vec<f64>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Const {
    Bool(bool),
    Number(i64),
    Float(f64), // pdf
    Pdf(Distribution),
}

pub type Var = String;

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
pub enum Unop {
    Neg,
    Not,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    EConst(Const),
    EVar(Var),
    EBinop(Binop, Box<Expr>, Box<Expr>),
    EUnop(Unop, Box<Expr>),
    EInput,
    ECall(Var, Vec<Expr>),
    EPred(Var, Vec<Var>, Box<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Command {
    Timestep,
    Global(Var, Expr),
    Next(Var, Expr),
    Update(Var, Expr),
    Finally(Var, Expr),
    Print(Expr),
    Assert(Box<Command>),
    Dist(Expr),
}

pub type Program = Vec<Command>;

#[derive(Clone, Debug, PartialEq)]
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

#[derive(Clone, Debug, PartialEq)]
pub enum TypedCommand {
    TGlobal(Var, TypedExpr),
    TNext(Var, TypedExpr),
    TUpdate(Var, TypedExpr),
    TFinally(Var, TypedExpr),
    TPrint(TypedExpr),
    TAssert(Box<TypedCommand>),
    TDist(TypedExpr),
}

// `y` is the free var in `f(x) = x * y`
pub type FreeVars = HashSet<Var>;
// only counts `current` since those are the only immediately available.
pub type DefVars = HashSet<Var>;

pub type TypedTimeBlock = Vec<TypedCommand>;

#[derive(Clone, Debug)]
pub struct TypedProgram {
    pub code: Vec<TypedTimeBlock>,
    pub udep_map: HashMap<usize, TypedExpr>,
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

pub fn const_of_texpr(te: TypedExpr) -> Const {
    match te {
        TypedExpr::TEConst(c, _) => c,
        _ => panic!("That ain't a constant."),
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
        Const::Float(_) => types::Type(temporal_undefined, types::SimpleType::Float),
        Const::Pdf(_) => types::Type(temporal_undefined, types::SimpleType::Pdf),
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
            let t = constrain(&old_type, &constraint, msg)?;
            Ok(TypedExpr::TEConst(c, t))
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

pub fn expr_of_texpr(te: TypedExpr) -> Expr {
    match te {
        TypedExpr::TEConst(c, _) => Expr::EConst(c),
        TypedExpr::TEVar(v, _) => Expr::EVar(v),
        TypedExpr::TEBinop(b, e1, e2, _) => Expr::EBinop(
            b,
            Box::new(expr_of_texpr(*e1)),
            Box::new(expr_of_texpr(*e2)),
        ),
        TypedExpr::TEUnop(u, e1, _) => Expr::EUnop(u, Box::new(expr_of_texpr(*e1))),
        TypedExpr::TEInput(_) => Expr::EInput,
        TypedExpr::TECall(name, args, _) => Expr::ECall(
            name,
            args.into_iter().map(expr_of_texpr).collect::<Vec<Expr>>(),
        ),
        TypedExpr::TEPred(name, args, body, _) => {
            Expr::EPred(name, args, Box::new(expr_of_texpr(*body)))
        }
    }
}

// this is to help handle `assert`s since `asssert x = 1 <!> cond` executes `1 <!> cond` everytime the assertion is checked,
// which can lead to `Error: UntilCond is True at the time of evaluation`
pub fn strip_untils_off_texpr(te: TypedExpr) -> TypedExpr {
    match te.clone() {
        TypedExpr::TEConst(_, _) => te,
        TypedExpr::TEVar(_, _) => te,
        TypedExpr::TEBinop(b, op1, op2, t) => match b {
            Binop::SUntil | Binop::Until => strip_untils_off_texpr(*op1),
            _ => TypedExpr::TEBinop(
                b,
                Box::new(strip_untils_off_texpr(*op1)),
                Box::new(strip_untils_off_texpr(*op2)),
                t,
            ),
        },
        TypedExpr::TEUnop(u, op1, t) => {
            TypedExpr::TEUnop(u, Box::new(strip_untils_off_texpr(*op1)), t)
        }
        TypedExpr::TEInput(_) => te,
        TypedExpr::TECall(_, _, _) => te,
        TypedExpr::TEPred(_, _, _, _) => te,
    }
}

pub fn string_of_const_type(c: &Const) -> String {
    match c {
        Const::Bool(_) => "Bool".to_string(),
        Const::Float(_) => "Float".to_string(),
        Const::Number(_) => "Number".to_string(),
        Const::Pdf(d) => match d {
            Distribution::Uniform(_, _) => "Uniform".to_string(),
            Distribution::Normal(_, _) => "Normal".to_string(),
            Distribution::List(_) => "List".to_string(),
        },
    }
}
