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

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Unop {
    Neg,
    Not,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Const(Const),
    Var(Var),
    Binop(Binop, Box<Expr>, Box<Expr>),
    Unop(Unop, Box<Expr>),
    Input,
    Call(Var, Vec<Expr>),
    Pred(Var, Vec<Var>, Box<Expr>),
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
    Const(Const, types::Type),
    Var(Var, types::Type),
    Binop(Binop, Box<TypedExpr>, Box<TypedExpr>, types::Type),
    Unop(Unop, Box<TypedExpr>, types::Type),
    Input(types::Type),
    Call(Var, Vec<TypedExpr>, types::Type),
    // Infer type of parameters?
    Pred(Var, Vec<Var>, Box<TypedExpr>, types::Type),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypedCommand {
    Global(Var, TypedExpr),
    Next(Var, TypedExpr),
    Update(Var, TypedExpr),
    Finally(Var, TypedExpr),
    Print(TypedExpr),
    Assert(Box<TypedCommand>),
    Dist(TypedExpr),
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
        TypedExpr::Const(_, t) => t,
        TypedExpr::Var(_, t) => t,
        TypedExpr::Binop(_, _, _, t) => t,
        TypedExpr::Unop(_, _, t) => t,
        TypedExpr::Input(t) => t,
        TypedExpr::Call(_, _, t) => t,
        TypedExpr::Pred(_, _, _, t) => t,
    }
}

pub fn const_of_texpr(te: TypedExpr) -> Const {
    match te {
        TypedExpr::Const(c, _) => c,
        _ => panic!("That ain't a constant."),
    }
}

pub fn type_of_constant(c: Const) -> types::Type {
    let temporal_undefined = types::TemporalType {
        when_available: types::TemporalAvailability::Undefined,
        when_dissipates: types::TemporalPersistency::Undefined,
        is_until: types::UntilDependencies {
            weak: Vec::new(),
            strong: Vec::new(),
        },
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

    match e {
        TypedExpr::Const(c, old_type) => {
            let t = constrain(&old_type, &constraint, msg)?;
            Ok(TypedExpr::Const(c, t))
        }
        TypedExpr::Var(v, old_type) => {
            let t = constrain(&old_type, &constraint, msg)?;
            Ok(TypedExpr::Var(v, t))
        }
        TypedExpr::Binop(b, e1, e2, old_type) => {
            let t = constrain(&old_type, &constraint, msg)?;
            Ok(TypedExpr::Binop(b, e1, e2, t))
        }
        TypedExpr::Unop(u, e1, old_type) => {
            let t = constrain(&old_type, &constraint, msg)?;
            Ok(TypedExpr::Unop(u, e1, t))
        }
        TypedExpr::Input(old_type) => {
            let t = constrain(&old_type, &constraint, msg)?;
            Ok(TypedExpr::Input(t))
        }
        TypedExpr::Call(name, args, old_type) => {
            let t = constrain(&old_type, &constraint, msg)?;
            Ok(TypedExpr::Call(name, args, t))
        }
        TypedExpr::Pred(name, args, body, old_type) => {
            let t = constrain(&old_type, &constraint, msg)?;
            Ok(TypedExpr::Pred(name, args, body, t))
        }
    }
}

pub fn expr_of_texpr(te: TypedExpr) -> Expr {
    match te {
        TypedExpr::Const(c, _) => Expr::Const(c),
        TypedExpr::Var(v, _) => Expr::Var(v),
        TypedExpr::Binop(b, e1, e2, _) => Expr::Binop(
            b,
            Box::new(expr_of_texpr(*e1)),
            Box::new(expr_of_texpr(*e2)),
        ),
        TypedExpr::Unop(u, e1, _) => Expr::Unop(u, Box::new(expr_of_texpr(*e1))),
        TypedExpr::Input(_) => Expr::Input,
        TypedExpr::Call(name, args, _) => Expr::Call(
            name,
            args.into_iter().map(expr_of_texpr).collect::<Vec<Expr>>(),
        ),
        TypedExpr::Pred(name, args, body, _) => {
            Expr::Pred(name, args, Box::new(expr_of_texpr(*body)))
        }
    }
}

// this is to help handle `assert`s since `asssert x = 1 <!> cond` executes `1 <!> cond` everytime the assertion is checked,
// which can lead to `Error: UntilCond is True at the time of evaluation`
pub fn strip_untils_off_texpr(te: TypedExpr) -> TypedExpr {
    match te.clone() {
        TypedExpr::Const(_, _) => te,
        TypedExpr::Var(_, _) => te,
        TypedExpr::Binop(b, op1, op2, t) => match b {
            Binop::SUntil | Binop::Until => strip_untils_off_texpr(*op1),
            _ => TypedExpr::Binop(
                b,
                Box::new(strip_untils_off_texpr(*op1)),
                Box::new(strip_untils_off_texpr(*op2)),
                t,
            ),
        },
        TypedExpr::Unop(u, op1, t) => TypedExpr::Unop(u, Box::new(strip_untils_off_texpr(*op1)), t),
        TypedExpr::Input(_) => te,
        TypedExpr::Call(_, _, _) => te,
        TypedExpr::Pred(_, _, _, _) => te,
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
