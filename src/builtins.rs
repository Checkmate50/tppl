/*
    I only need `lt` and `eq` since I could define the rest in a `prelude.tppl`.
    eq,&&& neq, lt, lte, gt, gte
*/

use std::collections::HashSet;

use crate::{arithmetic, ast, errors, types};

pub fn is_builtin(name: &String) -> bool {
    let builtins = HashSet::from([
        "eq", "neq", "lt", "lte", "gt", "gte", "uniform", "normal", "sample",
    ]);
    builtins.contains(name.as_str())
}

// pub fn ret_of_builtin(name: String) -> Option<types::SimpleType> {
//     match name.as_str() {
//         "eq" | "neq" | "lt" | "lte" | "gt" | "gte" => Some(types::SimpleType::Bool),
//         "uniform" | "normal" => Some(types::SimpleType::Pdf),
//         _ => None,
//     }
// }

pub fn typecheck_builtin(
    name: String,
    args: Vec<types::SimpleType>,
) -> Result<types::SimpleType, errors::TypeError> {
    /*
        Some(true) => builtin_cmp exists && the types are aight
        Some(false) => builtin_cmp exists BUT the types are bad
        None => builtin_cmp doesn't exist
    */
    use types::SimpleType;

    match name.as_str() {
        "eq" | "neq" => {
            if args.len() != 2 {
                Err(errors::ImproperCallError {
                    message: format!(
                        "Built-in Predicate {} requires 2 arguments but {} were provided.",
                        name,
                        args.len()
                    )
                    .to_string(),
                })?
            }
            match (args.get(0).expect("The if-length=2 check directly above failed...").to_owned(), args.get(1).expect("The if-length=2 check directly above failed...").to_owned()) {
                (SimpleType::Undefined, _) => Ok(SimpleType::Bool),
                (_, SimpleType::Undefined) => Ok(SimpleType::Bool),
                (a, b) if a == b => Ok(SimpleType::Bool),
                (SimpleType::Int, SimpleType::Float) => Ok(SimpleType::Bool),
                (SimpleType::Float, SimpleType::Int) => Ok(SimpleType::Bool),
                _ => Err(errors::ImproperCallError {message:format!("Built-in Predicate {} is of type `'a -> 'a -> bool` but {:?} were provided.", name, args).to_string()})?,
            }
        }
        "lt" | "lte" | "gt" | "gte" => {
            if args.len() != 2 {
                Err(errors::ImproperCallError {
                    message: format!(
                        "Built-in Predicate {} requires 2 arguments but {} were provided.",
                        name,
                        args.len()
                    )
                    .to_string(),
                })?
            }
            match (args.get(0).expect("The if-length=2 check directly above failed...").to_owned(), args.get(1).expect("The if-length=2 check directly above failed...").to_owned()) {
                (SimpleType::Undefined, _) => Ok(SimpleType::Bool),
                (_, SimpleType::Undefined) => Ok(SimpleType::Bool),
                (SimpleType::Int | SimpleType::Float, SimpleType::Int | SimpleType::Float) => Ok(SimpleType::Bool),
                _ => Err(errors::ImproperCallError {message:format!("Built-in Predicate {} is of type `int -> int -> bool` but {:?} were provided.", name, args).to_string()})?,
            }
        }
        "uniform" | "normal" => {
            if args.len() != 2 {
                Err(errors::ImproperCallError {
                    message: format!(
                        "Built-in Predicate {} requires 2 arguments but {} were provided.",
                        name,
                        args.len()
                    )
                    .to_string(),
                })?
            }
            match (args.get(0).expect("The if-length=2 check directly above failed...").to_owned(), args.get(1).expect("The if-length=2 check directly above failed...").to_owned()) {
                (SimpleType::Undefined, _) => Ok(SimpleType::Pdf),
                (_, SimpleType::Undefined) => Ok(SimpleType::Pdf),
                (SimpleType::Int | SimpleType::Float | SimpleType::Pdf, SimpleType::Int | SimpleType::Float | SimpleType::Pdf) => Ok(SimpleType::Pdf),
                _ => Err(errors::ImproperCallError {message:format!("Built-in Predicate {} is of type `int -> int -> bool` but {:?} were provided.", name, args).to_string()})?,
            }
        }
        "sample" => {
            if args.len() != 1 {
                Err(errors::ImproperCallError {
                    message: format!(
                        "Built-in Predicate {} requires 1 argument but {} were provided.",
                        name,
                        args.len()
                    )
                    .to_string(),
                })?
            }
            match args
                .get(0)
                .expect("The if-length=2 check directly above failed...")
                .to_owned()
            {
                SimpleType::Undefined => Ok(SimpleType::Float),
                SimpleType::Pdf => Ok(SimpleType::Float),
                _ => Err(errors::ImproperCallError {
                    message: format!(
                        "Built-in Predicate {} is of type `pdf -> float` but {:?} were provided.",
                        name, args
                    )
                    .to_string(),
                })?,
            }
        }
        _ => panic!("Name {} is not a built-in predicate...", name),
    }
}

pub fn exec_builtin(
    name: String,
    args: Vec<ast::TypedExpr>,
) -> Result<ast::TypedExpr, errors::SimpleConflictError> {
    let args: Vec<ast::Const> = args
        .into_iter()
        .map(|arg| match arg {
            ast::TypedExpr::TEConst(c, _) => c,
            _ => panic!("CMP should only be given `ast::TypedExpr::TEConst`s"),
        })
        .collect();

    // arity checks should've been done at CompileTime
    let cmp = match name.as_str() {
        "eq" => {
            builtin_eq as fn(Vec<ast::Const>) -> Result<ast::Const, errors::SimpleConflictError>
        }
        "neq" => {
            builtin_neq as fn(Vec<ast::Const>) -> Result<ast::Const, errors::SimpleConflictError>
        }
        "lt" => {
            builtin_lt as fn(Vec<ast::Const>) -> Result<ast::Const, errors::SimpleConflictError>
        }
        "lte" => {
            builtin_lte as fn(Vec<ast::Const>) -> Result<ast::Const, errors::SimpleConflictError>
        }
        "gt" => {
            builtin_gt as fn(Vec<ast::Const>) -> Result<ast::Const, errors::SimpleConflictError>
        }
        "gte" => {
            builtin_gte as fn(Vec<ast::Const>) -> Result<ast::Const, errors::SimpleConflictError>
        }
        "uniform" => {
            builtin_uniform
                as fn(Vec<ast::Const>) -> Result<ast::Const, errors::SimpleConflictError>
        }
        "normal" => {
            builtin_normal as fn(Vec<ast::Const>) -> Result<ast::Const, errors::SimpleConflictError>
        }
        "sample" => {
            builtin_sample as fn(Vec<ast::Const>) -> Result<ast::Const, errors::SimpleConflictError>
        }
        _ => panic!("Name {} is not a built-in predicate...", name),
    };

    let returned_const = cmp(args)?;
    Ok(ast::TypedExpr::TEConst(
        returned_const.clone(),
        ast::type_of_constant(returned_const),
    ))
}

pub fn builtin_eq(args: Vec<ast::Const>) -> Result<ast::Const, errors::SimpleConflictError> {
    let a = args
        .get(0)
        .expect("Arity check (typecheck_builtin) failed?")
        .to_owned();
    let b = args
        .get(1)
        .expect("Arity check (typecheck_builtin) failed?")
        .to_owned();
    match (a.clone(), b.clone()) {
        (ast::Const::Bool(b1), ast::Const::Bool(b2)) => Ok(ast::Const::Bool(b1 == b2)),
        (ast::Const::Number(n1), ast::Const::Number(n2)) => Ok(ast::Const::Bool(n1 == n2)),
        (ast::Const::Number(n1), ast::Const::Float(f2)) => Ok(ast::Const::Bool((n1 as f64) == f2)),
        (ast::Const::Float(f1), ast::Const::Number(n2)) => Ok(ast::Const::Bool(f1 == (n2 as f64))),
        (ast::Const::Float(f1), ast::Const::Float(f2)) => Ok(ast::Const::Bool(f1 == f2)),
        // (ast::Const::Pdf(d1), ast::Const::Pdf(d2)) =>
        (_, _) => Err(errors::SimpleConflictError {
            message: format!(
                "{} and {} are not comparable with eq",
                ast::string_of_const_type(&a),
                ast::string_of_const_type(&b)
            )
            .to_string(),
        }),
    }
}

fn builtin_neq(args: Vec<ast::Const>) -> Result<ast::Const, errors::SimpleConflictError> {
    let a = args
        .get(0)
        .expect("Arity check (typecheck_builtin) failed?")
        .to_owned();
    let b = args
        .get(1)
        .expect("Arity check (typecheck_builtin) failed?")
        .to_owned();
    match (a.clone(), b.clone()) {
        (ast::Const::Bool(b1), ast::Const::Bool(b2)) => Ok(ast::Const::Bool(b1 != b2)),
        (ast::Const::Number(n1), ast::Const::Number(n2)) => Ok(ast::Const::Bool(n1 != n2)),
        (ast::Const::Number(n1), ast::Const::Float(f2)) => Ok(ast::Const::Bool((n1 as f64) != f2)),
        (ast::Const::Float(f1), ast::Const::Number(n2)) => Ok(ast::Const::Bool(f1 != (n2 as f64))),
        (ast::Const::Float(f1), ast::Const::Float(f2)) => Ok(ast::Const::Bool(f1 != f2)),
        (_, _) => Err(errors::SimpleConflictError {
            message: format!(
                "{} and {} are not comparable with neq",
                ast::string_of_const_type(&a),
                ast::string_of_const_type(&b)
            )
            .to_string(),
        }),
    }
}

fn builtin_lt(args: Vec<ast::Const>) -> Result<ast::Const, errors::SimpleConflictError> {
    let a = args
        .get(0)
        .expect("Arity check (typecheck_builtin) failed?")
        .to_owned();
    let b = args
        .get(1)
        .expect("Arity check (typecheck_builtin) failed?")
        .to_owned();
    match (a.clone(), b.clone()) {
        (ast::Const::Number(n1), ast::Const::Number(n2)) => Ok(ast::Const::Bool(n1 < n2)),
        (ast::Const::Number(n1), ast::Const::Float(f2)) => Ok(ast::Const::Bool((n1 as f64) < f2)),
        (ast::Const::Float(f1), ast::Const::Number(n2)) => Ok(ast::Const::Bool(f1 < (n2 as f64))),
        (ast::Const::Float(f1), ast::Const::Float(f2)) => Ok(ast::Const::Bool(f1 < f2)),
        (_, _) => Err(errors::SimpleConflictError {
            message: format!(
                "{} and {} are not comparable with lt",
                ast::string_of_const_type(&a),
                ast::string_of_const_type(&b)
            )
            .to_string(),
        }),
    }
}

fn builtin_lte(args: Vec<ast::Const>) -> Result<ast::Const, errors::SimpleConflictError> {
    let a = args
        .get(0)
        .expect("Arity check (typecheck_builtin) failed?")
        .to_owned();
    let b = args
        .get(1)
        .expect("Arity check (typecheck_builtin) failed?")
        .to_owned();
    match (a.clone(), b.clone()) {
        (ast::Const::Number(n1), ast::Const::Number(n2)) => Ok(ast::Const::Bool(n1 <= n2)),
        (ast::Const::Number(n1), ast::Const::Float(f2)) => Ok(ast::Const::Bool((n1 as f64) <= f2)),
        (ast::Const::Float(f1), ast::Const::Number(n2)) => Ok(ast::Const::Bool(f1 <= (n2 as f64))),
        (ast::Const::Float(f1), ast::Const::Float(f2)) => Ok(ast::Const::Bool(f1 <= f2)),
        (_, _) => Err(errors::SimpleConflictError {
            message: format!(
                "{} and {} are not comparable with lte",
                ast::string_of_const_type(&a),
                ast::string_of_const_type(&b)
            )
            .to_string(),
        }),
    }
}

fn builtin_gt(args: Vec<ast::Const>) -> Result<ast::Const, errors::SimpleConflictError> {
    let a = args
        .get(0)
        .expect("Arity check (typecheck_builtin) failed?")
        .to_owned();
    let b = args
        .get(1)
        .expect("Arity check (typecheck_builtin) failed?")
        .to_owned();
    match (a.clone(), b.clone()) {
        (ast::Const::Number(n1), ast::Const::Number(n2)) => Ok(ast::Const::Bool(n1 > n2)),
        (ast::Const::Number(n1), ast::Const::Float(f2)) => Ok(ast::Const::Bool((n1 as f64) > f2)),
        (ast::Const::Float(f1), ast::Const::Number(n2)) => Ok(ast::Const::Bool(f1 > (n2 as f64))),
        (ast::Const::Float(f1), ast::Const::Float(f2)) => Ok(ast::Const::Bool(f1 > f2)),
        (_, _) => Err(errors::SimpleConflictError {
            message: format!(
                "{} and {} are not comparable with gt",
                ast::string_of_const_type(&a),
                ast::string_of_const_type(&b)
            )
            .to_string(),
        }),
    }
}

fn builtin_gte(args: Vec<ast::Const>) -> Result<ast::Const, errors::SimpleConflictError> {
    let a = args
        .get(0)
        .expect("Arity check (typecheck_builtin) failed?")
        .to_owned();
    let b = args
        .get(1)
        .expect("Arity check (typecheck_builtin) failed?")
        .to_owned();
    match (a.clone(), b.clone()) {
        (ast::Const::Number(n1), ast::Const::Number(n2)) => Ok(ast::Const::Bool(n1 >= n2)),
        (ast::Const::Number(n1), ast::Const::Float(f2)) => Ok(ast::Const::Bool((n1 as f64) >= f2)),
        (ast::Const::Float(f1), ast::Const::Number(n2)) => Ok(ast::Const::Bool(f1 >= (n2 as f64))),
        (ast::Const::Float(f1), ast::Const::Float(f2)) => Ok(ast::Const::Bool(f1 >= f2)),
        (_, _) => Err(errors::SimpleConflictError {
            message: format!(
                "{} and {} are not comparable with gte",
                ast::string_of_const_type(&a),
                ast::string_of_const_type(&b)
            )
            .to_string(),
        }),
    }
}

fn builtin_uniform(args: Vec<ast::Const>) -> Result<ast::Const, errors::SimpleConflictError> {
    let a = args
        .get(0)
        .expect("Arity check (typecheck_builtin) failed?")
        .to_owned();
    let b = args
        .get(1)
        .expect("Arity check (typecheck_builtin) failed?")
        .to_owned();
    match (a.clone(), b.clone()) {
        (
            ast::Const::Number(_) | ast::Const::Float(_) | ast::Const::Pdf(_),
            ast::Const::Number(_) | ast::Const::Float(_) | ast::Const::Pdf(_),
        ) => Ok(ast::Const::Pdf(ast::Distribution::Uniform(
            Box::new(a),
            Box::new(b),
        ))),
        (_, _) => Err(errors::SimpleConflictError {
            message: format!(
                "{} and {} are not comparable with uniform",
                ast::string_of_const_type(&a),
                ast::string_of_const_type(&b)
            )
            .to_string(),
        }),
    }
}

fn builtin_normal(args: Vec<ast::Const>) -> Result<ast::Const, errors::SimpleConflictError> {
    let a = args
        .get(0)
        .expect("Arity check (typecheck_builtin) failed?")
        .to_owned();
    let b = args
        .get(1)
        .expect("Arity check (typecheck_builtin) failed?")
        .to_owned();
    match (a.clone(), b.clone()) {
        (
            ast::Const::Number(_) | ast::Const::Float(_) | ast::Const::Pdf(_),
            ast::Const::Number(_) | ast::Const::Float(_) | ast::Const::Pdf(_),
        ) => Ok(ast::Const::Pdf(ast::Distribution::Normal(
            Box::new(a),
            Box::new(b),
        ))),
        (_, _) => Err(errors::SimpleConflictError {
            message: format!(
                "{} and {} are not comparable with normal",
                ast::string_of_const_type(&a),
                ast::string_of_const_type(&b)
            )
            .to_string(),
        }),
    }
}

fn builtin_sample(args: Vec<ast::Const>) -> Result<ast::Const, errors::SimpleConflictError> {
    let a = args
        .get(0)
        .expect("Arity check (typecheck_builtin) failed?")
        .to_owned();
    match a {
        ast::Const::Pdf(d) => Ok(ast::Const::Float(arithmetic::spam_sample(d, 1)[0])),
        _ => Err(errors::SimpleConflictError {
            message: format!("{} cannot be sampled", ast::string_of_const_type(&a),).to_string(),
        }),
    }
}
