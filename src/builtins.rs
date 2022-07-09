/*
    I only need `lt` and `eq` since I could define the rest in a `prelude.tppl`.
    eq,&&& neq, lt, lte, gt, gte
*/

use std::collections::HashSet;

use crate::{ast, errors, types};

pub fn is_builtin(name: &String) -> bool {
    let builtins = HashSet::from(["eq", "neq", "lt", "lte", "gt", "gte"]);
    builtins.contains(name.as_str())
}

pub fn ret_of_builtin_cmp(name: String) -> Option<types::SimpleType> {
    match name.as_str() {
        "eq" | "neq" | "lt" | "lte" | "gt" | "gte" => Some(types::SimpleType::Bool),
        _ => None,
    }
}

pub fn typecheck_builtin_cmp(
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
            match (args.get(0).unwrap().to_owned(), args.get(1).unwrap().to_owned()) {
                (SimpleType::Option(a), b) => typecheck_builtin_cmp(name.clone(), vec![*a, b]),
                (a, SimpleType::Option(b)) => typecheck_builtin_cmp(name.clone(), vec![a, *b]),
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
            match (args.get(0).unwrap().to_owned(), args.get(1).unwrap().to_owned()) {
                (SimpleType::Option(a), b) => typecheck_builtin_cmp(name.clone(), vec![*a, b]),
                (a, SimpleType::Option(b)) => typecheck_builtin_cmp(name.clone(), vec![a, *b]),
                (SimpleType::Undefined, _) => Ok(SimpleType::Bool),
                (_, SimpleType::Undefined) => Ok(SimpleType::Bool),
                (SimpleType::Int, SimpleType::Int) => Ok(SimpleType::Bool),
                (SimpleType::Int, SimpleType::Float) => Ok(SimpleType::Bool),
                (SimpleType::Float, SimpleType::Int) => Ok(SimpleType::Bool),
                (SimpleType::Float, SimpleType::Float) => Ok(SimpleType::Bool),
                _ => Err(errors::ImproperCallError {message:format!("Built-in Predicate {} is of type `int -> int -> bool` but {:?} were provided.", name, args).to_string()})?,
            }
        }
        _ => panic!("Name {} is not a built-in predicate...", name),
    }
}

pub fn exec_builtin_cmp(
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
        _ => panic!("Name {} is not a built-in predicate...", name),
    };

    let returned_const = cmp(args)?;
    Ok(ast::TypedExpr::TEConst(
        returned_const.clone(),
        ast::type_of_constant(returned_const),
    ))
}

fn string_of_const_type(c: &ast::Const) -> String {
    match c {
        ast::Const::Bool(_) => "Bool".to_string(),
        ast::Const::Float(_) => "Float".to_string(),
        ast::Const::Number(_) => "Number".to_string(),
    }
}

pub fn builtin_eq(args: Vec<ast::Const>) -> Result<ast::Const, errors::SimpleConflictError> {
    let a = args.get(0).unwrap().to_owned();
    let b = args.get(1).unwrap().to_owned();
    match (a.clone(), b.clone()) {
        (ast::Const::Bool(b1), ast::Const::Bool(b2)) => Ok(ast::Const::Bool(b1 == b2)),
        (ast::Const::Number(n1), ast::Const::Number(n2)) => Ok(ast::Const::Bool(n1 == n2)),
        (ast::Const::Number(n1), ast::Const::Float(f2)) => Ok(ast::Const::Bool((n1 as f64) == f2)),
        (ast::Const::Float(f1), ast::Const::Number(n2)) => Ok(ast::Const::Bool(f1 == (n2 as f64))),
        (ast::Const::Float(f1), ast::Const::Float(f2)) => Ok(ast::Const::Bool(f1 == f2)),
        (_, _) => Err(errors::SimpleConflictError {
            message: format!(
                "{} and {} are not comparable with eq",
                string_of_const_type(&a),
                string_of_const_type(&b)
            )
            .to_string(),
        }),
    }
}

pub fn builtin_neq(args: Vec<ast::Const>) -> Result<ast::Const, errors::SimpleConflictError> {
    let a = args.get(0).unwrap().to_owned();
    let b = args.get(1).unwrap().to_owned();
    match (a.clone(), b.clone()) {
        (ast::Const::Bool(b1), ast::Const::Bool(b2)) => Ok(ast::Const::Bool(b1 != b2)),
        (ast::Const::Number(n1), ast::Const::Number(n2)) => Ok(ast::Const::Bool(n1 != n2)),
        (ast::Const::Number(n1), ast::Const::Float(f2)) => Ok(ast::Const::Bool((n1 as f64) != f2)),
        (ast::Const::Float(f1), ast::Const::Number(n2)) => Ok(ast::Const::Bool(f1 != (n2 as f64))),
        (ast::Const::Float(f1), ast::Const::Float(f2)) => Ok(ast::Const::Bool(f1 != f2)),
        (_, _) => Err(errors::SimpleConflictError {
            message: format!(
                "{} and {} are not comparable with neq",
                string_of_const_type(&a),
                string_of_const_type(&b)
            )
            .to_string(),
        }),
    }
}

pub fn builtin_lt(args: Vec<ast::Const>) -> Result<ast::Const, errors::SimpleConflictError> {
    let a = args.get(0).unwrap().to_owned();
    let b = args.get(1).unwrap().to_owned();
    match (a.clone(), b.clone()) {
        (ast::Const::Number(n1), ast::Const::Number(n2)) => Ok(ast::Const::Bool(n1 < n2)),
        (ast::Const::Number(n1), ast::Const::Float(f2)) => Ok(ast::Const::Bool((n1 as f64) < f2)),
        (ast::Const::Float(f1), ast::Const::Number(n2)) => Ok(ast::Const::Bool(f1 < (n2 as f64))),
        (ast::Const::Float(f1), ast::Const::Float(f2)) => Ok(ast::Const::Bool(f1 < f2)),
        (_, _) => Err(errors::SimpleConflictError {
            message: format!(
                "{} and {} are not comparable with lt",
                string_of_const_type(&a),
                string_of_const_type(&b)
            )
            .to_string(),
        }),
    }
}

pub fn builtin_lte(args: Vec<ast::Const>) -> Result<ast::Const, errors::SimpleConflictError> {
    let a = args.get(0).unwrap().to_owned();
    let b = args.get(1).unwrap().to_owned();
    match (a.clone(), b.clone()) {
        (ast::Const::Number(n1), ast::Const::Number(n2)) => Ok(ast::Const::Bool(n1 <= n2)),
        (ast::Const::Number(n1), ast::Const::Float(f2)) => Ok(ast::Const::Bool((n1 as f64) <= f2)),
        (ast::Const::Float(f1), ast::Const::Number(n2)) => Ok(ast::Const::Bool(f1 <= (n2 as f64))),
        (ast::Const::Float(f1), ast::Const::Float(f2)) => Ok(ast::Const::Bool(f1 <= f2)),
        (_, _) => Err(errors::SimpleConflictError {
            message: format!(
                "{} and {} are not comparable with lte",
                string_of_const_type(&a),
                string_of_const_type(&b)
            )
            .to_string(),
        }),
    }
}

pub fn builtin_gt(args: Vec<ast::Const>) -> Result<ast::Const, errors::SimpleConflictError> {
    let a = args.get(0).unwrap().to_owned();
    let b = args.get(1).unwrap().to_owned();
    match (a.clone(), b.clone()) {
        (ast::Const::Number(n1), ast::Const::Number(n2)) => Ok(ast::Const::Bool(n1 > n2)),
        (ast::Const::Number(n1), ast::Const::Float(f2)) => Ok(ast::Const::Bool((n1 as f64) > f2)),
        (ast::Const::Float(f1), ast::Const::Number(n2)) => Ok(ast::Const::Bool(f1 > (n2 as f64))),
        (ast::Const::Float(f1), ast::Const::Float(f2)) => Ok(ast::Const::Bool(f1 > f2)),
        (_, _) => Err(errors::SimpleConflictError {
            message: format!(
                "{} and {} are not comparable with gt",
                string_of_const_type(&a),
                string_of_const_type(&b)
            )
            .to_string(),
        }),
    }
}

pub fn builtin_gte(args: Vec<ast::Const>) -> Result<ast::Const, errors::SimpleConflictError> {
    let a = args.get(0).unwrap().to_owned();
    let b = args.get(1).unwrap().to_owned();
    match (a.clone(), b.clone()) {
        (ast::Const::Number(n1), ast::Const::Number(n2)) => Ok(ast::Const::Bool(n1 >= n2)),
        (ast::Const::Number(n1), ast::Const::Float(f2)) => Ok(ast::Const::Bool((n1 as f64) >= f2)),
        (ast::Const::Float(f1), ast::Const::Number(n2)) => Ok(ast::Const::Bool(f1 >= (n2 as f64))),
        (ast::Const::Float(f1), ast::Const::Float(f2)) => Ok(ast::Const::Bool(f1 >= f2)),
        (_, _) => Err(errors::SimpleConflictError {
            message: format!(
                "{} and {} are not comparable with gte",
                string_of_const_type(&a),
                string_of_const_type(&b)
            )
            .to_string(),
        }),
    }
}
