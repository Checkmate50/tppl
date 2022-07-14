use crate::{ast, errors};
use ast::Const;

pub fn add(a: Const, b: Const) -> Result<Const, errors::SimpleConflictError> {
    match (a.clone(), b.clone()) {
        (Const::Number(n1), Const::Number(n2)) => Ok(Const::Number(n1 + n2)),
        (Const::Number(n), Const::Float(f)) | (Const::Float(f), Const::Number(n)) => Ok(add(Const::Float(n as f64), Const::Float(f))?),
        (Const::Float(f1), Const::Float(f2)) => Ok(Const::Float(f1 + f2)),

        (Const::Number(n), Const::Pdf(d)) | (Const::Pdf(d), Const::Number(n)) => {
            Ok(add(Const::Float(n as f64), Const::Pdf(d))?)
        }

        (Const::Float(f), Const::Pdf(d)) | (Const::Pdf(d), Const::Float(f)) => {
            use ast::Distribution;
            let new_d = match d {
                Distribution::Uniform(low, high) => Distribution::Uniform(
                    Box::new(add(*low, Const::Float(f))?),
                    Box::new(add(*high, Const::Float(f))?)
                ),
                Distribution::Normal(mean, std_dev) => Distribution::Normal(
                    Box::new(add(*mean, Const::Float(f))?),
                    std_dev
                )
            };
            Ok(Const::Pdf(new_d))
        }

        (Const::Pdf(_d1), Const::Pdf(_d2)) => {
            Err(errors::SimpleConflictError {
                message: format!(
                    "{} and {} are not comparable with add. PDF * PDF arithmetic ain't implemented yet.",
                    ast::string_of_const_type(&a),
                    ast::string_of_const_type(&b)
                )
                .to_string(),
            })
        }

        (Const::Bool(_), _) | (_, Const::Bool(_)) => Err(errors::SimpleConflictError {
            message: format!(
                "{} and {} are not comparable with add.",
                ast::string_of_const_type(&a),
                ast::string_of_const_type(&b)
            )
            .to_string(),
        }),
    }
}

pub fn sub(a: Const, b: Const) -> Result<Const, errors::SimpleConflictError> {
    match (a.clone(), b.clone()) {
        (Const::Number(n1), Const::Number(n2)) => Ok(Const::Number(n1 - n2)),
        (Const::Number(n), Const::Float(f)) | (Const::Float(f), Const::Number(n)) => Ok(sub(Const::Float(n as f64), Const::Float(f))?),
        (Const::Float(f1), Const::Float(f2)) => Ok(Const::Float(f1 - f2)),

        (Const::Number(n), Const::Pdf(d)) | (Const::Pdf(d), Const::Number(n)) => {
            Ok(sub(Const::Float(n as f64), Const::Pdf(d))?)
        }

        (Const::Float(f), Const::Pdf(d)) | (Const::Pdf(d), Const::Float(f)) => {
            use ast::Distribution;
            let new_d = match d {
                Distribution::Uniform(low, high) => Distribution::Uniform(
                    Box::new(sub(*low, Const::Float(f))?),
                    Box::new(sub(*high, Const::Float(f))?)
                ),
                Distribution::Normal(mean, std_dev) => Distribution::Normal(
                    Box::new(sub(*mean, Const::Float(f))?),
                    std_dev
                )
            };
            Ok(Const::Pdf(new_d))
        }

        (Const::Pdf(_d1), Const::Pdf(_d2)) => {
            Err(errors::SimpleConflictError {
                message: format!(
                    "{} and {} are not comparable with sub. PDF * PDF arithmetic ain't implemented yet.",
                    ast::string_of_const_type(&a),
                    ast::string_of_const_type(&b)
                )
                .to_string(),
            })
        }

        (Const::Bool(_), _) | (_, Const::Bool(_)) => Err(errors::SimpleConflictError {
            message: format!(
                "{} and {} are not comparable with sub.",
                ast::string_of_const_type(&a),
                ast::string_of_const_type(&b)
            )
            .to_string(),
        }),
    }
}

pub fn mul(a: Const, b: Const) -> Result<Const, errors::SimpleConflictError> {
    match (a.clone(), b.clone()) {
        (Const::Number(n1), Const::Number(n2)) => Ok(Const::Number(n1 * n2)),
        (Const::Number(n), Const::Float(f)) | (Const::Float(f), Const::Number(n)) => Ok(mul(Const::Float(n as f64), Const::Float(f))?),
        (Const::Float(f1), Const::Float(f2)) => Ok(Const::Float(f1 * f2)),

        (Const::Number(n), Const::Pdf(d)) | (Const::Pdf(d), Const::Number(n)) => {
            Ok(mul(Const::Float(n as f64), Const::Pdf(d))?)
        }

        (Const::Float(f), Const::Pdf(d)) | (Const::Pdf(d), Const::Float(f)) => {
            use ast::Distribution;
            let new_d = match d {
                Distribution::Uniform(low, high) => Distribution::Uniform(
                    Box::new(mul(*low, Const::Float(f))?),
                    Box::new(mul(*high, Const::Float(f))?)
                ),
                Distribution::Normal(mean, std_dev) => Distribution::Normal(
                    Box::new(mul(*mean, Const::Float(f))?),
                    Box::new(mul(*std_dev, Const::Float(f64::powf(f, 2.0)))?)
                )
            };
            Ok(Const::Pdf(new_d))
        }

        (Const::Pdf(_d1), Const::Pdf(_d2)) => {
            Err(errors::SimpleConflictError {
                message: format!(
                    "{} and {} are not comparable with mul. PDF * PDF arithmetic ain't implemented yet.",
                    ast::string_of_const_type(&a),
                    ast::string_of_const_type(&b)
                )
                .to_string(),
            })
        }

        (Const::Bool(_), _) | (_, Const::Bool(_)) => Err(errors::SimpleConflictError {
            message: format!(
                "{} and {} are not comparable with mul.",
                ast::string_of_const_type(&a),
                ast::string_of_const_type(&b)
            )
            .to_string(),
        }),
    }
}

pub fn div(a: Const, b: Const) -> Result<Const, errors::SimpleConflictError> {
    match (a.clone(), b.clone()) {
        (Const::Number(n1), Const::Number(n2)) => Ok(Const::Number(n1 / n2)),
        (Const::Number(n), Const::Float(f)) | (Const::Float(f), Const::Number(n)) => Ok(div(Const::Float(n as f64), Const::Float(f))?),
        (Const::Float(f1), Const::Float(f2)) => Ok(Const::Float(f1 / f2)),

        (Const::Number(n), Const::Pdf(d)) | (Const::Pdf(d), Const::Number(n)) => {
            Ok(div(Const::Float(n as f64), Const::Pdf(d))?)
        }

        (Const::Float(f), Const::Pdf(d)) | (Const::Pdf(d), Const::Float(f)) => {
            use ast::Distribution;
            let new_d = match d {
                Distribution::Uniform(low, high) => Distribution::Uniform(
                    Box::new(div(*low, Const::Float(f))?),
                    Box::new(div(*high, Const::Float(f))?)
                ),
                Distribution::Normal(mean, std_dev) => Distribution::Normal(
                    Box::new(div(*mean, Const::Float(f))?),
                    Box::new(div(*std_dev, Const::Float(f64::powf(f, 2.0)))?)
                )
            };
            Ok(Const::Pdf(new_d))
        }

        (Const::Pdf(_d1), Const::Pdf(_d2)) => {
            Err(errors::SimpleConflictError {
                message: format!(
                    "{} and {} are not comparable with div. PDF * PDF arithmetic ain't implemented yet.",
                    ast::string_of_const_type(&a),
                    ast::string_of_const_type(&b)
                )
                .to_string(),
            })
        }

        (Const::Bool(_), _) | (_, Const::Bool(_)) => Err(errors::SimpleConflictError {
            message: format!(
                "{} and {} are not comparable with div.",
                ast::string_of_const_type(&a),
                ast::string_of_const_type(&b)
            )
            .to_string(),
        }),
    }
}
