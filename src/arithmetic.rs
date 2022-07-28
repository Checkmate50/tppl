use std::iter;

use crate::{ast, errors};
use ast::Const;
// use rand::prelude::{IteratorRandom, SliceRandom};
use rand_distr::Distribution;

pub static SAMPLE_N: usize = 10_000;

pub fn add(a: Const, b: Const) -> Result<Const, errors::SimpleConflictError> {
    match (a.clone(), b.clone()) {
        (Const::Number(n1), Const::Number(n2)) => Ok(Const::Number(n1 + n2)),
        // commutative
        (Const::Number(n), Const::Float(f)) | (Const::Float(f), Const::Number(n)) => {
            Ok(add(Const::Float(n as f64), Const::Float(f))?)
        }
        (Const::Float(f1), Const::Float(f2)) => Ok(Const::Float(f1 + f2)),
        // commutative
        (Const::Number(n), Const::Pdf(d)) | (Const::Pdf(d), Const::Number(n)) => {
            Ok(sub(Const::Float(n as f64), Const::Pdf(d))?)
        }
        // commutative
        (Const::Float(f), Const::Pdf(d)) | (Const::Pdf(d), Const::Float(f)) => {
            use ast::Distribution;
            let new_d = match d {
                Distribution::Uniform(low, high) => Distribution::Uniform(
                    Box::new(add(*low, Const::Float(f))?),
                    Box::new(add(*high, Const::Float(f))?),
                ),
                Distribution::Normal(mean, std_dev) => {
                    Distribution::Normal(Box::new(add(*mean, Const::Float(f))?), std_dev)
                }
                Distribution::List(data_points) => {
                    Distribution::List(data_points.into_iter().map(|x| f + x).collect::<Vec<f64>>())
                }
            };
            Ok(Const::Pdf(new_d))
        }

        (Const::Pdf(d1), Const::Pdf(d2)) => {
            use ast::Distribution;
            match (d1.clone(), d2.clone()) {
                (Distribution::Normal(mean1, std_dev1), Distribution::Normal(mean2, std_dev2)) => {
                    // https://en.wikipedia.org/wiki/Sum_of_normally_distributed_random_variables    <- Operations on two independent normal variables
                    let mean = add(*mean1, *mean2)?;
                    let std_dev = pow(
                        (add(
                            pow(*std_dev1, Const::Number(2))?,
                            pow(*std_dev2, Const::Number(2))?,
                        ))?,
                        Const::Float(0.5),
                    )?;
                    Ok(Const::Pdf(Distribution::Normal(
                        Box::new(mean),
                        Box::new(std_dev),
                    )))
                }
                (_, _) => {
                    let addends_1: Vec<f64> = spam_sample(d1, SAMPLE_N);
                    let addends_2: Vec<f64> = spam_sample(d2, SAMPLE_N);
                    let sampled_sums: Vec<f64> = addends_1
                        .into_iter()
                        .zip(addends_2.into_iter())
                        .map(|(a, b)| a + b)
                        .collect();
                    Ok(Const::Pdf(Distribution::List(sampled_sums)))
                }
            }
        }

        (Const::Bool(_), _) | (_, Const::Bool(_)) => Err(errors::SimpleConflictError {
            message: format!(
                "{} and {} are not operable with add.",
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
        (Const::Number(n), Const::Float(f)) => Ok(sub(Const::Float(n as f64), Const::Float(f))?),
        (Const::Float(f), Const::Number(n)) => Ok(sub(Const::Float(f), Const::Float(n as f64))?),
        (Const::Float(f1), Const::Float(f2)) => Ok(Const::Float(f1 - f2)),
        (Const::Number(n), Const::Pdf(d)) => Ok(sub(Const::Float(n as f64), Const::Pdf(d))?),
        (Const::Pdf(d), Const::Number(n)) => Ok(sub(Const::Pdf(d), Const::Float(n as f64))?),
        (Const::Float(f), Const::Pdf(d)) => {
            use ast::Distribution;
            let new_d = match d {
                Distribution::Uniform(low, high) => Distribution::Uniform(
                    Box::new(sub(Const::Float(f), *high)?), // `(c - high) < (c - low)`
                    Box::new(sub(Const::Float(f), *low)?),
                ),
                Distribution::Normal(mean, std_dev) => {
                    Distribution::Normal(Box::new(sub(Const::Float(f), *mean)?), std_dev)
                }
                Distribution::List(data_points) => {
                    Distribution::List(data_points.into_iter().map(|x| f - x).collect::<Vec<f64>>())
                }
            };
            Ok(Const::Pdf(new_d))
        }
        (Const::Pdf(d), Const::Float(f)) => {
            use ast::Distribution;
            let new_d = match d {
                Distribution::Uniform(low, high) => Distribution::Uniform(
                    Box::new(sub(*low, Const::Float(f))?),
                    Box::new(sub(*high, Const::Float(f))?),
                ),
                Distribution::Normal(mean, std_dev) => {
                    Distribution::Normal(Box::new(sub(*mean, Const::Float(f))?), std_dev)
                }
                Distribution::List(data_points) => {
                    Distribution::List(data_points.into_iter().map(|x| x - f).collect::<Vec<f64>>())
                }
            };
            Ok(Const::Pdf(new_d))
        }
        (Const::Pdf(d1), Const::Pdf(d2)) => {
            use ast::Distribution;
            match (d1.clone(), d2.clone()) {
                (Distribution::Normal(mean1, std_dev1), Distribution::Normal(mean2, std_dev2)) => {
                    // https://en.wikipedia.org/wiki/Sum_of_normally_distributed_random_variables    <- Operations on two independent normal variables
                    let mean = sub(*mean1, *mean2)?;
                    let std_dev = pow(
                        (add(
                            pow(*std_dev1, Const::Number(2))?,
                            pow(*std_dev2, Const::Number(2))?,
                        ))?,
                        Const::Float(0.5),
                    )?;
                    Ok(Const::Pdf(Distribution::Normal(
                        Box::new(mean),
                        Box::new(std_dev),
                    )))
                }
                (_, _) => {
                    let minuends: Vec<f64> = spam_sample(d1, SAMPLE_N);
                    let subtrahends: Vec<f64> = spam_sample(d2, SAMPLE_N);
                    let sampled_diffs: Vec<f64> = minuends
                        .into_iter()
                        .zip(subtrahends.into_iter())
                        .map(|(a, b)| a - b)
                        .collect();
                    Ok(Const::Pdf(Distribution::List(sampled_diffs)))
                }
            }
        }
        (Const::Bool(_), _) | (_, Const::Bool(_)) => Err(errors::SimpleConflictError {
            message: format!(
                "{} and {} are not operable with sub.",
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
        // commutative
        (Const::Number(n), Const::Float(f)) | (Const::Float(f), Const::Number(n)) => {
            Ok(mul(Const::Float(n as f64), Const::Float(f))?)
        }
        (Const::Float(f1), Const::Float(f2)) => Ok(Const::Float(f1 * f2)),
        // commutative
        (Const::Number(n), Const::Pdf(d)) | (Const::Pdf(d), Const::Number(n)) => {
            Ok(mul(Const::Float(n as f64), Const::Pdf(d))?)
        }
        // commutative
        (Const::Float(f), Const::Pdf(d)) | (Const::Pdf(d), Const::Float(f)) => {
            use ast::Distribution;
            let new_d = match d {
                Distribution::Uniform(low, high) => Distribution::Uniform(
                    Box::new(mul(*low, Const::Float(f))?),
                    Box::new(mul(*high, Const::Float(f))?),
                ),
                Distribution::Normal(mean, std_dev) => Distribution::Normal(
                    Box::new(mul(*mean, Const::Float(f))?),
                    Box::new(mul(*std_dev, Const::Float(f.abs()))?),
                ),
                Distribution::List(data_points) => {
                    Distribution::List(data_points.into_iter().map(|x| f * x).collect::<Vec<f64>>())
                }
            };
            Ok(Const::Pdf(new_d))
        }

        (Const::Pdf(d1), Const::Pdf(d2)) => {
            use ast::Distribution;
            let factors_1: Vec<f64> = spam_sample(d1, SAMPLE_N);
            let factors_2: Vec<f64> = spam_sample(d2, SAMPLE_N);
            let sampled_prods: Vec<f64> = factors_1
                .into_iter()
                .zip(factors_2.into_iter())
                .map(|(a, b)| a * b)
                .collect();
            Ok(Const::Pdf(Distribution::List(sampled_prods)))
        }

        (Const::Bool(_), _) | (_, Const::Bool(_)) => Err(errors::SimpleConflictError {
            message: format!(
                "{} and {} are not operable with mul.",
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
        (Const::Number(n), Const::Float(f)) => Ok(div(Const::Float(n as f64), Const::Float(f))?),
        (Const::Float(f), Const::Number(n)) => Ok(div(Const::Float(f), Const::Float(n as f64))?),
        (Const::Float(f1), Const::Float(f2)) => Ok(Const::Float(f1 / f2)),
        (Const::Number(n), Const::Pdf(d)) => Ok(div(Const::Float(n as f64), Const::Pdf(d))?),
        (Const::Pdf(d), Const::Number(n)) => Ok(div(Const::Pdf(d), Const::Float(n as f64))?),
        (Const::Float(f), Const::Pdf(d)) => {
            use ast::Distribution;
            let divisors: Vec<f64> = spam_sample(d, SAMPLE_N);
            let sampled_quots: Vec<f64> = divisors.into_iter().map(|b| f / b).collect();
            Ok(Const::Pdf(Distribution::List(sampled_quots)))
        }
        (Const::Pdf(d), Const::Float(f)) => {
            use ast::Distribution;
            let new_d = match d {
                Distribution::Uniform(low, high) => Distribution::Uniform(
                    Box::new(div(*low, Const::Float(f))?),
                    Box::new(div(*high, Const::Float(f))?),
                ),
                Distribution::Normal(mean, std_dev) => Distribution::Normal(
                    Box::new(div(*mean, Const::Float(f))?),
                    Box::new(div(*std_dev, Const::Float(f.abs()))?),
                ),
                Distribution::List(dividends) => {
                    Distribution::List(dividends.into_iter().map(|x| x / f).collect::<Vec<f64>>())
                }
            };
            Ok(Const::Pdf(new_d))
        }

        (Const::Pdf(d1), Const::Pdf(d2)) => {
            use ast::Distribution;
            let dividends: Vec<f64> = spam_sample(d1, SAMPLE_N);
            let divisiors: Vec<f64> = spam_sample(d2, SAMPLE_N);
            let sampled_quots: Vec<f64> = dividends
                .into_iter()
                .zip(divisiors.into_iter())
                .map(|(a, b)| a / b)
                .collect();
            Ok(Const::Pdf(Distribution::List(sampled_quots)))
        }

        (Const::Bool(_), _) | (_, Const::Bool(_)) => Err(errors::SimpleConflictError {
            message: format!(
                "{} and {} are not operable with div.",
                ast::string_of_const_type(&a),
                ast::string_of_const_type(&b)
            )
            .to_string(),
        }),
    }
}

fn pow(a: Const, b: Const) -> Result<Const, errors::SimpleConflictError> {
    match (a.clone(), b.clone()) {
        (Const::Number(n1), Const::Number(n2)) => {
            Ok(Const::Number(i64::pow(n1, n2.try_into().unwrap())))
        }
        (Const::Number(n), Const::Float(f)) => Ok(pow(Const::Float(n as f64), Const::Float(f))?),
        (Const::Float(f), Const::Number(n)) => Ok(pow(Const::Float(f), Const::Float(n as f64))?),
        (Const::Float(f1), Const::Float(f2)) => Ok(Const::Float(f64::powf(f1, f2))),
        (Const::Number(n), Const::Pdf(d)) => Ok(pow(Const::Float(n as f64), Const::Pdf(d))?),
        (Const::Pdf(d), Const::Number(n)) => Ok(pow(Const::Pdf(d), Const::Float(n as f64))?),
        (Const::Float(f), Const::Pdf(d)) => {
            use ast::Distribution;
            let exponents = spam_sample(d, SAMPLE_N);
            let sampled_pows: Vec<f64> =
                exponents.into_iter().map(|exp| f64::powf(f, exp)).collect();
            Ok(Const::Pdf(Distribution::List(sampled_pows)))
        }

        (Const::Pdf(d), Const::Float(f)) => {
            use ast::Distribution;
            let bases = spam_sample(d, SAMPLE_N);
            let sampled_pows: Vec<f64> = bases.into_iter().map(|base| f64::powf(base, f)).collect();
            Ok(Const::Pdf(Distribution::List(sampled_pows)))
        }

        (Const::Pdf(d1), Const::Pdf(d2)) => {
            use ast::Distribution;
            let bases: Vec<f64> = spam_sample(d1, SAMPLE_N);
            let exponents: Vec<f64> = spam_sample(d2, SAMPLE_N);
            let sampled_pows: Vec<f64> = bases
                .into_iter()
                .zip(exponents.into_iter())
                .map(|(a, b)| f64::powf(a, b))
                .collect();
            Ok(Const::Pdf(Distribution::List(sampled_pows)))
        }

        (Const::Bool(_), _) | (_, Const::Bool(_)) => Err(errors::SimpleConflictError {
            message: format!(
                "{} and {} are not operable with pow.",
                ast::string_of_const_type(&a),
                ast::string_of_const_type(&b)
            )
            .to_string(),
        }),
    }
}

pub fn spam_sample(d: ast::Distribution, count: usize) -> Vec<f64> {
    let mut rng = rand::thread_rng();
    match d {
        ast::Distribution::Uniform(low, high) => {
            // todo: optimize by only constructing `rand::distributions::Uniform` once if given `Number | Float`?
            let lows = match *low {
                ast::Const::Number(n) => iter::repeat([n as f64].to_vec().into_iter())
                    .take(count)
                    .flatten(),
                ast::Const::Float(f) => {
                    iter::repeat([f].to_vec().into_iter()).take(count).flatten()
                }
                ast::Const::Pdf(low_d) => iter::repeat(spam_sample(low_d, count).into_iter())
                    .take(1)
                    .flatten(),
                _ => panic!("Uniform's low was of unexpected type."),
            };
            let highs = match *high {
                ast::Const::Number(n) => iter::repeat([n as f64].to_vec().into_iter())
                    .take(count)
                    .flatten(),
                ast::Const::Float(f) => {
                    iter::repeat([f].to_vec().into_iter()).take(count).flatten()
                }
                ast::Const::Pdf(low_d) => iter::repeat(spam_sample(low_d, count).into_iter())
                    .take(1)
                    .flatten(),
                _ => panic!("Uniform's high was of unexpected type."),
            };

            lows.into_iter()
                .zip(highs.into_iter())
                .map(|(low, high)| {
                    let dist: rand::distributions::Uniform<f64> =
                        rand::distributions::Uniform::new_inclusive(low, high);
                    dist.sample(&mut rng)
                })
                .collect::<Vec<f64>>()
        }
        ast::Distribution::Normal(mean, std_dev) => {
            let means = match *mean {
                ast::Const::Number(n) => iter::repeat([n as f64].to_vec().into_iter())
                    .take(count)
                    .flatten(),
                ast::Const::Float(f) => {
                    iter::repeat([f].to_vec().into_iter()).take(count).flatten()
                }
                ast::Const::Pdf(mean_d) => iter::repeat(spam_sample(mean_d, count).into_iter())
                    .take(1)
                    .flatten(),
                _ => panic!("Normal's mean was of unexpected type."),
            };
            let std_devs = match *std_dev {
                ast::Const::Number(n) => iter::repeat([n as f64].to_vec().into_iter())
                    .take(count)
                    .flatten(),
                ast::Const::Float(f) => {
                    iter::repeat([f].to_vec().into_iter()).take(count).flatten()
                }
                ast::Const::Pdf(std_dev_d) => {
                    iter::repeat(spam_sample(std_dev_d, count).into_iter())
                        .take(1)
                        .flatten()
                }
                _ => panic!("Normal's std_dev was of unexpected type."),
            };

            means
                .into_iter()
                .zip(std_devs.into_iter())
                .map(|(mean, std_dev)| {
                    // todo: better error handling for `rand_distr::Normal::new(_, _)`.
                    let dist: rand_distr::Normal<f64> =
                        rand_distr::Normal::new(mean, std_dev).unwrap();
                    dist.sample(&mut rng)
                })
                .collect::<Vec<f64>>()
        }
        ast::Distribution::List(v) => {
            // note: if we do resize `v`, the order must also be random! bc `interpreters::eval_expr`'s predcall depends on random order.

            // let length = v.len();
            // let quotient = count / length;
            // let remainder = count % length;

            // let mut sample: Vec<f64> = Vec::new();
            // for _ in 0..quotient {
            //     // we don't need to shuffle here since this
            //     sample.append(&mut v.into_iter().choose_multiple(&mut rng, length)); // maybe just do `sample.append(&mut v.clone())`
            // }

            // let (remaining_bit, _) = v.partial_shuffle(&mut rng, remainder);
            // sample.append(&mut remaining_bit.to_vec());

            // sample

            v
        }
    }
}
