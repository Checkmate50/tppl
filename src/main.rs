use chumsky::{prelude::*, Stream};
use std::{env, fs};

// use crate::arithmetic::spam_sample;
mod arithmetic;
mod ast;
mod ast_printer;
mod builtins;
mod errors;
mod interpreter;
mod lexer;
mod parser;
mod stats;
mod typecheck;
mod types;

const DOMAIN_N: i32 = 100;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let f_name = env::args().nth(1).expect("Expected file argument");
    let src = fs::read_to_string(f_name.clone()).expect("failed to read file");

    let prog = lex_and_parse(src.as_str());
    if let Some(funcs) = prog {
        println!("{}", ast_printer::string_of_program(funcs.clone()));

        let program_result = match typecheck::infer_program(funcs) {
            Ok(ty_prog) => match interpreter::exec_program(ty_prog.clone()) {
                Ok(dist_queue) => {
                    println!("Executed");
                    Ok(dist_queue)
                }
                Err(e) => Err(errors::Error::Dynamic(e)),
            },
            Err(e) => Err(errors::Error::Static(e)),
        };
        match program_result {
            Ok(dist_queue) => draw(
                dist_queue,
                std::path::Path::new(f_name.as_str())
                    .file_stem()
                    .unwrap()
                    .to_str()
                    .unwrap(),
            )?,
            Err(e) => println!("{:?}", e),
        }
    }
    Ok(())
}


fn lex_and_parse(src: &str) -> Option<ast::Program> {
    let (tokens, lex_errs) = lexer::lexer().parse_recovery(src);
    if lex_errs.len() > 0 {
        println!("lexer errors {:?}", lex_errs);
        None
    } else {
        if let Some(tokens) = tokens {
            let len = src.chars().count();
            let (prog, parse_errs) = parser::parser()
                .parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

            if parse_errs.len() > 0 {
                println!("parser errors {:?}", parse_errs);
                None
            } else {
                prog
            }
        } else {
            None
        }
    }
}

// todo: perhaps always sample when drawing?
fn draw(
    dist_queue: Vec<Vec<(String, ast::Const)>>,
    f_name: &str,
) -> Result<(), Box<dyn std::error::Error>> {
    use plotters::prelude::*;

    let version_range = 0..1_000;
    if !std::path::Path::new("out").exists() {
        fs::create_dir("out")?;
    }
    let f_name_dir = format!("out/{}", f_name);
    if !std::path::Path::new(f_name_dir.as_str()).exists() {
        fs::create_dir(f_name_dir.clone())?;
    }
    let dir_root: String = version_range
        .into_iter()
        .find_map(|n| {
            let dir_root_cand = format!("{}/{}", f_name_dir.clone(), n);
            if std::path::Path::new(dir_root_cand.as_str()).exists() {
                None
            } else {
                Some(dir_root_cand)
            }
        })
        .expect("User, you need to clean out your `out` directory...");
    fs::create_dir(dir_root.clone())?;

    for (timeblock_num, graphs) in dist_queue.into_iter().enumerate() {
        let timeblock_dir = format!("{}/timeblock_{}", dir_root.clone(), timeblock_num.clone());
        fs::create_dir(timeblock_dir.clone())?;
        for (graph_i, (title, constant)) in graphs.into_iter().enumerate() {
            let observations: Vec<f64> = match constant {
                ast::Const::Bool(_) => todo!(),
                ast::Const::Number(_) => todo!(),
                ast::Const::Float(_) => todo!(),
                ast::Const::Pdf(d) => arithmetic::spam_sample(d, arithmetic::SAMPLE_N),
            };

            let min = observations
                .iter()
                .min_by(|a, b| a.partial_cmp(b).unwrap())
                .unwrap()
                .to_owned();
            let max = observations
                .iter()
                .max_by(|a, b| a.partial_cmp(b).unwrap())
                .unwrap()
                .to_owned();

            let x_d = stats::range(min, max, DOMAIN_N);

            // x = [-2.1; -1.3; -0.4; 1.9; 5.1; 6.2];
            // x_d = range(-7, 11, length = 100)

            // dens = [D(xstep, sqrt(2.25), x, K) for xstep in x_d, K in (kgauss, kbox, ktri)]
            //
            // # visualize the kernels
            // plot(x_d, dens, label = ["Gaussian", "Box", "Triangular"])

            let data_set = x_d.into_iter().map(|x| {
                (
                    x as f32,
                    stats::kernel_density_estimation(
                        x,
                        0.05,
                        observations.clone(),
                        stats::Kernel::Gaussian,
                    ) as f32,
                )
            });

            let graph_file_name = format!("{}/{}.png", timeblock_dir.clone(), graph_i);

            let root = BitMapBackend::new(graph_file_name.as_str(), (640, 480)).into_drawing_area();
            root.fill(&WHITE)?;

            let title_size: i32 = if title.len() <= 36 {
                50
            } else {
                50 * 36 / title.len() as i32
            };

            let mut chart = ChartBuilder::on(&root)
                .caption(title.clone(), ("sans-serif", title_size).into_font())
                .margin::<u32>(5)
                .set_left_and_bottom_label_area_size::<u32>(20)
                .x_label_area_size(30)
                .y_label_area_size(30)
                .build_cartesian_2d((min as f32)..(max as f32), -0.1f32..1f32)?;
            chart.configure_mesh().draw()?;

            chart
                .draw_series(LineSeries::new(data_set, &RED))?
                .label("distribution")
                .legend(|(x, y)| PathElement::new(vec![(x, y), (x + 20, y)], &RED));

            chart
                .configure_series_labels()
                .background_style(&WHITE.mix(0.8))
                .border_style(&BLACK)
                .draw()?;

            root.present()?;
        }
    }
    Ok(())
}
