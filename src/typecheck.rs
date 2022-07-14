use std::collections::{HashMap, HashSet};
use std::iter;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::ast::{self, type_of_typedexpr};
use crate::errors::{self, CompileTimeError, ConstrainError};
use crate::{builtins, types};
use ast::Var;

use petgraph::algo::toposort;
use petgraph::graph::Graph;
use petgraph::stable_graph::NodeIndex;
// use petgraph::algo::min_spanning_tree;
// use petgraph::algo::is_cyclic_directed;

#[derive(Debug, Clone)]
pub struct TypeContext {
    // only global scope
    pub vars: HashMap<Var, (Vec<types::TemporalType>, types::SimpleType)>,
    pub clock: i32,
}

impl TypeContext {
    // gives type of variable name.
    pub fn look_up(&mut self, var_name: &Var) -> Result<types::Type, errors::CompileTimeError> {
        if builtins::is_builtin(var_name) {
            Err(errors::PredicateExprError {
                message: format!(
                    "Name {} is taken by a builtin predicate. No need to internally look it up.",
                    var_name
                )
                .to_string(),
            })?
        }

        if let Some((temps, simpl)) = self.vars.get(var_name) {
            if filter_out_nexts(temps).len() == 0 {
                Err(errors::NameError {
                    message: format!(
                        "Name {} doesn't have any temporal types that are currently available.",
                        var_name
                    )
                    .to_string(),
                })?
            }

            if simpl.is_predicate() {
                // doesn't matter that `future` might be used in `interpreters.rs`, but `current` is used in `typecheck.rs`.
                // it's an incomplete scan for errors.
                if let Some(temp) = sort_types_by_immediacy(temps) {
                    // it's fine to give just one type rather than all since it's just type-checking rn. the important bit is `simpl`.
                    Ok(types::Type(
                        temp.get(0).unwrap().to_owned(),
                        simpl.to_owned(),
                    ))
                } else {
                    Err(errors::CurrentlyUnavailableError {
                        message: format!(
                            "Name only has {:?}, which isn't immediately available.",
                            temps
                        )
                        .to_string(),
                    })?
                }
            } else {
                if let Some(temp) = get_most_immediate_type(temps) {
                    Ok(types::Type(temp, simpl.to_owned()))
                } else {
                    Err(errors::CurrentlyUnavailableError {
                        message: format!(
                            "Name only has {:?}, which isn't immediately available.",
                            temps
                        )
                        .to_string(),
                    })?
                }
            }
        } else {
            Err(errors::NameError {
                message: format!("Name {} was never assigned to.", var_name).to_string(),
            })?
        }
    }

    // adds new variable
    pub fn add_name(
        &mut self,
        var_name: &Var,
        data: types::Type,
    ) -> Result<(), errors::CompileTimeError> {
        if builtins::is_builtin(var_name) {
            Err(errors::BuiltinNameConflictError {
                message: format!(
                    "Name {} can't be assigned to since it's the name of a built-in predicate.",
                    var_name
                )
                .to_string(),
            })?
        }

        let types::Type(cand_temp, cand_simpl) = data;
        if let Some((temps, simpl)) = self.vars.clone().get(var_name) {
            let simpl = types::resolve_simple_conflicts(simpl.to_owned(), cand_simpl)?;

            // static-checking can't decide when/if `until`'s condition releases, thus we might as well forgive it for now. violations will be caught at run-time.
            if simpl.is_predicate() {
                self.vars.insert(
                    var_name.to_string(),
                    (
                        Vec::from_iter(
                            temps
                                .iter()
                                .chain(iter::once(&cand_temp))
                                .map(|temp| temp.to_owned()),
                        ),
                        simpl,
                    ),
                );
            } else {
                self.vars.insert(
                    var_name.to_string(),
                    (
                        types::resolve_temporal_conflicts(temps.to_owned(), cand_temp, true)?,
                        simpl,
                    ),
                );
            }
        } else {
            self.vars
                .insert(var_name.to_string(), ([cand_temp].to_vec(), cand_simpl));
        }
        Ok(())
    }

    pub fn step_time(&mut self) -> () {
        self.clock += 1;
        self.vars = self
            .vars
            .clone()
            .into_iter()
            .map(|(name, (temps, simpl))| {
                (
                    name,
                    (
                        temps.into_iter().filter_map(types::advance_type).collect(),
                        simpl,
                    ),
                )
            })
            .filter(
                |(_, (temps, _)): &(String, (Vec<types::TemporalType>, types::SimpleType))| {
                    !temps.is_empty()
                },
            )
            .collect();
    }
}

fn get_id() -> usize {
    static COUNTER: AtomicUsize = AtomicUsize::new(1);
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

pub fn get_most_immediate_type(temps: &Vec<types::TemporalType>) -> Option<types::TemporalType> {
    use types::TemporalAvailability;
    let most_immediate = temps
        .into_iter()
        .max_by_key(|temp| match temp.when_available {
            TemporalAvailability::Current => 1,
            TemporalAvailability::Next => -1,
            TemporalAvailability::Future => 0,
            TemporalAvailability::Undefined => {
                panic!("The immediacy of `undefined` is meaningless.")
            }
        })?;

    if most_immediate.when_available == TemporalAvailability::Next {
        return None;
    } else {
        Some(most_immediate.to_owned())
    }
}

pub fn filter_out_nexts(temps: &Vec<types::TemporalType>) -> Vec<types::TemporalType> {
    let only_currents: Vec<types::TemporalType> = temps
        .into_iter()
        .filter(|temp| temp.when_available != types::TemporalAvailability::Next)
        .map(|temp| temp.to_owned())
        .collect();
    only_currents
}

pub fn sort_types_by_immediacy(
    temps: &Vec<types::TemporalType>,
) -> Option<Vec<types::TemporalType>> {
    use types::TemporalAvailability;
    let mut only_currents = filter_out_nexts(temps);

    if only_currents.len() == 0 {
        return None;
    } else {
        only_currents.sort_by_key(|temp| match temp.when_available {
            TemporalAvailability::Current => 1,
            TemporalAvailability::Next => -1,
            TemporalAvailability::Future => 0,
            TemporalAvailability::Undefined => {
                panic!("The immediacy of `undefined` is meaningless.")
            }
        });
        Some(only_currents)
    }
}

pub fn infer_expr(
    e: ast::Expr,
    ctx: &mut TypeContext,
    until_dependencies: &mut types::UntilDependencies,
    udep_map: &mut HashMap<usize, ast::TypedExpr>,
) -> Result<ast::TypedExpr, errors::CompileTimeError> {
    use ast::Expr;
    use ast::TypedExpr;
    use types::SimpleType;
    use types::TemporalType;
    use types::Type;

    let temporal_undefined = TemporalType {
        when_available: types::TemporalAvailability::Undefined,
        when_dissipates: types::TemporalPersistency::Undefined,
        is_until: None,
    };

    match e {
        Expr::EBinop(b, e1, e2) => {
            use ast::Binop;
            let operand1 = infer_expr(*e1, ctx, until_dependencies, udep_map)?;
            let operand2 = infer_expr(*e2, ctx, until_dependencies, udep_map)?;

            match b {
                Binop::Plus | Binop::Minus | Binop::Times | Binop::Div => {
                    let simpl1 =
                        match types::boil_simple(type_of_typedexpr(operand1.clone()).get_simpl()) {
                            SimpleType::Option(_) => {
                                panic!("Boiling still gave an Option SimpleType??")
                            }
                            SimpleType::Int => SimpleType::Int,
                            SimpleType::Float => SimpleType::Float,
                            SimpleType::Undefined => SimpleType::Undefined,
                            SimpleType::Pdf => SimpleType::Pdf,
                            _ => SimpleType::Bottom,
                        };
                    let simpl2 =
                        match types::boil_simple(type_of_typedexpr(operand2.clone()).get_simpl()) {
                            SimpleType::Option(_) => {
                                panic!("Boiling still gave an Option SimpleType??")
                            }
                            SimpleType::Int => SimpleType::Int,
                            SimpleType::Float => SimpleType::Float,
                            SimpleType::Undefined => SimpleType::Undefined,
                            SimpleType::Pdf => SimpleType::Pdf,
                            _ => SimpleType::Bottom,
                        };
                    let ty1 = Type(temporal_undefined.clone(), simpl1.clone());
                    let ty2 = Type(temporal_undefined.clone(), simpl2.clone());
                    let op1 = ast::new_texpr(
                        operand1,
                        ty1,
                        "Left operand of an arithmetic binary operator must be an integer."
                            .to_string(),
                    )?;
                    let op2 = ast::new_texpr(
                        operand2,
                        ty2,
                        "Right operand of an arithmetic binary operator must be an integer."
                            .to_string(),
                    )?;

                    let simpl = match (simpl1, simpl2) {
                        (SimpleType::Option(_), _) | (_, SimpleType::Option(_)) => {
                            panic!("Boiling still gave an Option SimpleType??")
                        }
                        (SimpleType::Undefined, _) | (_, SimpleType::Undefined) => {
                            SimpleType::Undefined
                        }
                        (SimpleType::Float, SimpleType::Float)
                        | (SimpleType::Float, SimpleType::Int)
                        | (SimpleType::Int, SimpleType::Float) => SimpleType::Float,
                        (SimpleType::Int, SimpleType::Int) => {
                            if b == Binop::Div {
                                SimpleType::Float
                            } else {
                                SimpleType::Int
                            }
                        }
                        (SimpleType::Pdf, _) | (_, SimpleType::Pdf) => SimpleType::Pdf,
                        _ => SimpleType::Bottom,
                    };

                    let t = Type(temporal_undefined, simpl);
                    Ok(TypedExpr::TEBinop(b, Box::new(op1), Box::new(op2), t))
                }
                Binop::Or => {
                    // todo: negation as failure?
                    let ty1 = Type(temporal_undefined.clone(), SimpleType::Bool);
                    let op1 = ast::new_texpr(
                        operand1,
                        ty1,
                        "Left operand of a logical operator must be a boolean.".to_string(),
                    )?;
                    // let ty2 = Type(temporal_undefined.clone(), SimpleType::Bool);
                    // let op2 = ast::new_texpr(operand2, ty2, "Right operand of a logical operator must be a boolean.".to_string())?;
                    let op2 = operand2;

                    // let t = Type(temporal_undefined, SimpleType::Bool);
                    let t = type_of_typedexpr(op2.clone());
                    Ok(TypedExpr::TEBinop(b, Box::new(op1), Box::new(op2), t))
                }
                Binop::And => {
                    // todo: negation as failure?
                    let ty1 = Type(temporal_undefined.clone(), SimpleType::Bool);
                    let op1 = ast::new_texpr(
                        operand1,
                        ty1,
                        "Left operand of a logical operator must be a boolean.".to_string(),
                    )?;
                    let op2 = operand2;
                    let Type(temp, simpl) = type_of_typedexpr(op2.clone());
                    let t = if simpl.is_option() {
                        Type(temp, simpl)
                    } else {
                        Type(temp, SimpleType::Option(Box::new(simpl)))
                    };
                    Ok(TypedExpr::TEBinop(b, Box::new(op1), Box::new(op2), t))
                }
                Binop::Until => {
                    // todo: truthiness
                    let ty1 = Type(temporal_undefined.clone(), SimpleType::Undefined);
                    let op1 = ast::new_texpr(
                        operand1,
                        ty1,
                        "Left operand of Until should have type 'current'.".to_string(),
                    )?;
                    let ty2 = Type(temporal_undefined, SimpleType::Bool);
                    let op2 = ast::new_texpr(operand2, ty2, "Right operand of Until should have type 'until bool'. Basically a thunk if it has free variables...".to_string())?;

                    let id: usize = get_id();
                    until_dependencies.weak.push(id);
                    udep_map.insert(id, op2.clone());

                    let t = ast::type_of_typedexpr(op1.clone());
                    Ok(TypedExpr::TEBinop(b, Box::new(op1), Box::new(op2), t))
                }
                Binop::SUntil => {
                    // the same as `Until` because `SUntil(A, B) :- Until(A, B) and Finally(B)` can be done when translating to LTL/BA.
                    // todo: truthiness
                    let ty1 = Type(temporal_undefined.clone(), SimpleType::Undefined);
                    let op1 = ast::new_texpr(
                        operand1,
                        ty1,
                        "Left operand of SUntil should have type 'current'.".to_string(),
                    )?;
                    let ty2 = Type(temporal_undefined, SimpleType::Bool);
                    let op2 = ast::new_texpr(operand2, ty2, "Right operand of SUntil should have type 'until bool'. Basically a thunk if it has free variables...".to_string())?;

                    let id: usize = get_id();
                    until_dependencies.strong.push(id);
                    udep_map.insert(id, op2.clone());

                    let t = ast::type_of_typedexpr(op1.clone());
                    Ok(TypedExpr::TEBinop(b, Box::new(op1), Box::new(op2), t))
                }
            }
        }
        Expr::EConst(c) => {
            let t = ast::type_of_constant(c.clone());
            Ok(TypedExpr::TEConst(c, t))
        }
        Expr::EUnop(u, e1) => {
            use ast::Unop;
            let operand1 = infer_expr(*e1, ctx, until_dependencies, udep_map)?;
            match u {
                Unop::Neg => {
                    let simpl1 =
                        match types::boil_simple(type_of_typedexpr(operand1.clone()).get_simpl()) {
                            SimpleType::Option(_) => {
                                panic!("Boiling still gave an Option SimpleType??")
                            }
                            SimpleType::Int => SimpleType::Int,
                            SimpleType::Float => SimpleType::Float,
                            SimpleType::Undefined => SimpleType::Undefined,
                            SimpleType::Pdf => SimpleType::Pdf,
                            _ => SimpleType::Bottom,
                        };
                    let ty1 = Type(temporal_undefined.clone(), simpl1);
                    let op1 = ast::new_texpr(
                        operand1,
                        ty1,
                        "Neg operator only works on numerics.".to_string(),
                    )?;

                    let t = Type(temporal_undefined, SimpleType::Int);
                    Ok(TypedExpr::TEUnop(u, Box::new(op1), t))
                }
                Unop::Not => {
                    // todo: truthiness
                    let ty1 = Type(temporal_undefined.clone(), SimpleType::Bool);
                    let op1 = ast::new_texpr(
                        operand1,
                        ty1,
                        "Not operator only works on booleans.".to_string(),
                    )?;

                    let t = Type(temporal_undefined, SimpleType::Bool);
                    Ok(TypedExpr::TEUnop(u, Box::new(op1), t))
                }
            }
        }
        Expr::EVar(s) => {
            if builtins::is_builtin(&s) {
                Err(errors::PredicateExprError {
                    message: format!("Tried to access builtin predicate {}. FOL!", s).to_string(),
                })?
            }

            let named_type = ctx.look_up(&s)?;

            if named_type.get_simpl().is_predicate() {
                Err(errors::PredicateExprError {
                    message: format!("Tried to access predicate {}. FOL!", s).to_string(),
                })?
            }

            if types::is_currently_available(&named_type.get_temporal()) {
                Ok(TypedExpr::TEVar(
                    s,
                    Type(temporal_undefined, named_type.get_simpl()),
                ))
            } else {
                Err(errors::CurrentlyUnavailableError {
                    message: "Variable doesn't have `current` type.".to_string(),
                })?
            }
        }
        Expr::EInput => Ok(TypedExpr::TEInput(Type(
            temporal_undefined,
            types::SimpleType::Int,
        ))),
        Expr::ECall(name, args) => {
            // temporal type of arguments. these can't be temporally `undefined`, which `infer_expr` would blindly give.
            // we need to simulate `=` assignment in `local_context`.
            let temporal_ty_arg = TemporalType {
                when_available: types::TemporalAvailability::Current,
                when_dissipates: types::TemporalPersistency::Always,
                is_until: None,
            };

            let typed_args: Result<Vec<TypedExpr>, CompileTimeError> = args
                .into_iter()
                .map(|e| infer_expr(e, ctx, until_dependencies, udep_map))
                .collect();

            if builtins::is_builtin(&name) {
                let typed_args = typed_args?;
                let simpls: Vec<SimpleType> = typed_args
                    .clone()
                    .into_iter()
                    .map(|texpr| ast::type_of_typedexpr(texpr).get_simpl())
                    .collect();
                let ret: SimpleType = builtins::typecheck_builtin(name.clone(), simpls)?;
                Ok(TypedExpr::TECall(
                    name,
                    typed_args,
                    Type(temporal_undefined, ret),
                ))
            } else {
                let Type(_, pred_simpl) = ctx.look_up(&name)?;
                if let SimpleType::Predicate(params_ts, ret) = pred_simpl {
                    let constrained_typed_args: Result<Vec<TypedExpr>, ConstrainError> = typed_args?
                        .into_iter().zip(params_ts.into_iter())
                        .map(|(texpr, arg_simpl)| {
                            ast::new_texpr(
                                texpr,
                                Type(temporal_ty_arg.clone(), *arg_simpl),
                                "Somehow `typecheck::infer_expr` didn't give `Type(Temp:Und, Simpl::Arg's)`"
                                    .to_string(),
                            )
                        })
                        .collect();
                    Ok(TypedExpr::TECall(
                        name,
                        constrained_typed_args?,
                        Type(temporal_undefined, *ret),
                    ))
                } else {
                    Err(errors::ImproperCallError {
                        message: format!("Attempted to call a non-predicate {}", name).to_string(),
                    })?
                }
            }
        }
        Expr::EPred(name, params, body) => {
            if builtins::is_builtin(&name) {
                Err(errors::PredicateExprError {
                    message: "Predicates that share a name with a builtin cannot be created."
                        .to_string(),
                })?
            }

            // temporal type of parameters
            let temporal_ty_param = TemporalType {
                when_available: types::TemporalAvailability::Current,
                when_dissipates: types::TemporalPersistency::Always,
                is_until: None,
            };

            // not inferring parameters rn.
            let arg_types: Vec<Box<SimpleType>> = params
                .iter()
                .map(|_| Box::new(SimpleType::Undefined))
                .collect();

            let mut local_context = ctx.clone();

            // We don't know the return value of predicate, so we set it `SimpleType::Undefined` for now.
            // Then, we repeat inference to use the proper returned_simpletype of the predicate.
            let is_first_time = !ctx.vars.contains_key(&name);
            if is_first_time {
                // insert predicate name into local
                local_context.add_name(
                    &name,
                    Type(
                        // temporalness is assumed to be like `temporal_ty_param`.
                        // At run-time, the temporalness depends on the assignment operator, but that can be handled then.
                        // This is just meant to infer the body anyways.
                        temporal_ty_param.clone(),
                        SimpleType::Predicate(arg_types.clone(), Box::new(SimpleType::Undefined)),
                    ),
                )?;
            }
            // insert parameters into local
            for param in params.iter() {
                local_context.vars.remove(param);
                local_context.add_name(
                    param,
                    Type(temporal_ty_param.clone(), SimpleType::Undefined),
                )?;
            }

            // intial inference
            if is_first_time {
                let typed_body = infer_expr(
                    *body.clone(),
                    &mut local_context,
                    until_dependencies,
                    udep_map,
                )?;
                // `f(x) = 2`'s return type is just gonna be `Option<Int>`.
                // May later change inference_algo to detect if `&` is used to decide whether or not type is Optional.
                let simpl_return_type =
                    SimpleType::Option(Box::new(type_of_typedexpr(typed_body.clone()).get_simpl()));

                local_context.add_name(
                    &name,
                    Type(
                        temporal_ty_param.clone(),
                        SimpleType::Predicate(arg_types.clone(), Box::new(simpl_return_type)),
                    ),
                )?;
            }

            let typed_body = infer_expr(*body, &mut local_context, until_dependencies, udep_map)?;

            // `f(x) = 2`'s return type is just gonna be `Option<Int>`.
            // May later change inference_algo to detect if `&` is used to decide whether or not type is Optional.
            let simpl_return_type =
                SimpleType::Option(Box::new(type_of_typedexpr(typed_body.clone()).get_simpl()));

            Ok(TypedExpr::TEPred(
                name,
                params,
                Box::new(typed_body),
                Type(
                    temporal_undefined,
                    SimpleType::Predicate(arg_types, Box::new(simpl_return_type)),
                ),
            ))
        }
    }
}

pub fn infer_command(
    cmd: ast::Command,
    ctx: &mut TypeContext,
    udep_map: &mut HashMap<usize, ast::TypedExpr>,
    is_assert: bool,
) -> Result<ast::TypedCommand, errors::CompileTimeError> {
    /*
        look into doc-strings
    */
    use ast::{Command, TypedCommand};
    use types::{SimpleType, TemporalType, Type};

    let mut blank_until_dependencies = types::UntilDependencies {
        weak: Vec::new(),
        strong: Vec::new(),
    };
    /*
        Global  : current, always   , false
        Next    : next   , fleeting , false
        Current : current, fleeting , false
        Until   : current, lingering, true

        Next(Until)    : next   , fleeting , true
        Global(Until)  : current, always   , true
        Global(Future) : future , always   , false

        Global(Future(Until)) : future, always, true
    */
    match cmd {
        Command::Timestep => {
            panic!("Huh, timesteps should've been taken out with `split`... Why was `infer_command` called on one?")
        }
        Command::Global(v, e) => {
            let te = infer_expr(e, ctx, &mut blank_until_dependencies, udep_map)?;
            let t = if blank_until_dependencies.is_empty() {
                Type(
                    TemporalType {
                        when_available: types::TemporalAvailability::Current,
                        when_dissipates: types::TemporalPersistency::Always,
                        is_until: None,
                    },
                    ast::type_of_typedexpr(te.clone()).get_simpl(),
                )
            } else {
                Type(
                    TemporalType {
                        when_available: types::TemporalAvailability::Current,
                        when_dissipates: types::TemporalPersistency::Always,
                        is_until: Some(blank_until_dependencies),
                    },
                    ast::type_of_typedexpr(te.clone()).get_simpl(),
                )
            };
            let te = ast::new_texpr(te, t.clone(), "Idk how Command-lvl type inference failed. This is only used to make until-dependency analysis convenient later down the pipe.".to_string())?;
            if !is_assert {
                ctx.add_name(&v, t)?;
            }
            Ok(TypedCommand::TGlobal(v, te))
        }
        Command::Next(v, e) => {
            let te = infer_expr(e, ctx, &mut blank_until_dependencies, udep_map)?;
            let t = if blank_until_dependencies.is_empty() {
                Type(
                    TemporalType {
                        when_available: types::TemporalAvailability::Next,
                        when_dissipates: types::TemporalPersistency::Fleeting,
                        is_until: None,
                    },
                    ast::type_of_typedexpr(te.clone()).get_simpl(),
                )
            } else {
                Type(
                    TemporalType {
                        when_available: types::TemporalAvailability::Next,
                        when_dissipates: types::TemporalPersistency::Fleeting,
                        is_until: Some(blank_until_dependencies),
                    },
                    ast::type_of_typedexpr(te.clone()).get_simpl(),
                )
            };
            let te = ast::new_texpr(te, t.clone(), "Idk how Command-lvl type inference failed. This is only used to make until-dependency analysis convenient later down the pipe.".to_string())?;
            if !is_assert {
                ctx.add_name(&v, t)?;
            }
            Ok(TypedCommand::TNext(v, te))
        }
        Command::Update(v, e) => {
            let te = infer_expr(e, ctx, &mut blank_until_dependencies, udep_map)?;
            let t = if blank_until_dependencies.is_empty() {
                Type(
                    TemporalType {
                        when_available: types::TemporalAvailability::Next,
                        when_dissipates: types::TemporalPersistency::Fleeting,
                        is_until: Some(blank_until_dependencies),
                    },
                    ast::type_of_typedexpr(te.clone()).get_simpl(),
                )
            } else {
                Type(
                    TemporalType {
                        when_available: types::TemporalAvailability::Next,
                        when_dissipates: types::TemporalPersistency::Fleeting,
                        is_until: Some(blank_until_dependencies),
                    },
                    ast::type_of_typedexpr(te.clone()).get_simpl(),
                )
            };
            let te = ast::new_texpr(te, t.clone(), "Idk how Command-lvl type inference failed. This is only used to make until-dependency analysis convenient later down the pipe.".to_string())?;
            if !is_assert {
                ctx.add_name(&v, t)?;
            }
            Ok(TypedCommand::TUpdate(v, te))
        }
        Command::Finally(v, e) => {
            let te = infer_expr(e, ctx, &mut blank_until_dependencies, udep_map)?;
            let t = if blank_until_dependencies.is_empty() {
                Type(
                    TemporalType {
                        when_available: types::TemporalAvailability::Future,
                        when_dissipates: types::TemporalPersistency::Always,
                        is_until: None,
                    },
                    ast::type_of_typedexpr(te.clone()).get_simpl(),
                )
            } else {
                Type(
                    TemporalType {
                        when_available: types::TemporalAvailability::Future,
                        when_dissipates: types::TemporalPersistency::Always,
                        is_until: Some(blank_until_dependencies),
                    },
                    ast::type_of_typedexpr(te.clone()).get_simpl(),
                )
            };
            let te = ast::new_texpr(te, t.clone(), "Idk how Command-lvl type inference failed. This is only used to make until-dependency analysis convenient later down the pipe.".to_string())?;
            if !is_assert {
                ctx.add_name(&v, t)?;
            }
            Ok(TypedCommand::TFinally(v, te))
        }
        Command::Print(e) => {
            let te = infer_expr(e, ctx, &mut blank_until_dependencies, udep_map)?;
            if blank_until_dependencies.is_empty() {
                Ok(TypedCommand::TPrint(te))
            } else {
                Err(errors::TemporalConflictError {
                    message: "Why are you trying to print an until expression? Just simplify it..."
                        .to_string(),
                })?
            }
        }
        Command::Assert(assertion) => {
            let assertion = *assertion;
            let mut local = ctx.clone();

            let free_vars = free_vars_of_command(&assertion);
            let target: Var = match assertion.clone() {
                Command::Timestep | Command::Print(_) | Command::Assert(_) => {
                    panic!("This shouldn't be inside a `Command:Assert`.")
                }
                Command::Global(v, _)
                | Command::Next(v, _)
                | Command::Update(v, _)
                | Command::Finally(v, _) => v,
            };

            // for static checking, we pretend the variables are always available when needed.
            let temporal_ty_assert = TemporalType {
                when_available: types::TemporalAvailability::Current,
                when_dissipates: types::TemporalPersistency::Always,
                is_until: None,
            };

            for v in free_vars.iter().chain(iter::once(&target)) {
                local.vars.remove(v);
                local.add_name(v, Type(temporal_ty_assert.clone(), SimpleType::Undefined))?
            }
            Ok(TypedCommand::TAssert(Box::new(infer_command(
                assertion, &mut local, udep_map, true,
            )?)))
        }
    }
}

// x <- 3
/*
    Next(Until(x = 3, true))  # this allows reassignment, but it maintains forever if not violated (or reassigned).
*/

/*
    | x = e         // Global
    | x <X e        // Next
    | x <- e        // Next(Until)
    | x <.. e       // Global(Future)
*/
pub fn dupe_check_cmd(
    cmd: ast::Command,
    var_assign: &mut HashMap<ast::Var, (bool, bool, bool)>,
) -> Result<(), errors::DupeAssignError> {
    use ast::Command;
    match cmd {
        Command::Timestep => {
            panic!("Huh, timesteps should've been taken out with `split`... Why was `dupe_check` called on one?")
        }
        Command::Global(v, _) => {
            if var_assign.get(&v).unwrap_or(&(false, false, false)).0 {
                Err(errors::DupeAssignError
                    {message : "Attempted Global assignment to a variable that was already Globally assigned to inside this timestep.".to_string()})
            } else {
                if let Some(tup) = var_assign.clone().get(&v) {
                    var_assign.insert(v, (true, tup.1, tup.2));
                } else {
                    var_assign.insert(v, (false, false, false));
                }
                Ok(())
            }
        }
        Command::Next(v, _) => {
            if var_assign.get(&v).unwrap_or(&(false, false, false)).1 {
                Err(errors::DupeAssignError
                    {message : "Attempted Next assignment to a variable that was already Nextly or Updatedly assigned to inside this timestep.".to_string()})
            } else {
                if let Some(tup) = var_assign.clone().get(&v) {
                    var_assign.insert(v, (tup.1, true, tup.2));
                } else {
                    var_assign.insert(v, (false, false, false));
                }
                Ok(())
            }
        }
        Command::Update(v, _) => {
            if var_assign.get(&v).unwrap_or(&(false, false, false)).1 {
                Err(errors::DupeAssignError
                    {message : "Attempted Update assignment to a variable that was already Nextly or Updatedly assigned to inside this timestep.".to_string()})
            } else {
                if let Some(tup) = var_assign.clone().get(&v) {
                    var_assign.insert(v, (tup.1, true, tup.2));
                } else {
                    var_assign.insert(v, (false, false, false));
                }
                Ok(())
            }
        }
        Command::Finally(v, _) => {
            if var_assign.get(&v).unwrap_or(&(false, false, false)).1 {
                Err(errors::DupeAssignError
                    {message : "Attempted Finally assignment to a variable that was already Finally assigned to inside this timestep.".to_string()})
            } else {
                if let Some(tup) = var_assign.clone().get(&v) {
                    var_assign.insert(v, (tup.1, true, tup.2));
                } else {
                    var_assign.insert(v, (false, false, false));
                }
                Ok(())
            }
        }
        Command::Print(_) | Command::Assert(_) => Ok(()),
    }
}

pub fn dupe_check(block: Vec<ast::Command>) -> Result<(), errors::AssignError> {
    // {var_name : (is_global, is_next_or_update, is_future)}
    let mut var_assign = HashMap::new();
    block
        .iter()
        .try_for_each(|cmd| dupe_check_cmd(cmd.clone(), &mut var_assign))?;
    Ok(())
}

pub fn infer_timeblock(
    block: Vec<ast::Command>,
    ctx: &mut TypeContext,
    udep_map: &mut HashMap<usize, ast::TypedExpr>,
) -> Result<ast::TypedTimeBlock, errors::CompileTimeError> {
    let cmds: Result<ast::TypedTimeBlock, _> = block
        .iter()
        .map(|cmd| infer_command(cmd.clone(), ctx, udep_map, false))
        .collect();
    ctx.step_time();

    cmds
}

pub fn free_vars_of_expr(e: ast::Expr) -> ast::FreeVars {
    use ast::Expr;
    match e {
        Expr::EConst(_) => ast::FreeVars::new(),
        Expr::EVar(v) => {
            let mut free_vars = ast::FreeVars::new();
            free_vars.insert(v);
            free_vars
        }
        Expr::EBinop(_, e1, e2) => free_vars_of_expr(*e1)
            .union(&free_vars_of_expr(*e2))
            .cloned()
            .collect(),
        Expr::EUnop(_, e1) => free_vars_of_expr(*e1),
        Expr::EInput => ast::FreeVars::new(),
        Expr::ECall(name, args) => {
            let name = HashSet::from([name]);
            let free_vars = args.into_iter().fold(name, |vars, x| {
                vars.union(&free_vars_of_expr(x))
                    .map(|v| v.to_owned())
                    .collect::<HashSet<Var>>()
            });
            free_vars
        }
        Expr::EPred(name, args, body) => {
            let body_free_vars = free_vars_of_expr(*body);
            let defined_vars = ast::FreeVars::from_iter(args.into_iter().chain(iter::once(name)));
            body_free_vars
                .difference(&defined_vars)
                .map(|v| v.to_owned())
                .collect::<HashSet<Var>>()
        }
    }
}

pub fn free_vars_of_command(cmd: &ast::Command) -> ast::FreeVars {
    // todo: set subtraction if propositions are implemented. `f(x) = x * y` ==> `free_vars = {x, y} - {x} = {y}`
    use ast::Command;
    match cmd.clone() {
        Command::Timestep => panic!("Huh, timesteps should've been taken out with `split`... Why was `infer_command` called on one?"),
        Command::Global(_v, e) | Command::Next(_v, e) | Command::Update(_v, e) | Command::Finally(_v, e) => {
            free_vars_of_expr(e.clone())
        },
        Command::Print(e) => free_vars_of_expr(e),
        Command::Assert(c) => free_vars_of_command(&*c)
    }
}

// doesn't capture "undefined variables" errors.
pub fn defined_var_of_command(cmd: &ast::Command) -> Option<ast::Var> {
    use ast::Command;
    match cmd.clone() {
        Command::Timestep => panic!("Huh, timesteps should've been taken out with `split`... Why was `infer_command` called on one?"),
        Command::Global(v, _) => Some(v),
        Command::Next(_v, _) => None, // doesn't have an effect in the current time block. thus, doesn't matter in `arrange_by_dependencies`.
        Command::Update(_v, _) => None,
        Command::Finally(v, _) => Some(v),
        Command::Print(_) => None,
        Command::Assert(_) => None
    }
}

pub fn arrange_by_dependencies(
    block: Vec<ast::Command>,
) -> Result<Vec<ast::Command>, errors::CircularAssignError> {
    let a = block
        .iter()
        .map(|cmd| (defined_var_of_command(cmd), cmd.clone()));
    let defined_vars: ast::DefVars = HashSet::from_iter(
        a.clone()
            .filter(|(opt, _)| opt.is_some())
            .map(|(opt, _)| opt.unwrap()),
    );
    let delayed_commands: Vec<ast::Command> = a
        .filter(|(opt, _)| opt.is_none())
        .map(|(_, cmd)| cmd)
        .collect();

    let free_vars_by_cmd: HashMap<Var, ast::FreeVars> = HashMap::from_iter(
        block
            .iter()
            .map(
                // `cmd.free_vars & defined_vars` because we don't care about timeblock-wide free variables. we care only about command's free variables.
                // also, `graph.add_edge` will give an error if it tries to connect a nonexistant node with an existing/nonexisting node.
                |cmd| {
                    (
                        defined_var_of_command(cmd),
                        free_vars_of_command(cmd)
                            .intersection(&defined_vars)
                            .map(|var| var.clone())
                            .collect(),
                    )
                },
            )
            .filter(|(def_var, _)| def_var.is_some())
            .map(|(def_var, free_vars)| (def_var.unwrap(), free_vars)), // if `defined_vars.contains(tup.0)`
    );

    // scan for recursive definitions. since `defined_vars_of_command` excludes next/update, all recursive definitions are blocked.

    let mut graph = Graph::<Var, i32>::new();

    let var_map: HashMap<Var, NodeIndex> = defined_vars
        .into_iter()
        .map(|def_var| (def_var.clone(), graph.add_node(def_var)))
        .collect();
    // for var in defined_vars.into_iter() {
    //     let origin = graph.add_node(var);
    // }
    for (origin, destinations) in free_vars_by_cmd.iter() {
        for dest in destinations {
            graph.add_edge(
                *var_map.get(dest).unwrap(),
                *var_map.get(origin).unwrap(),
                1,
            );
        }
    }

    match toposort(&graph, None) {
        Ok(order) => {
            let top_order: Vec<Var> = order
                .iter()
                .map(|node| graph.node_weight(node.clone()).unwrap().clone())
                .collect();

            let mut cmd_by_var: HashMap<Var, Vec<ast::Command>> = HashMap::new();
            for cmd in block.into_iter() {
                if let Some(def_var) = defined_var_of_command(&cmd) {
                    cmd_by_var.entry(def_var).or_default().push(cmd);
                }
            }

            let mut cmd_order: Vec<ast::Command> = Vec::new();
            for var in top_order.iter() {
                cmd_order.append(&mut cmd_by_var.get(var).unwrap().clone());
            }
            cmd_order.append(&mut delayed_commands.clone());
            Ok(cmd_order)
        }
        Err(err) => Err(errors::CircularAssignError {
            message: format!(
                "Error: Graph has cycle with node: {}",
                graph.node_weight(err.node_id()).unwrap().as_str()
            )
            .to_string(),
        }),
    }
}

pub fn infer_program(program: ast::Program) -> Result<ast::TypedProgram, errors::CompileTimeError> {
    let mut ctx = TypeContext {
        vars: HashMap::new(),
        clock: 0,
    };

    let mut udep_map: HashMap<usize, ast::TypedExpr> = HashMap::new();

    // don't filter out empty blocks since `---\n---` is valid.
    let time_blocks: Vec<_> = program
        .split(|cmd| *cmd == ast::Command::Timestep)
        .map(|tb| tb.to_vec())
        .collect();

    time_blocks
        .clone()
        .iter()
        .try_for_each(|tb| dupe_check(tb.clone()))?;

    let arranged_program: Vec<Result<Vec<ast::Command>, errors::CircularAssignError>> = time_blocks
        .into_iter()
        .map(|tb| arrange_by_dependencies(tb))
        .collect();
    let arranged_program: Result<Vec<Vec<ast::Command>>, errors::CircularAssignError> =
        arranged_program.into_iter().collect();
    let arranged_program = arranged_program?;

    let inferred_program: Vec<Vec<ast::TypedCommand>> = arranged_program
        .iter()
        .map(|block| infer_timeblock(block.to_vec(), &mut ctx, &mut udep_map))
        .collect::<Result<_, _>>()?;
    Ok(ast::TypedProgram {
        code: inferred_program,
        udep_map: udep_map,
    })
}
