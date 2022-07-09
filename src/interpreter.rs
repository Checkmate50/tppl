use std::cmp::Eq;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::iter;

use crate::ast::type_of_typedexpr;
use crate::types;
use crate::{ast, builtins};
use crate::{ast_printer, errors};
use ast::{TypedCommand, TypedExpr, Var};
use types::{SimpleType, TemporalType, Type};

pub type Value = (Type, TypedExpr);

type Count = u8;
#[derive(Clone, PartialEq, Debug)]
pub struct VarContext {
    // Vec<ValueID> rather than HashSet<ValueID> since the length will be at most 3, so "O(n) vs O(1) remove" doesn't mean anything.
    pub vars: HashMap<Var, (Vec<(TemporalType, TypedExpr)>, SimpleType)>,
    /*
    Of the form (independent, dependencies).
    For example, ```
        x = 3 <!!> y & c
        a = x <!> y
    ``` would be `{y: {x, a}, c : {x}}`
    */
    pub until_dependencies_tracker: HashMap<Var, HashMap<Var, Count>>,
    // key is `hash(texpr)` and value is `texpr`
    pub udep_map: HashMap<usize, TypedExpr>,
    pub clock: i32,
}

impl VarContext {
    pub fn look_up(&mut self, var_name: &Var) -> Result<Vec<Value>, errors::ExecutionTimeError> {
        if builtins::is_builtin(var_name) {
            Err(errors::PredicateExprError {
                message: format!(
                    "Name {} is taken by a builtin predicate. No need to internally look it up.",
                    var_name
                )
                .to_string(),
            })?
        }

        if let Some((temp_texpr_pairs, simpl)) = self.vars.get(var_name) {
            if filter_out_nexts(temp_texpr_pairs).len() == 0 {
                Err(errors::CurrentlyUnavailableError {
                    message: format!(
                        "Name {} doesn't have any temporal types that are currently available.",
                        var_name
                    )
                    .to_string(),
                })?
            } else {
                if simpl.is_predicate() {
                    if let Some(temps) = sort_texprs_by_immediacy(temp_texpr_pairs) {
                        let values: Vec<Value> = temps
                            .into_iter()
                            .map(|(temp, texpr)| (Type(temp, simpl.clone()), texpr))
                            .collect();

                        Ok(values)
                    } else {
                        Err(errors::CurrentlyUnavailableError {
                            message: format!(
                                "Name {} doesn't have any temporal types that are currently available.",
                                var_name
                            )
                            .to_string(),
                        })?
                    }
                } else {
                    if let Some((temp, texpr)) = get_most_immediate_texpr(temp_texpr_pairs) {
                        Ok([(Type(temp, simpl.to_owned()), texpr)].to_vec())
                    } else {
                        Err(errors::CurrentlyUnavailableError {
                            message: format!(
                                "Name {} doesn't have any temporal types that are currently available.",
                                var_name
                            )
                            .to_string(),
                        })?
                    }
                }
            }
        } else {
            Err(errors::NameError {
                message: format!("There is no variable with name {}", var_name).to_string(),
            })?
        }
    }

    pub fn add_var(
        &mut self,
        var_name: &Var,
        data: &TypedExpr,
        typ: &Type,
    ) -> Result<(), errors::ExecutionTimeError> {
        if builtins::is_builtin(var_name) {
            Err(errors::BuiltinNameConflictError {
                message: format!(
                    "Name {} can't be assigned to since it's the name of a built-in predicate.",
                    var_name
                )
                .to_string(),
            })?
        }

        if let Some((temp_texpr_pairs, simpl)) = self.vars.clone().get(var_name) {
            let mut temp_texpr_pairs = temp_texpr_pairs.clone();

            let simpl = types::resolve_simple_conflicts(simpl.to_owned(), typ.get_simpl())?;

            if !typ.get_simpl().is_predicate() {
                let temps: Vec<TemporalType> = temp_texpr_pairs
                    .iter()
                    .map(|(temp, _)| temp.to_owned())
                    .collect();
                let temp = typ.get_temporal();
                types::resolve_temporal_conflicts(temps, temp.clone(), false)?;
                temp_texpr_pairs.push((temp, data.to_owned()));
            } else {
                let temp = typ.get_temporal();
                temp_texpr_pairs.push((temp, data.to_owned()));
            }
            self.vars
                .insert(var_name.to_owned(), (temp_texpr_pairs, simpl.to_owned()));
        } else {
            self.vars.insert(
                var_name.to_owned(),
                (
                    [(typ.get_temporal(), data.to_owned())].to_vec(),
                    typ.get_simpl(),
                ),
            );
        }

        if let Some(until_dependencies) = typ.get_temporal().is_until {
            self.add_until_dependencies(var_name, &until_dependencies);
        }

        self.detect_cycle_until_dep(var_name)?;

        self.refresh_dependents(var_name)?;

        Ok(())
    }

    fn get_from_hashmap<'a, K: Hash + Eq, V>(hm: &'a HashMap<K, V>, key: &'a K) -> Option<&'a V> {
        match hm.get(key) {
            Some(x) => Some(x),
            None => None,
        }
    }

    pub fn refresh_dependents(&mut self, var_name: &Var) -> Result<(), errors::ExecutionTimeError> {
        if let Some(dependents) = self.until_dependencies_tracker.clone().get(var_name) {
            for (dep_name, _) in dependents.iter() {
                let vars = &self.vars.clone();
                let (temp_texpr_pairs, _) = Self::get_from_hashmap(vars, dep_name).unwrap();
                for (temp, texpr) in temp_texpr_pairs.iter() {
                    if let Some(until_conds) = temp.is_until.clone() {
                        let udep_map = &self.udep_map;
                        let mut context_clone = self.clone();
                        let evaled_conds: Result<Vec<_>, errors::ExecutionTimeError> = until_conds
                            .strong
                            .iter()
                            .chain(until_conds.weak.iter())
                            .map(|until_cond| {
                                let cond_texpr =
                                    Self::get_from_hashmap(udep_map, until_cond).unwrap();
                                eval_expr(cond_texpr.to_owned(), &mut context_clone)
                            })
                            .collect();
                        if evaled_conds?.into_iter().any(boolify) {
                            self.destruct_value(
                                dep_name.clone(),
                                (temp.to_owned(), texpr.to_owned()),
                            )?;
                        }
                    }
                }
            }
        }
        Ok(())
    }

    pub fn age_values(
        &mut self,
        temp_texpr_pairs: Vec<(TemporalType, TypedExpr)>,
        name: String,
    ) -> Result<(Vec<(TemporalType, TypedExpr)>, Vec<Var>), errors::ExecutionTimeError> {
        let mut nexts: Vec<Var> = Vec::new();
        let mut aged_pairs: Vec<(TemporalType, TypedExpr)> = Vec::new();
        let mut aged_temps: Vec<TemporalType> = Vec::new();

        for temp_texpr_pair in temp_texpr_pairs.into_iter() {
            let (temp, te) = temp_texpr_pair;
            if let Some(aged_temp) = types::advance_type(temp.clone()) {
                if !types::is_currently_available(&temp)
                    && types::is_currently_available(&aged_temp)
                {
                    // NEXT -> CURRENT. no need to do CURRENT -> EXPIRED since expired doesn't affect until_conds, and current would've already.
                    nexts.push(name.clone());
                }
                types::resolve_temporal_conflicts(aged_temps.clone(), aged_temp.clone(), false)?;
                aged_temps.push(aged_temp.clone());
                aged_pairs.push((aged_temp, te));
            } else {
                self.destruct_value(name.clone(), (temp, te))?;
            }
        }
        Ok((aged_pairs, nexts))
    }

    pub fn step_time(&mut self) -> Result<(), errors::ExecutionTimeError> {
        self.clock += 1;

        let mut all_nexts: Vec<Var> = Vec::new();
        let context: Result<
            HashMap<Var, (Vec<(TemporalType, TypedExpr)>, SimpleType)>,
            errors::ExecutionTimeError,
        > = self
            .vars
            .clone()
            .into_iter()
            .map(|(name, (temp_texpr_pairs, simpl))| {
                let (aged_pairs, mut nexts) = self.age_values(temp_texpr_pairs, name.clone())?;
                all_nexts.append(&mut nexts);
                Ok((name, (aged_pairs, simpl)))
            })
            .collect();
        self.vars = context?;

        // the value of the variables have been updated with new_currently_available values.
        for v in all_nexts.iter() {
            self.refresh_dependents(v)?;
        }
        Ok(())
    }

    pub fn add_until_dependencies(
        &mut self,
        target: &Var,
        until_dependencies: &types::UntilDependencies,
    ) -> () {
        let weak = HashSet::<_>::from_iter(
            until_dependencies
                .weak
                .iter()
                .map(|id| self.udep_map.get(id).unwrap().to_owned())
                .map(free_vars_of_texpr)
                .flatten(),
        );
        let strong = HashSet::<_>::from_iter(
            until_dependencies
                .strong
                .iter()
                .map(|id| self.udep_map.get(id).unwrap().to_owned())
                .map(free_vars_of_texpr)
                .flatten(),
        );

        // add to until_dependencies
        for independent in weak.union(&strong) {
            let dependents = self
                .until_dependencies_tracker
                .entry(independent.to_string())
                .or_insert(HashMap::new());

            if let Some(count) = dependents.clone().get(target) {
                dependents.insert(target.clone(), count + 1);
            } else {
                dependents.insert(target.clone(), 1);
            }
        }
    }

    pub fn destruct_value(
        &mut self,
        var_name: Var,
        temp_texpr: (TemporalType, TypedExpr),
    ) -> Result<(), errors::ExecutionTimeError> {
        let (temp, _) = temp_texpr.clone();

        // check the Strong until_conditions if they're present
        if let Some(until_conds) = temp.is_until {
            for strong in until_conds.strong.iter() {
                let strong_cond_texpr = self.udep_map.get(strong).unwrap().clone();
                let res = eval_expr(strong_cond_texpr, self)?;
                if !(boolify(res)) {
                    Err(errors::SUntilConditionUnsatisfied
                        {message : format!("A StrongUntil condition was not satisfied by the time of the value's destruction.\n temp_texpr = {:?}\n unsatisfied_condition = {:?}", temp_texpr, strong)})?
                }
            }
        }

        // delete the value from `self`
        if let Some((temp_texpr_pairs, simpl)) = self.vars.clone().get(&var_name) {
            // remove once, thus `position` rather than `filter`
            let mut temp_texpr_pairs = temp_texpr_pairs.to_owned();

            if let Some(pos) = temp_texpr_pairs
                .clone()
                .into_iter()
                .position(|x| x == temp_texpr)
            {
                temp_texpr_pairs.remove(pos);
                self.vars
                    .insert(var_name.clone(), (temp_texpr_pairs, simpl.to_owned()));
            } else {
                panic!("{}", format!("Somehow, a value experienced a destruction-attempt, but it didn't even exist under `variable_name`={:?}...", var_name));
            }
        } else {
            panic!("Somehow, a `variable_name` experienced a destruction-attempt, but the `variable_name`={:?} didn't exist in VarContext...", var_name);
        }

        self.until_dependencies_tracker = self
            .until_dependencies_tracker
            .clone()
            .into_iter()
            .map(|(indp_name, mut dependents)| {
                if let Some(count) = dependents.remove(&var_name) {
                    if count > 1 {
                        dependents.insert(var_name.clone(), count - 1);
                    }
                    (indp_name, dependents)
                } else {
                    (indp_name, dependents)
                }
            })
            .collect();
        Ok(())
    }

    // checks and deletes values accordingly
    pub fn detect_cycle_until_dep(&mut self, var_name: &Var) -> Result<(), errors::AssignError> {
        if let Some(direct_dependents) = self.until_dependencies_tracker.get(var_name) {
            let mut all_dependents: HashSet<String> = HashSet::new();
            let mut stack = [direct_dependents].to_vec();

            while stack.len() > 0 {
                let dependents = stack.pop().unwrap();
                for dep in dependents.iter().map(|(dep_name, _)| dep_name) {
                    if !all_dependents.contains(dep) {
                        all_dependents.insert(dep.clone());
                        if let Some(grandchildren) = self.until_dependencies_tracker.get(dep) {
                            stack.push(grandchildren);
                        }
                    }
                }
            }

            if all_dependents.contains(var_name) {
                Err(errors::CyclicalUntilDependency {message : format!("There is a cycle in the until_conditions. This cycle contains variable {:?}", var_name)})?
            } else {
                Ok(())
            }
        } else {
            Ok(())
        }
    }
}

pub fn boolify(texpr: TypedExpr) -> bool {
    match texpr {
        TypedExpr::TEConst(c, _) => match c {
            ast::Const::Bool(b) => b,
            ast::Const::Number(_) => panic!("Truthy-falsey not implemented yet for integers."),
            ast::Const::Float(_) => panic!("Truthy-falsey not implemented yet for floats."),
        },
        _ => panic!("Truthy-falsey not implemented yet for non-constants."),
    }
}

pub fn is_success(texpr: TypedExpr) -> bool {
    match texpr {
        TypedExpr::TEConst(c, _) => match c {
            ast::Const::Bool(b) => b,
            ast::Const::Number(_) => true,
            ast::Const::Float(_) => true,
        },
        _ => panic!("Truthy-falsey not implemented yet for non-constants."),
    }
}

pub fn get_most_immediate_texpr(
    values: &Vec<(TemporalType, TypedExpr)>,
) -> Option<(TemporalType, TypedExpr)> {
    use types::TemporalAvailability;
    let most_immediate = values
        .into_iter()
        .max_by_key(|(temp, _)| match temp.when_available {
            TemporalAvailability::Current => 1,
            TemporalAvailability::Next => -1,
            TemporalAvailability::Future => 0,
            TemporalAvailability::Undefined => {
                panic!("The immediacy of `undefined` is meaningless.")
            }
        })?;

    if most_immediate.0.when_available == TemporalAvailability::Next {
        return None;
    } else {
        Some(most_immediate.to_owned())
    }
}

fn filter_out_nexts(
    temp_texpr_pairs: &Vec<(TemporalType, TypedExpr)>,
) -> Vec<(TemporalType, TypedExpr)> {
    let only_currents: Vec<(TemporalType, TypedExpr)> = temp_texpr_pairs
        .into_iter()
        .filter(|(temp, _)| temp.when_available != types::TemporalAvailability::Next)
        .map(|tup| tup.to_owned())
        .collect();

    only_currents
}

pub fn sort_texprs_by_immediacy(
    temp_texpr_pairs: &Vec<(TemporalType, TypedExpr)>,
) -> Option<Vec<(TemporalType, TypedExpr)>> {
    use types::TemporalAvailability;

    let mut only_currents = filter_out_nexts(temp_texpr_pairs);

    if only_currents.len() == 0 {
        return None;
    } else {
        only_currents.sort_by_key(|(temp, _)| match temp.when_available {
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

pub fn free_vars_of_texpr(e: TypedExpr) -> ast::FreeVars {
    match e {
        TypedExpr::TEConst(_, _) => ast::FreeVars::new(),
        TypedExpr::TEVar(v, _) => {
            let mut free_vars = ast::FreeVars::new();
            free_vars.insert(v.to_string());
            free_vars
        }
        TypedExpr::TEBinop(_, e1, e2, _) => free_vars_of_texpr(*e1)
            .union(&free_vars_of_texpr(*e2))
            .cloned()
            .collect(),
        TypedExpr::TEUnop(_, e1, _) => free_vars_of_texpr(*e1),
        TypedExpr::TEInput(_) => ast::FreeVars::new(),
        TypedExpr::TECall(name, args, _) => {
            let name = HashSet::from([name]);
            let free_vars = args.into_iter().fold(name, |vars, x| {
                vars.union(&free_vars_of_texpr(x))
                    .map(|v| v.to_owned())
                    .collect::<HashSet<Var>>()
            });
            free_vars
        }
        TypedExpr::TEPred(name, args, body, _) => {
            let body_free_vars = free_vars_of_texpr(*body);
            let defined_vars = ast::FreeVars::from_iter(args.into_iter().chain(iter::once(name)));
            body_free_vars
                .difference(&defined_vars)
                .map(|v| v.to_owned())
                .collect::<HashSet<Var>>()
        }
    }
}

pub fn eval_expr(
    e: TypedExpr,
    ctx: &mut VarContext,
) -> Result<TypedExpr, errors::ExecutionTimeError> {
    match e {
        TypedExpr::TEConst(_, _) => Ok(e),
        TypedExpr::TEVar(var_name, t) => {
            // any `FOL` violations should've been caught at CompileTime.

            let values: Vec<Value> = ctx.look_up(&var_name)?;
            let (ty, texpr) = values.get(0).unwrap();
            if types::is_currently_available(&ty.get_temporal()) {
                Ok(ast::new_texpr(
                    texpr.to_owned(),
                    t,
                    "Variable gave unexpected type.".to_string(),
                )?)
            } else {
                Err(errors::CurrentlyUnavailableError{message : format!("You tried access a value that's only available in the next timestep... Temporal Type was {:?}", ty).to_string()})?
            }
        }
        TypedExpr::TEBinop(b, e1, e2, t) => {
            use ast::Binop;
            use ast::Const;

            // only desiring constants is gucci since we're only using FOL. Thus, we can't have "propositions as values."
            // Sure, propositions are expressions, but stuff like returning a proposition?
            // `f(x) = x > .3 & g` is illegal if `g : proposition`
            let c1 = match eval_expr(*e1, ctx)? {
                TypedExpr::TEConst(c1, _) => c1,
                a => panic!(
                    "{}",
                    format!(
                        "Did not receive a constant after evaluation. Instead, recieved:\n {:?}",
                        a
                    )
                ),
            };

            // `And` is lazy.
            let c2: Option<Const> = if b.clone() != Binop::And {
                Some(match eval_expr(*e2.clone(), ctx)? {
                    TypedExpr::TEConst(c2, _) => c2,
                    b => panic!(
                        "{}",
                        format!(
                            "Did not receive a constant after evaluation. Instead, recieved:\n {:?}",
                            b
                        )
                    ),
                })
            } else {
                None
            };

            let result: Const = match b {
                Binop::Plus => match (c1, c2.unwrap()) {
                    (Const::Number(n1), Const::Number(n2)) => Const::Number(n1 + n2),
                    (Const::Number(n1), Const::Float(f2)) => Const::Float((n1 as f64) + f2),
                    (Const::Float(f1), Const::Number(n2)) => Const::Float(f1 + (n2 as f64)),
                    (Const::Float(f1), Const::Float(f2)) => Const::Float(f1 + f2),
                    _ => panic!("Type-checking failed??"),
                },
                Binop::Minus => match (c1, c2.unwrap()) {
                    (Const::Number(n1), Const::Number(n2)) => Const::Number(n1 - n2),
                    (Const::Number(n1), Const::Float(f2)) => Const::Float((n1 as f64) - f2),
                    (Const::Float(f1), Const::Number(n2)) => Const::Float(f1 - (n2 as f64)),
                    (Const::Float(f1), Const::Float(f2)) => Const::Float(f1 - f2),
                    _ => panic!("Type-checking failed??"),
                },
                Binop::Times => match (c1, c2.unwrap()) {
                    (Const::Number(n1), Const::Number(n2)) => Const::Number(n1 * n2),
                    (Const::Number(n1), Const::Float(f2)) => Const::Float((n1 as f64) * f2),
                    (Const::Float(f1), Const::Number(n2)) => Const::Float(f1 * (n2 as f64)),
                    (Const::Float(f1), Const::Float(f2)) => Const::Float(f1 * f2),
                    _ => panic!("Type-checking failed??"),
                },
                Binop::Div => match (c1, c2.unwrap()) {
                    (Const::Number(n1), Const::Number(n2)) => {
                        Const::Float((n1 as f64) / (n2 as f64))
                    }
                    (Const::Number(n1), Const::Float(f2)) => Const::Float((n1 as f64) / f2),
                    (Const::Float(f1), Const::Number(n2)) => Const::Float(f1 / (n2 as f64)),
                    (Const::Float(f1), Const::Float(f2)) => Const::Float(f1 / f2),
                    _ => panic!("Type-checking failed??"),
                },
                Binop::Until | Binop::SUntil => match (c1.clone(), c2.clone().unwrap()) {
                    (_, Const::Bool(cond)) => {
                        if cond {
                            Err(errors::UntilVoidError {
                                message: "Until's condition is true by the time of evaluation"
                                    .to_string(),
                            })?
                        } else {
                            c1
                        }
                    }
                    _ => panic!(
                        "(S)Until condition should be boolean! Not {:?}",
                        c2.unwrap()
                    ),
                },
                Binop::Or => match (c1.clone(), c2.clone().unwrap()) {
                    (Const::Bool(b1), Const::Bool(_)) => {
                        if b1 {
                            c1
                        } else {
                            c2.unwrap()
                        }
                    }
                    _ => panic!("Type-checking failed??"),
                },
                Binop::And => match c1.clone() {
                    Const::Bool(b1) => {
                        if b1 {
                            match eval_expr(*e2, ctx)? {
                                    TypedExpr::TEConst(c2, _) => c2,
                                    b => panic!(
                                        "{}",
                                        format!(
                                            "Did not receive a constant after evaluation. Instead, recieved:\n {:?}",
                                            b
                                        )
                                    ),
                                }
                        } else {
                            c1
                        }
                    }
                    _ => panic!("Type-checking failed??"),
                },
            };

            let constant = TypedExpr::TEConst(result.clone(), ast::type_of_constant(result));
            Ok(ast::new_texpr(
                constant,
                t,
                "Binop gave unexpected type.".to_string(),
            )?)
        }
        TypedExpr::TEUnop(u, e1, t) => {
            use ast::Const;
            use ast::Unop;

            let c = match eval_expr(*e1, ctx)? {
                TypedExpr::TEConst(c, _) => c,
                // todo: untils are later
                _ => panic!("Did not receive a constant after evaluation."),
            };

            let result = match u {
                Unop::Neg => match c {
                    Const::Bool(_) => panic!("Type-checking failed??"),
                    Const::Number(n) => Const::Number(-n),
                    Const::Float(f) => Const::Float(-f),
                },
                Unop::Not => match c {
                    Const::Bool(b) => Const::Bool(!b),
                    Const::Number(_) => panic!("Type-checking failed??"),
                    Const::Float(_) => panic!("Type-checking failed??"),
                },
            };
            let constant = TypedExpr::TEConst(result.clone(), ast::type_of_constant(result));
            Ok(ast::new_texpr(
                constant,
                t,
                "Unop gave unexpected type.".to_string(),
            )?)
        }
        TypedExpr::TEInput(t) => {
            let mut buffer = String::new();
            std::io::stdin()
                .read_line(&mut buffer)
                .expect("Failed to read line.");
            if let Ok(n) = buffer.trim().parse() {
                Ok(TypedExpr::TEConst(ast::Const::Number(n), t))
            } else {
                Err(errors::InputError {
                    message: "Input is not an integer!".to_string(),
                })?
            }
        }
        TypedExpr::TECall(name, args, return_type) => {
            // this ain't lazy
            let args: Result<Vec<TypedExpr>, errors::ExecutionTimeError> =
                args.into_iter().map(|arg| eval_expr(arg, ctx)).collect();
            let args: Vec<TypedExpr> = args?;

            let return_val: Result<TypedExpr, errors::ExecutionTimeError> =
                if builtins::is_builtin(&name) {
                    // don't need to constrain/check `return_type` since it's built-in.
                    Ok(builtins::exec_builtin_cmp(name, args)?)
                } else {
                    // Doesn't use `TEVar` to hold `name` since we are using a vec of predicates. `TEVar` only uses *one* value.
                    let preds = ctx.look_up(&name)?;

                    let currents: Vec<Value> = preds
                        .clone()
                        .into_iter()
                        .filter(|(Type(temp, _), _)| {
                            temp.when_available == types::TemporalAvailability::Current
                        })
                        .collect();
                    let futures: Vec<Value> = preds
                        .into_iter()
                        .filter(|(Type(temp, _), _)| {
                            temp.when_available == types::TemporalAvailability::Future
                        })
                        .collect();

                    let curr_successes =
                        run_predicate_definitions(name.clone(), currents, args.clone(), ctx)?;

                    if curr_successes.len() == 0 {
                        let futu_successes = run_predicate_definitions(name, futures, args, ctx)?;
                        if futu_successes.len() == 0 {
                            Err(errors::NoPredicateError {
                                message: "No Predicates succeeded".to_string(),
                            })?
                        } else if futu_successes.len() > 1 {
                            Err(errors::MultiplePredicateError {
                                message: "Multiple (future) Predicates succeeded".to_string(),
                            })?
                        } else {
                            Ok(futu_successes.get(0).unwrap().to_owned())
                        }
                    } else if curr_successes.len() > 1 {
                        Err(errors::MultiplePredicateError {
                            message: "Multiple (current) Predicates succeeded".to_string(),
                        })?
                    } else {
                        Ok(curr_successes.get(0).unwrap().to_owned())
                    }
                };
            Ok(ast::new_texpr(
                return_val?,
                return_type,
                "Predicate Invocation didn't return desired type.".to_string(),
            )?)
        }
        TypedExpr::TEPred(name, params, body, t) => {
            if builtins::is_builtin(&name) {
                Err(errors::PredicateExprError {
                    message: "Predicates that share a name with a builtin cannot be created."
                        .to_string(),
                })?
            }
            Ok(TypedExpr::TEPred(name, params, body, t))
        }
    }
}

pub fn run_predicate_definitions(
    name: Var,
    definitions: Vec<Value>,
    args: Vec<TypedExpr>,
    ctx: &mut VarContext,
) -> Result<Vec<TypedExpr>, errors::ExecutionTimeError> {
    let temporal_undefined = TemporalType {
        when_available: types::TemporalAvailability::Undefined,
        when_dissipates: types::TemporalPersistency::Undefined,
        is_until: None,
    };

    let mut successes: Vec<TypedExpr> = Vec::new();
    for (Type(_, simpl), pred) in definitions.into_iter() {
        if let SimpleType::Predicate(arg_ts, ret) = simpl {
            let mut local_context = ctx.clone();
            if let TypedExpr::TEPred(_, params, body, _) = pred {
                for ((param, arg), arg_t) in params
                    .iter()
                    .zip(args.clone().into_iter())
                    .zip(arg_ts.into_iter())
                {
                    local_context.vars.remove(param);
                    let arg = ast::new_texpr(
                        arg,
                        Type(temporal_undefined.clone(), *arg_t),
                        "Predicate didn't receive expected types of arguments.".to_string(),
                    )?;
                    local_context.add_var(param, &arg, &type_of_typedexpr(arg.clone()))?;
                }

                let val = eval_expr(*body, &mut local_context)?;
                if is_success(val.clone()) {
                    let type_checked_val = ast::new_texpr(
                        val,
                        Type(temporal_undefined.clone(), *ret),
                        "predicate gave a funky type".to_string(),
                    )?;
                    successes.push(type_checked_val);
                }
            } else {
                panic!("There shouldn't be any non-predicates..?")
            }
        } else {
            Err(errors::ImproperCallError {
                message: format!(
                    "You tried to call {} which isn't a predicate. It's a {:?}",
                    name, simpl
                )
                .to_string(),
            })?
        }
    }
    Ok(successes)
}

pub fn exec_command(
    c: TypedCommand,
    ctx: &mut VarContext,
) -> Result<(), errors::ExecutionTimeError> {
    match c {
        TypedCommand::TGlobal(v, texpr) => {
            let val = eval_expr(texpr.clone(), ctx)?;
            ctx.add_var(&v, &val, &ast::type_of_typedexpr(texpr))?;
            Ok(())
        }
        TypedCommand::TNext(v, texpr) => {
            let val = eval_expr(texpr.clone(), ctx)?;
            ctx.add_var(&v, &val, &ast::type_of_typedexpr(texpr))?;
            Ok(())
        }
        TypedCommand::TUpdate(v, texpr) => {
            let val = eval_expr(texpr.clone(), ctx)?;
            ctx.add_var(&v, &val, &ast::type_of_typedexpr(texpr))?;
            Ok(())
        }
        TypedCommand::TFinally(v, texpr) => {
            let val = eval_expr(texpr.clone(), ctx)?;
            ctx.add_var(&v, &val, &ast::type_of_typedexpr(texpr))?;
            Ok(())
        }
        TypedCommand::TPrint(texpr) => {
            // todo: handle pdfs and propositions when implemented
            let c = match eval_expr(texpr.clone(), ctx)? {
                TypedExpr::TEConst(c, _) => c,
                _ => panic!("Evaluation did not result in a constant."),
            };
            match c {
                ast::Const::Bool(b) => println!(
                    "Print({}) => {}",
                    ast_printer::string_of_expr(ast::strip_types_off_texpr(texpr)),
                    b
                ),
                ast::Const::Number(n) => println!(
                    "Print({}) => {}",
                    ast_printer::string_of_expr(ast::strip_types_off_texpr(texpr)),
                    n
                ),
                ast::Const::Float(f) => println!(
                    "Print({}) => {}",
                    ast_printer::string_of_expr(ast::strip_types_off_texpr(texpr)),
                    f
                ),
            };
            Ok(())
        }
    }
}

pub fn exec_time_block(
    time_block: ast::TypedTimeBlock,
    ctx: &mut VarContext,
) -> Result<(), errors::ExecutionTimeError> {
    println!("--------");
    time_block
        .iter()
        .try_for_each(|cmd| exec_command(cmd.clone(), ctx))
}

// todo: consider optimizing `udep_map` with reference counting.
pub fn exec_program(pgrm: ast::TypedProgram) -> Result<(), errors::ExecutionTimeError> {
    let mut ctx = VarContext {
        vars: HashMap::new(),
        until_dependencies_tracker: HashMap::new(),
        udep_map: pgrm.udep_map,
        clock: 0,
    };

    pgrm.code.iter().try_for_each(|tb| {
        ctx.step_time()?;
        exec_time_block(tb.clone(), &mut ctx)
    })?;

    for (name, (temp_texpr_pairs, _)) in ctx.clone().vars.into_iter() {
        for pair in temp_texpr_pairs.into_iter() {
            ctx.destruct_value(name.clone(), pair)?;
        }
    }

    Ok(())
}
