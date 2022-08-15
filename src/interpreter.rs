use std::cmp::Eq;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::iter;

use crate::ast::type_of_typedexpr;
use crate::{arithmetic, types};
use crate::{ast, builtins};
use crate::{ast_printer, errors};
use ast::{TypedCommand, TypedExpr, Var};
use types::{SimpleType, TemporalType, Type};

type Value = (Type, TypedExpr);

type Count = u8;
#[derive(Clone, PartialEq, Debug)]
struct VarContext {
    // Vec<ValueID> rather than HashSet<ValueID> since the length will be at most 3, so "O(n) vs O(1) remove" doesn't mean anything.
    vars: HashMap<Var, (Vec<(TemporalType, TypedExpr)>, SimpleType)>,
    /*
    Of the form (independent, dependencies).
    For example, ```
        x = 3 <!!> y & c
        a = x <!> y
    ``` would be `{y: {x, a}, c : {x}}`
    */
    until_dependencies_tracker: HashMap<Var, HashMap<Var, Count>>,
    // key is `hash(texpr)` and value is `texpr`
    udep_map: HashMap<usize, TypedExpr>,
    // VariableName : (TemporalTypeOfAssertion, ValueAsserted, was_satisfied_at_least_once)
    assertions: HashMap<Var, Vec<(TemporalType, ast::TypedExpr, bool)>>,
    clock: i32,
}

impl VarContext {
    fn look_up(&mut self, var_name: &Var) -> Result<Vec<Value>, errors::ExecutionTimeError> {
        if builtins::is_builtin(var_name) {
            Err(errors::PredicateExprError {
                message: format!(
                    "Name {} is taken by a builtin predicate. No need to internally look it up.",
                    var_name
                )
                ,
            })?
        }

        if let Some((temp_texpr_pairs, simpl)) = self.vars.get(var_name) {
            if filter_out_nexts(temp_texpr_pairs).is_empty() {
                Err(errors::CurrentlyUnavailableError {
                    message: format!(
                        "Name {} doesn't have any temporal types that are currently available.",
                        var_name
                    )
                    ,
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
                            ,
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
                            ,
                        })?
                    }
                }
            }
        } else {
            Err(errors::NameError {
                message: format!("There is no variable with name {}", var_name),
            })?
        }
    }

    fn add_var(
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
                ,
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
                .insert(var_name.to_owned(), (temp_texpr_pairs, simpl));
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

    fn refresh_dependents(&mut self, var_name: &Var) -> Result<(), errors::ExecutionTimeError> {
        if let Some(dependents) = self.until_dependencies_tracker.clone().get(var_name) {
            for (dep_name, _) in dependents.iter() {
                let vars = &self.vars.clone();
                let (temp_texpr_pairs, _) = Self::get_from_hashmap(vars, dep_name).expect("Name({dep_name}) isn't in `until_dependencies_tracker.get(Name({var_name}))`, but Name({dep_name}) is supposedly dependent on Name({var_name}).`.");
                for (temp, texpr) in temp_texpr_pairs.iter() {
                    if let Some(until_conds) = temp.is_until.clone() {
                        let udep_map = &self.udep_map;
                        let mut context_clone = self.clone();
                        let evaled_conds: Result<Vec<Option<TypedExpr>>, errors::ExecutionTimeError> = until_conds
                            .strong
                            .iter()
                            .chain(until_conds.weak.iter())
                            .map(|until_cond| {
                                let cond_texpr =
                                    Self::get_from_hashmap(udep_map, until_cond).expect("`udep_map` is missing `until_cond` {until_cond}. The only time `udep_map` is mutated (and shrunk) during run-time is `destruct_value`.");
                                eval_expr(cond_texpr.to_owned(), &mut context_clone, false)
                            })
                            .collect();
                        if evaled_conds?.into_iter().map(|opt| opt.expect("`eval_expr` shouldn't have given any `None`s since those are only returned from `eval_expr(..., is_pred=true)` (ie. this should only occur when running a predicate_definition).")).any(boolify) {
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

    fn age_values(
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
                if !type_of_typedexpr(te.clone()).get_simpl().is_predicate() {
                    types::resolve_temporal_conflicts(
                        aged_temps.clone(),
                        aged_temp.clone(),
                        false,
                    )?;
                }
                aged_temps.push(aged_temp.clone());
                aged_pairs.push((aged_temp, te));
            } else {
                self.destruct_value(name.clone(), (temp, te))?;
            }
        }
        Ok((aged_pairs, nexts))
    }

    fn add_assertion(&mut self, assertion: TypedCommand) {
        match assertion {
            TypedCommand::TGlobal(name, texpr) | TypedCommand::TNext(name, texpr) | TypedCommand::TUpdate(name, texpr) | TypedCommand::TFinally(name, texpr) => {
                if let Some(asserts_de_name) = Self::get_from_hashmap(&self.assertions, &name) {
                    let ty:Type = ast::type_of_typedexpr(texpr.clone());
                    // not going to check for temporal conflicts since they can just be caught at assertion-time.
                    let asserts_de_name = &mut asserts_de_name.clone();
                    asserts_de_name.push((ty.get_temporal(), ast::strip_untils_off_texpr(texpr), false));
                    self.assertions.insert(name, asserts_de_name.to_owned());
                } else {
                    self.assertions.insert(name, [(ast::type_of_typedexpr(texpr.clone()).get_temporal(), ast::strip_untils_off_texpr(texpr), false)].to_vec());
                }
            },
            TypedCommand::TPrint(_) | TypedCommand::TDist(_) | TypedCommand::TAssert(_) => panic!("This should've been caught during parsing. Assertions shouldn't assert prints/assertions/dist. `assert(assert(3))` `assert(print(3))` `assert(dist(uniform))`???")

        };
    }

    fn refresh_assertions(&mut self) -> Result<(), errors::ExecutionTimeError> {
        let aged = self
            .assertions
            .clone()
            .into_iter()
            .filter_map(|(name, assertion)| {
                let asserts_de_name = assertion
                    .into_iter()
                    .filter_map(|(temp, texpr, was_satisfied)| {
                        types::advance_type(temp).map(|aged_temp| (aged_temp, texpr, was_satisfied))
                    })
                    .collect::<Vec<_>>();
                if asserts_de_name.is_empty() {
                    None
                } else {
                    Some((name, asserts_de_name))
                }
            })
            .collect::<Vec<(String, Vec<(TemporalType, TypedExpr, bool)>)>>();

        // use `udep_map`
        let until_checked = aged
            .into_iter()
            .map(|(name, assertion)| {
                let asserts_de_name:Vec<(TemporalType, TypedExpr, bool)> = assertion.into_iter()
                    .map(|(temp, texpr, was_satisfied)| {
                    if let Some(until_conds) = temp.clone().is_until {
                        // shouldn't get any `None`s since those are only returned from `eval_expr(..., is_pred=true)`
                        let strong: Vec<TypedExpr> = until_conds.strong.iter().map(|cond_id| eval_expr(self.udep_map.get(cond_id).unwrap().to_owned(), self, false)).collect::<Result<Vec<Option<TypedExpr>>, errors::ExecutionTimeError>>()?.into_iter().map(Option::unwrap).collect();
                        let weak: Vec<TypedExpr> = until_conds.weak.iter().map(|cond_id| eval_expr(self.udep_map.get(cond_id).unwrap().to_owned(), self, false)).collect::<Result<Vec<Option<TypedExpr>>, errors::ExecutionTimeError>>()?.into_iter().map(Option::unwrap).collect();
                        if strong.clone().into_iter().chain(weak.into_iter()).any(boolify) {
                            // an until_dependency was truthy
                            if strong.into_iter().all(boolify) {
                                // all strongs (if they exist) are truthy
                                Ok(None)
                            } else{
                                Err(errors::SUntilConditionUnsatisfied{message:format!("An assertion of {} had an SUntil that was not satisfied by the time of destruction", name)})?
                            }
                        } else {
                            // no until_dependencies are truthy
                            Ok(Some((temp, texpr, was_satisfied)))
                        }
                    } else {
                        // no until dependencies
                        Ok(Some((temp, texpr, was_satisfied)))
                    }
                }).collect::<Result<Vec<Option<(TemporalType, TypedExpr, bool)>>, errors::ExecutionTimeError>>()?
                .into_iter()
                .flatten()
                .collect();
                if asserts_de_name.is_empty() {
                    // no assertions (either pre or post until_checkup)
                    Ok(None)
                } else {
                    Ok(Some((name, asserts_de_name)))
                }
            }
        ).collect::<Result<Vec<Option<(String, Vec<(TemporalType, TypedExpr, bool)>)>>, errors::ExecutionTimeError>>()?
        .into_iter()
        .flatten()
        .collect()
        ;
        self.assertions = until_checked;
        Ok(())
    }

    fn run_assertions(&mut self) -> Result<(), errors::ExecutionTimeError> {
        for (name, temp_assertion_pairs) in self.assertions.clone().into_iter() {
            let currents: Vec<(usize, (TemporalType, TypedExpr, bool))> = temp_assertion_pairs
                .clone()
                .into_iter()
                .enumerate()
                .filter(|(_, (temp, _, _))| {
                    temp.when_available == types::TemporalAvailability::Current
                })
                .collect();
            let futures: Vec<(usize, (TemporalType, TypedExpr, bool))> = temp_assertion_pairs
                .clone()
                .into_iter()
                .enumerate()
                .filter(|(_, (temp, _, _))| {
                    temp.when_available == types::TemporalAvailability::Future
                })
                .collect();

            let assertions_to_be_checked: Option<(Vec<(usize, TypedExpr, bool)>, bool)> =
                if currents.is_empty() {
                    if futures.is_empty() {
                        None
                    } else {
                        // forgive for now since it's `future`. it just needs to "eventually" "at some point" hold.
                        let temporarily_forgive = true;
                        Some((
                            futures
                                .into_iter()
                                .map(
                                    |(index, (_, texpr, was_sat))| match eval_expr(texpr, self, false) {
                                        Ok(Some(v)) => Ok((index, v, was_sat)),
                                        Ok(None) => panic!("shouldn't get any `None`s since those are only returned from `eval_expr(..., is_pred=true)`"),
                                        Err(e) => Err(e),
                                    },
                                )
                                .collect::<Result<Vec<_>, errors::ExecutionTimeError>>()?,
                            temporarily_forgive,
                        ))
                    }
                } else {
                    let temporarily_forgive = false;
                    Some((
                        currents
                            .into_iter()
                            .map(
                                |(index, (_, texpr, was_sat))| match eval_expr(texpr, self, false) {
                                    Ok(Some(v)) => Ok((index, v, was_sat)),
                                    Ok(None) => panic!("shouldn't get any `None`s since those are only returned from `eval_expr(..., is_pred=true)`"),
                                    Err(e) => Err(e),
                                },
                            )
                            .collect::<Result<Vec<_>, errors::ExecutionTimeError>>()?,
                        temporarily_forgive,
                    ))
                };

            let temporal_undefined = TemporalType {
                when_available: types::TemporalAvailability::Undefined,
                when_dissipates: types::TemporalPersistency::Undefined,
                is_until: None,
            };

            match eval_expr(
                TypedExpr::TEVar(
                    name.clone(),
                    Type(temporal_undefined, SimpleType::Undefined),
                ),
                self,
                false
            ) {
                Ok(Some(texpr)) => {
                    if let Some((assertions_to_be_checked, temporarily_forgive)) =
                        assertions_to_be_checked
                    {
                        let named_const = ast::const_of_texpr(texpr);
                        let assertion_results: Result<
                            Vec<(usize, bool, bool)>,
                            errors::ExecutionTimeError,
                        > = assertions_to_be_checked
                            .into_iter()
                            .map(|(index, e, was_sat)| {
                                let operands =
                                    [named_const.clone(), ast::const_of_texpr(e)].to_vec();
                                let equals: ast::Const = builtins::builtin_eq(operands)?;
                                match equals {
                                    ast::Const::Bool(b) => Ok((index, b, b || was_sat)),
                                    _ => panic!("Builtin_Eq returned a non-bool."),
                                }
                            })
                            .collect();

                        let assertion_results = assertion_results?;
                        if !temporarily_forgive
                            && !assertion_results.iter().all(|(_, x, _)| x.to_owned())
                        {
                            Err(errors::AssertionError {
                                message: format!("an assertion of {} failed.", name).to_string(),
                            })?
                        }

                        let assertion_id_wassatisfied: HashMap<usize, bool> = assertion_results
                            .into_iter()
                            .map(|(index, _, was_sat)| (index, was_sat))
                            .collect();
                        self.assertions.insert(
                            name,
                            temp_assertion_pairs
                                .into_iter()
                                .enumerate()
                                .map(|(index, (temp, texpr, was_sat))| {
                                    if let Some(was_sat) = assertion_id_wassatisfied.get(&index) {
                                        (temp, texpr, was_sat.to_owned())
                                    } else {
                                        (temp, texpr, was_sat)
                                    }
                                })
                                .collect(),
                        );
                    }
                }
                Ok(None) => panic!("shouldn't get any `None`s since those are only returned from `eval_expr(..., is_pred=true)`"),
                Err(errors::ExecutionTimeError::Access(errors::AccessError::PredExpr(e))) => {
                    Err(e)?
                }
                Err(_) => (), // we ignore assertion if it's currently impossible.
            };
        }
        Ok(())
    }

    fn step_time(&mut self) -> Result<(), errors::ExecutionTimeError> {
        self.clock += 1;

        self.run_assertions()?;

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

        self.refresh_assertions()?;

        Ok(())
    }

    fn add_until_dependencies(
        &mut self,
        target: &Var,
        until_dependencies: &types::UntilDependencies,
    ) {
        let weak = HashSet::<_>::from_iter(
            until_dependencies
                .weak
                .iter()
                .map(|id| self.udep_map.get(id).unwrap().to_owned())
                .flat_map(free_vars_of_texpr),
        );
        let strong = HashSet::<_>::from_iter(
            until_dependencies
                .strong
                .iter()
                .map(|id| self.udep_map.get(id).unwrap().to_owned())
                .flat_map(free_vars_of_texpr),
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

    fn destruct_value(
        &mut self,
        var_name: Var,
        temp_texpr: (TemporalType, TypedExpr),
    ) -> Result<(), errors::ExecutionTimeError> {
        let (temp, _) = temp_texpr.clone();

        // check the Strong until_conditions if they're present
        if let Some(until_conds) = temp.is_until {
            for strong in until_conds.strong.iter() {
                let strong_cond_texpr = self.udep_map.get(strong).unwrap().clone();
                let res = eval_expr(strong_cond_texpr, self, false)?;
                // shouldn't get any `None`s since those are only returned from `eval_expr(..., is_pred=true)`
                if !(boolify(res.unwrap())) {
                    Err(errors::SUntilConditionUnsatisfied
                        {message : format!("A StrongUntil condition was not satisfied by the time of the value's destruction.\n temp_texpr = {:?}\n unsatisfied_condition = {:?}", temp_texpr, strong)})?
                }
            }

            // free until_cond_ids
            for cond_id in until_conds.strong.iter().chain(until_conds.weak.iter()) {
                self.udep_map.remove(cond_id);
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

    fn detect_cycle_until_dep(&mut self, var_name: &Var) -> Result<(), errors::AssignError> {
        if let Some(direct_dependents) = self.until_dependencies_tracker.get(var_name) {
            let mut all_dependents: HashSet<String> = HashSet::new();
            let mut stack = [direct_dependents].to_vec();

            while !stack.is_empty() {
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

fn boolify(texpr: TypedExpr) -> bool {
    match texpr {
        TypedExpr::TEConst(c, _) => match c {
            ast::Const::Bool(b) => b,
            ast::Const::Number(_) => panic!("Truthy-falsey not implemented yet for integers."),
            ast::Const::Float(_) => panic!("Truthy-falsey not implemented yet for floats."),
            ast::Const::Pdf(_) => panic!("Truthy-falsey not implemented yet for floats."),
        },
        _ => panic!("Truthy-falsey not implemented yet for non-constants."),
    }
}

fn get_most_immediate_texpr(
    values: &Vec<(TemporalType, TypedExpr)>,
) -> Option<(TemporalType, TypedExpr)> {
    use types::TemporalAvailability;
    let most_immediate = values
        .iter()
        .max_by_key(|(temp, _)| match temp.when_available {
            TemporalAvailability::Current => 1,
            TemporalAvailability::Next => -1,
            TemporalAvailability::Future => 0,
            TemporalAvailability::Undefined => {
                panic!("The immediacy of `undefined` is meaningless.")
            }
        })?;

    if most_immediate.0.when_available == TemporalAvailability::Next {
        None
    } else {
        Some(most_immediate.to_owned())
    }
}

fn filter_out_nexts(
    temp_texpr_pairs: &Vec<(TemporalType, TypedExpr)>,
) -> Vec<(TemporalType, TypedExpr)> {
    let only_currents: Vec<(TemporalType, TypedExpr)> = temp_texpr_pairs
        .iter()
        .filter(|(temp, _)| temp.when_available != types::TemporalAvailability::Next)
        .map(|tup| tup.to_owned())
        .collect();

    only_currents
}

fn sort_texprs_by_immediacy(
    temp_texpr_pairs: &Vec<(TemporalType, TypedExpr)>,
) -> Option<Vec<(TemporalType, TypedExpr)>> {
    use types::TemporalAvailability;

    let mut only_currents = filter_out_nexts(temp_texpr_pairs);

    if only_currents.is_empty() {
        None
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

fn free_vars_of_texpr(e: TypedExpr) -> ast::FreeVars {
    match e {
        TypedExpr::TEConst(_, _) => ast::FreeVars::new(),
        TypedExpr::TEVar(v, _) => {
            let mut free_vars = ast::FreeVars::new();
            free_vars.insert(v);
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
            
            args.into_iter().fold(name, |vars, x| {
                vars.union(&free_vars_of_texpr(x))
                    .map(|v| v.to_owned())
                    .collect::<HashSet<Var>>()
            })
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

fn eval_expr(
    e: TypedExpr,
    ctx: &mut VarContext,
    is_pred: bool,
) -> Result<Option<TypedExpr>, errors::ExecutionTimeError> {
    match e {
        TypedExpr::TEConst(_, _) => Ok(Some(e)),
        TypedExpr::TEVar(var_name, t) => {
            // any `FOL` violations should've been caught at CompileTime.

            let values: Vec<Value> = ctx.look_up(&var_name)?;
            let (ty, texpr) = values.get(0).unwrap();
            if types::is_currently_available(&ty.get_temporal()) {
                Ok(Some(ast::new_texpr(
                    texpr.to_owned(),
                    t,
                    "Variable gave unexpected type.".to_string(),
                )?))
            } else {
                Err(errors::CurrentlyUnavailableError{message : format!("You tried access a value that's only available in the next timestep... Temporal Type was {:?}", ty)})?
            }
        }
        TypedExpr::TEBinop(b, e1, e2, t) => {
            use ast::Binop;
            use ast::Const;

            // only desiring constants is gucci since we're only using FOL. Thus, we can't have "propositions as values."
            // Sure, propositions are expressions, but stuff like returning a proposition?
            // `f(x) = x > .3 & g` is illegal if `g : proposition`
            let c1: Option<Const> = match eval_expr(*e1, ctx, is_pred)? {
                Some(TypedExpr::TEConst(c1, _)) => Some(c1),
                Some(a) => panic!(
                    "{}",
                    format!(
                        "Did not receive a constant after evaluation. Instead, recieved:\n {:?}",
                        a
                    )
                ),
                None => None,
            };

            let result: Option<Const> = match b {
                Binop::Plus => {
                    let c2: Option<Const> = match eval_expr(*e2, ctx, is_pred)? {
                        Some(TypedExpr::TEConst(c2, _)) => Some(c2),
                        Some(b) => panic!(
                            "{}",
                            format!(
                                "Did not receive a constant after evaluation. Instead, recieved:\n {:?}",
                                b
                            )
                        ),
                        None => None
                    };
                    match (c1, c2) {
                        (Some(c1), Some(c2)) => Some(arithmetic::add(c1, c2)?),
                        _ => None,
                    }
                }
                Binop::Minus => {
                    let c2: Option<Const> = match eval_expr(*e2, ctx, is_pred)? {
                        Some(TypedExpr::TEConst(c2, _)) => Some(c2),
                        Some(b) => panic!(
                            "{}",
                            format!(
                                "Did not receive a constant after evaluation. Instead, recieved:\n {:?}",
                                b
                            )
                        ),
                        None => None
                    };
                    match (c1, c2) {
                        (Some(c1), Some(c2)) => Some(arithmetic::sub(c1, c2)?),
                        _ => None,
                    }
                }
                Binop::Times => {
                    let c2: Option<Const> = match eval_expr(*e2, ctx, is_pred)? {
                        Some(TypedExpr::TEConst(c2, _)) => Some(c2),
                        Some(b) => panic!(
                            "{}",
                            format!(
                                "Did not receive a constant after evaluation. Instead, recieved:\n {:?}",
                                b
                            )
                        ),
                        None => None
                    };
                    match (c1, c2) {
                        (Some(c1), Some(c2)) => Some(arithmetic::mul(c1, c2)?),
                        _ => None,
                    }
                }
                Binop::Div => {
                    let c2: Option<Const> = match eval_expr(*e2, ctx, is_pred)? {
                        Some(TypedExpr::TEConst(c2, _)) => Some(c2),
                        Some(b) => panic!(
                            "{}",
                            format!(
                                "Did not receive a constant after evaluation. Instead, recieved:\n {:?}",
                                b
                            )
                        ),
                        None => None
                    };
                    match (c1, c2) {
                        (Some(c1), Some(c2)) => Some(arithmetic::div(c1, c2)?),
                        _ => None,
                    }
                }
                Binop::Until | Binop::SUntil => {
                    let c2: Option<Const> = match eval_expr(*e2, ctx, is_pred)? {
                        Some(TypedExpr::TEConst(c2, _)) => Some(c2),
                        Some(b) => panic!(
                            "{}",
                            format!(
                                "Did not receive a constant after evaluation. Instead, recieved:\n {:?}",
                                b
                            )
                        ),
                        None => None
                    };

                    match (c1, c2.clone()) {
                        (Some(c1), Some(Const::Bool(cond))) => {
                            if cond {
                                Err(errors::UntilVoidError {
                                    message: "Until's condition is true by the time of evaluation"
                                        .to_string(),
                                })?
                            } else {
                                Some(c1)
                            }
                        }
                        (Some(_), Some(_)) => panic!(
                            "(S)Until condition should be boolean! Not {:?}",
                            c2.unwrap()
                        ),
                        _ => None,
                    }
                }
                Binop::Or => {
                    let c2: Option<Const> = match eval_expr(*e2, ctx, is_pred)? {
                        Some(TypedExpr::TEConst(c2, _)) => Some(c2),
                        Some(b) => panic!(
                            "{}",
                            format!(
                                "Did not receive a constant after evaluation. Instead, recieved:\n {:?}",
                                b
                            )
                        ),
                        None => None
                    };

                    match (c1.clone(), c2.clone()) {
                        (Some(Const::Bool(b1)), Some(Const::Bool(_))) => {
                            if b1 {
                                c1
                            } else {
                                c2
                            }
                        }
                        (Some(_), Some(_)) => panic!("Type-checking failed??"),
                        _ => None,
                    }
                }
                Binop::And => match c1 {
                    Some(Const::Bool(b1)) => {
                        if b1 {
                            match eval_expr(*e2, ctx, is_pred)? {
                                    Some(TypedExpr::TEConst(c2, _)) => Some(c2),
                                    Some(b) => panic!(
                                        "{}",
                                        format!(
                                            "Did not receive a constant after evaluation. Instead, recieved:\n {:?}",
                                            b
                                        )
                                    ),
                                    None => None
                                }
                        } else {
                            None
                        }
                    }
                    Some(_) => panic!("Type-checking failed??"),
                    None => None,
                },
            };

            Ok(if let Some(result) = result {
                let constant = TypedExpr::TEConst(result.clone(), ast::type_of_constant(result));
                Some(ast::new_texpr(
                    constant,
                    t,
                    "Binop gave unexpected type.".to_string(),
                )?)
            } else {
                None
            })
        }
        TypedExpr::TEUnop(u, e1, t) => {
            use ast::Const;
            use ast::Unop;

            let c: Option<Const> = match eval_expr(*e1, ctx, is_pred)? {
                Some(TypedExpr::TEConst(c, _)) => Some(c),
                Some(_) => panic!("Did not receive a constant after evaluation."),
                None => None,
            };

            let result: Option<Const> = match u {
                Unop::Neg => {
                    if let Some(c) = c {
                        Some(arithmetic::sub(Const::Number(0), c)?)
                    } else {
                        None
                    }
                }
                Unop::Not => match c {
                    Some(Const::Bool(b)) => Some(Const::Bool(!b)),
                    Some(_) => {
                        panic!("Type-checking failed??")
                    }
                    None => None,
                },
            };

            Ok(if let Some(result) = result {
                let constant = TypedExpr::TEConst(result.clone(), ast::type_of_constant(result));
                Some(ast::new_texpr(
                    constant,
                    t,
                    "Unop gave unexpected type.".to_string(),
                )?)
            } else {
                None
            })
        }
        TypedExpr::TEInput(t) => {
            let mut buffer = String::new();
            std::io::stdin()
                .read_line(&mut buffer)
                .expect("Failed to read line.");
            if let Ok(n) = buffer.trim().parse() {
                Ok(Some(TypedExpr::TEConst(ast::Const::Number(n), t)))
            } else {
                Err(errors::InputError {
                    message: "Input is not an integer!".to_string(),
                })?
            }
        }
        TypedExpr::TECall(name, args, return_type) => {
            // this ain't lazy
            let args: Option<Vec<TypedExpr>> = args
                .into_iter()
                .map(|arg| eval_expr(arg, ctx, is_pred))
                .collect::<Result<Vec<Option<TypedExpr>>, errors::ExecutionTimeError>>()?
                .into_iter()
                .collect();

            if let Some(args) = args {
                let is_dist_splatted: bool = args.clone().into_iter().any(|arg| {
                    match ast::type_of_typedexpr(arg).get_simpl() {
                        types::SimpleType::Pdf => true,
                        _ => false,
                    }
                });

                let return_val: Result<TypedExpr, errors::ExecutionTimeError> = if is_dist_splatted
                {
                    let curr_existing_val_temp = TemporalType {
                        when_available: types::TemporalAvailability::Current,
                        when_dissipates: types::TemporalPersistency::Always,
                        is_until: None,
                    };

                    let arg_streams: Vec<Vec<TypedExpr>> = args.into_iter().map(|arg| {
                        match ast::type_of_typedexpr(arg.clone()).get_simpl() {
                            SimpleType::Pdf => {
                                match arg {
                                    TypedExpr::TEConst(ast::Const::Pdf(d), _) => arithmetic::spam_sample(d, arithmetic::SAMPLE_N).into_iter().map(|f| TypedExpr::TEConst(ast::Const::Float(f), types::Type(curr_existing_val_temp.clone(), types::SimpleType::Float))).collect::<Vec<TypedExpr>>(),
                                    _ => panic!("How did a value that's not a ast::teconst(ast::const::pdf) have type simpletype::pdf?")
                                }
                            },
                            SimpleType::Float  => vec![arg; arithmetic::SAMPLE_N],
                            SimpleType::Int => vec![arg; arithmetic::SAMPLE_N],
                            SimpleType::Bool => vec![arg; arithmetic::SAMPLE_N],
                            SimpleType::Predicate(_, _) | SimpleType::Undefined | SimpleType::Bottom => panic!("By the time of evaluation, these types shouldn't be used as an argument's type.")
                        }
                    }).collect();

                    let arg_sets: Vec<Vec<TypedExpr>> = (0..arg_streams[0].len())
                        .map(|i| {
                            arg_streams
                                .iter()
                                .map(|inner| inner[i].clone())
                                .collect::<Vec<TypedExpr>>()
                        })
                        .collect();

                    let results: Vec<Option<TypedExpr>> = arg_sets
                        .into_iter()
                        .map(|arg_set| {
                            eval_expr(
                                TypedExpr::TECall(
                                    name.clone(),
                                    arg_set,
                                    types::Type(
                                        types::TemporalType {
                                            when_available: types::TemporalAvailability::Undefined,
                                            when_dissipates: types::TemporalPersistency::Undefined,
                                            is_until: None,
                                        },
                                        types::SimpleType::Undefined,
                                    ),
                                ),
                                ctx,
                                true,
                            )
                        })
                        .collect::<Result<Vec<Option<TypedExpr>>, errors::ExecutionTimeError>>()?;

                    // any `None`s should've been handled already since that's just a "no predicate definition matches" error.
                    let consts: Vec<ast::Const> = results
                        .into_iter()
                        .map(|opt_texpr| match opt_texpr {
                            Some(TypedExpr::TEConst(c, _)) => c,
                            _ => panic!("should've received constants after evaluation"),
                        })
                        .collect();

                    let list: Vec<f64> = consts
                        .into_iter()
                        .map(|c| match c {
                            ast::Const::Bool(b) => {
                                if b {
                                    1.0
                                } else {
                                    0.0
                                }
                            }
                            ast::Const::Float(f) => f,
                            ast::Const::Number(n) => n as f64,
                            ast::Const::Pdf(d) => arithmetic::spam_sample(d, 1)[0],
                        })
                        .collect();

                    Ok(TypedExpr::TEConst(
                        ast::Const::Pdf(ast::Distribution::List(list)),
                        types::Type(curr_existing_val_temp, types::SimpleType::Pdf),
                    ))
                } else if builtins::is_builtin(&name) {
                    // don't need to constrain/check `return_type` since it's built-in.
                    Ok(builtins::exec_builtin(name, args)?)
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

                    if curr_successes.is_empty() {
                        let futu_successes = run_predicate_definitions(name, futures, args, ctx)?;
                        if futu_successes.is_empty() {
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
                Ok(Some(ast::new_texpr(
                    return_val?,
                    return_type,
                    "Predicate Invocation didn't return desired type.".to_string(),
                )?))
            } else {
                Ok(None)
            }
        }
        TypedExpr::TEPred(name, params, body, t) => {
            if builtins::is_builtin(&name) {
                Err(errors::PredicateExprError {
                    message: "Predicates that share a name with a builtin cannot be created."
                        .to_string(),
                })?
            }
            Ok(Some(TypedExpr::TEPred(name, params, body, t)))
        }
    }
}

fn run_predicate_definitions(
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
                        Type(temporal_undefined.clone(), arg_t),
                        "Predicate didn't receive expected types of arguments.".to_string(),
                    )?;
                    local_context.add_var(param, &arg, &type_of_typedexpr(arg.clone()))?;
                }

                let val = eval_expr(*body, &mut local_context, true)?;
                if let Some(val) = val {
                    // if successful
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

fn exec_command(
    c: TypedCommand,
    ctx: &mut VarContext,
) -> Result<Option<(String, ast::Const)>, errors::ExecutionTimeError> {
    match c {
        TypedCommand::TGlobal(v, texpr) => {
            // shouldn't get any `None`s since those are only returned from `eval_expr(..., is_pred=true)`")
            let val = eval_expr(texpr.clone(), ctx, false)?.unwrap();
            ctx.add_var(&v, &val, &ast::type_of_typedexpr(texpr))?;
            Ok(None)
        }
        TypedCommand::TNext(v, texpr) => {
            let val = eval_expr(texpr.clone(), ctx, false)?.unwrap();
            ctx.add_var(&v, &val, &ast::type_of_typedexpr(texpr))?;
            Ok(None)
        }
        TypedCommand::TUpdate(v, texpr) => {
            let val = eval_expr(texpr.clone(), ctx, false)?.unwrap();
            ctx.add_var(&v, &val, &ast::type_of_typedexpr(texpr))?;
            Ok(None)
        }
        TypedCommand::TFinally(v, texpr) => {
            let val = eval_expr(texpr.clone(), ctx, false)?.unwrap();
            ctx.add_var(&v, &val, &ast::type_of_typedexpr(texpr))?;
            Ok(None)
        }
        TypedCommand::TPrint(texpr) => {
            let c = match eval_expr(texpr.clone(), ctx, false)?.unwrap() {
                TypedExpr::TEConst(c, _) => c,
                _ => panic!("Evaluation did not result in a constant."),
            };
            match c {
                ast::Const::Bool(b) => println!(
                    "Print({}) => {}",
                    ast_printer::string_of_expr(ast::expr_of_texpr(texpr)),
                    b
                ),
                ast::Const::Number(n) => println!(
                    "Print({}) => {}i",
                    ast_printer::string_of_expr(ast::expr_of_texpr(texpr)),
                    n
                ),
                ast::Const::Float(f) => println!(
                    "Print({}) => {}f",
                    ast_printer::string_of_expr(ast::expr_of_texpr(texpr)),
                    f
                ),
                ast::Const::Pdf(d) => println!(
                    "Print({}) => {}",
                    ast_printer::string_of_expr(ast::expr_of_texpr(texpr)),
                    ast_printer::string_of_distribution(d)
                ),
            };
            Ok(None)
        }
        TypedCommand::TAssert(assertion) => {
            ctx.add_assertion(*assertion);
            Ok(None)
        }
        TypedCommand::TDist(texpr) => {
            let c: ast::Const = match eval_expr(texpr.clone(), ctx, false)?.unwrap() {
                TypedExpr::TEConst(c, _) => c,
                _ => panic!("Evaluation did not result in a constant."),
            };
            Ok(Some((
                ast_printer::string_of_expr(ast::expr_of_texpr(texpr)),
                c,
            )))
        }
    }
}

fn exec_time_block(
    time_block: ast::TypedTimeBlock,
    ctx: &mut VarContext,
) -> Result<Vec<(String, ast::Const)>, errors::ExecutionTimeError> {
    println!("--------");
    let dist_block: Vec<(String, ast::Const)> = time_block
        .into_iter()
        .map(|cmd| exec_command(cmd, ctx))
        .collect::<Result<Vec<Option<(String, ast::Const)>>, errors::ExecutionTimeError>>()?
        .into_iter()
        .flatten()
        .collect();

    Ok(dist_block)
}

// todo: consider optimizing `udep_map` with reference counting.
pub fn exec_program(
    pgrm: ast::TypedProgram,
) -> Result<Vec<Vec<(String, ast::Const)>>, errors::ExecutionTimeError> {
    let mut ctx = VarContext {
        vars: HashMap::new(),
        until_dependencies_tracker: HashMap::new(),
        udep_map: pgrm.udep_map,
        assertions: HashMap::new(),
        clock: 0,
    };

    let dist_queue: Vec<Vec<(String, ast::Const)>> = pgrm
        .code
        .into_iter()
        .map(|tb| {
            ctx.step_time()?;
            exec_time_block(tb, &mut ctx)
        })
        .collect::<Result<Vec<Vec<(String, ast::Const)>>, errors::ExecutionTimeError>>()?;

    for (name, (temp_texpr_pairs, _)) in ctx.clone().vars.into_iter() {
        for pair in temp_texpr_pairs.into_iter() {
            ctx.destruct_value(name.clone(), pair)?;
        }
    }

    // refresh assertions here?

    if !ctx.assertions.clone().into_iter().all(|(_, assertions)| {
        assertions.is_empty() || assertions.into_iter().all(|(_, _, was_sat)| was_sat)
    }) {
        Err(errors::AssertionError {
            message: format!(
                "The following assertions were not satisfied by the end of the program.\n{}",
                ctx.assertions
                    .into_iter()
                    .flat_map(|(name, asserts_de_name)| asserts_de_name
                        .into_iter()
                        .map(|(_, te, _)| (name.clone(), te))
                        .collect::<Vec<_>>())
                    .map(|(name, te)| name
                        + ": "
                        + &ast_printer::string_of_expr(ast::expr_of_texpr(te)))
                    .collect::<Vec<_>>()
                    .join("\n")
            )
            ,
        })?
    }

    Ok(dist_queue)
}
