use std::collections::HashMap;
use std::collections::HashSet;

use crate::ast;
use crate::errors;
use crate::types;
use ast::TypedCommand;
use ast::TypedExpr;
use ast::Var;
use types::TemporalType;
use types::Type;

pub type Value = (Type, TypedExpr);

type Count = u8;
#[derive(Clone, PartialEq, Debug)]
pub struct VarContext {
    // Vec<ValueID> rather than HashSet<ValueID> since the length will be at most 3, so "O(n) vs O(1) remove" doesn't mean anything.
    pub vars: HashMap<Var, Vec<Value>>,
    /*
    Of the form (independent, dependencies).
    For example, ```
        x = 3 <!!> y & c
        a = x <!> y
    ``` would be `{y: {x, a}, c : {x}}`
    */
    pub until_dependencies_tracker: HashMap<Var, HashMap<Var, Count>>,
    // key is `hash(texpr)` and value is `texpr`
    pub udep_map: HashMap<u64, ast::TypedExpr>,
    pub clock: i32,
}

impl VarContext {
    pub fn look_up(&mut self, var_name: &Var) -> Result<Value, errors::ExecutionTimeError> {
        println!("\n\nlookedup {}\n\nin vars:\n{:?}", var_name, self.vars);
        if let Some(values) = self.vars.get(var_name) {
            if let Some(v) = get_most_immediate_value(values.to_owned()).clone() {
                Ok(v)
            } else {
                Err(errors::CurrentlyUnavailableError {
                    message: format!(
                        "Name only has {:?}, which aren't immediately available.",
                        values
                    )
                    .to_string(),
                })?
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
        let new_val: Value = (typ.clone(), data.clone());
        println!(
            "\n\nadded to name {} value {:?}",
            var_name.clone(),
            new_val.clone()
        );
        if let Some(values) = self.vars.clone().get_mut(var_name) {
            let typs: Vec<Type> = values.iter().map(|(t, _)| t.to_owned()).collect();
            types::resolve_temporal_conflicts(typs, typ.clone(), false)?;
            values.push(new_val);
            self.vars.insert(var_name.to_owned(), values.to_owned());
        } else {
            self.vars.insert(var_name.to_owned(), [new_val].to_vec());
        }

        if let Some(until_dependencies) = typ.get_temporal().is_until {
            self.add_until_dependencies(var_name, &until_dependencies);
        }

        self.detect_cycle_until_dep(var_name)?;

        self.refresh_dependents(var_name)?;

        Ok(())
    }

    pub fn refresh_dependents(&mut self, var_name: &Var) -> Result<(), errors::ExecutionTimeError> {
        // println!("refreshing dependents of {}", var_name);
        if let Some(dependents) = self.until_dependencies_tracker.clone().get(var_name) {
            for (dep_name, _) in dependents.iter() {
                let dep_value: Value = self.look_up(dep_name)?;
                let (dep_t, _): (Type, _) = dep_value.clone();

                if let Some(until_conds) = dep_t.get_temporal().is_until {
                    for until_cond in until_conds.strong.iter().chain(until_conds.weak.iter()) {
                        let cond_texpr = self.udep_map.get(until_cond).unwrap().clone();
                        let res = eval_expr(cond_texpr, self)?;
                        if boolify(res) {
                            self.destruct_value(dep_name.clone(), dep_value.clone())?
                        }
                    }
                }
            }
        }
        Ok(())
    }

    pub fn age_values(
        &mut self,
        values: Vec<Value>,
        name: String,
    ) -> Result<(Option<Vec<Value>>, Vec<Var>), errors::ExecutionTimeError> {
        let mut nexts: Vec<Var> = Vec::new();
        let mut aged_values: Vec<Value> = Vec::new();
        let mut aged_types: Vec<Type> = Vec::new();

        for value in values.into_iter() {
            let (t, te) = value.clone();
            if let Some(aged_type) = types::advance_type(t.clone()) {
                if !types::is_currently_available(&t.get_temporal())
                    && types::is_currently_available(&aged_type.get_temporal())
                {
                    // NEXT -> CURRENT. no need to do CURRENT -> EXPIRED since expired doesn't affect until_conds, and current would've already.
                    nexts.push(name.clone());
                }
                types::resolve_temporal_conflicts(aged_types.clone(), aged_type.clone(), false)?;
                aged_types.push(aged_type.clone());
                aged_values.push((aged_type, te));
            } else {
                self.destruct_value(name.clone(), value.clone())?;
            }
        }
        if aged_values.is_empty() {
            Ok((None, [].to_vec()))
        } else {
            Ok((Some(aged_values), nexts))
        }
    }

    pub fn step_time(&mut self) -> Result<(), errors::ExecutionTimeError> {
        self.clock += 1;

        let mut all_nexts: Vec<Var> = Vec::new();
        let context: Result<Vec<(Var, Option<Vec<Value>>)>, errors::ExecutionTimeError> = self
            .vars
            .clone()
            .into_iter()
            .map(|(name, values)| {
                let (aged_value, mut nexts) = self.age_values(values, name.clone())?;
                all_nexts.append(&mut nexts);
                Ok((name, aged_value))
            })
            .collect();
        self.vars = context?
            .into_iter()
            .filter_map(|(name, aged)| {
                if let Some(aged) = aged {
                    Some((name, aged))
                } else {
                    None
                }
            })
            .collect();
        // println!("context : {:?}", self);
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
                .map(|hash_val| self.udep_map.get(hash_val).unwrap())
                .map(free_vars_of_texpr)
                .flatten(),
        );
        let strong = HashSet::<_>::from_iter(
            until_dependencies
                .strong
                .iter()
                .map(|hash_val| self.udep_map.get(hash_val).unwrap())
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
        value: Value,
    ) -> Result<(), errors::ExecutionTimeError> {
        println!("destructing {}", var_name);
        let (t, _): (Type, _) = value.clone();

        // handle the until_conditions if they're present
        if let Some(until_conds) = t.get_temporal().is_until {
            for strong in until_conds.strong.iter() {
                let strong_cond_texpr = self.udep_map.get(strong).unwrap().clone();
                let res = eval_expr(strong_cond_texpr, self)?;
                if !(boolify(res)) {
                    Err(errors::SUntilConditionUnsatisfied
                        {message : format!("A StrongUntil condition was not satisfied by the time of the value's destruction.\n value = {:?}\n unsatisfied_condition = {:?}", value, strong)})?
                }
            }
        }

        // delete the value from `self`
        let values: &mut Vec<Value> = self.vars.get_mut(&var_name).unwrap_or_else(|| panic!("Somehow, a `variable_name` experienced a destruction-attempt, but the `variable_name`={:?} didn't exist in VarContext...", var_name));
        // remove once, thus `position` rather than `filter`
        if let Some(pos) = values.clone().into_iter().position(|x| x == value) {
            values.remove(pos);
        } else {
            println!("non-existent value {:?}", value);
            println!("values of var {:?}", self.vars.get(&var_name).unwrap());
            panic!("{}", format!("Somehow, a value experienced a destruction-attempt, but it didn't even exist under `variable_name`={:?}...", var_name));
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
        },
        _ => panic!("Truthy-falsey not implemented yet for non-constants."),
    }
}

pub fn get_most_immediate_value(values: Vec<Value>) -> Option<Value> {
    use types::TemporalAvailability;

    println!("\nmost_immediate:\n{:?}\n", values);
    let most_immediate =
        values
            .into_iter()
            .max_by_key(|(t, _)| match t.get_temporal().when_available {
                TemporalAvailability::Current => 1,
                TemporalAvailability::Next => -1,
                TemporalAvailability::Future => 0,
                TemporalAvailability::Undefined => {
                    panic!("The immediacy of `undefined` is meaningless.")
                }
            })?;

    if most_immediate.0.get_temporal().when_available == TemporalAvailability::Next {
        return None;
    } else {
        Some(most_immediate)
    }
}

pub fn free_vars_of_texpr(e: &TypedExpr) -> ast::FreeVars {
    match e {
        TypedExpr::TEConst(_, _) => ast::FreeVars::new(),
        TypedExpr::TEVar(v, _) => {
            let mut free_vars = ast::FreeVars::new();
            free_vars.insert(v.to_string());
            free_vars
        }
        TypedExpr::TEBinop(_, e1, e2, _) => free_vars_of_texpr(&*e1)
            .union(&free_vars_of_texpr(&*e2))
            .cloned()
            .collect(),
        TypedExpr::TEUnop(_, e1, _) => free_vars_of_texpr(&*e1),
        TypedExpr::TInput(_) => ast::FreeVars::new(),
    }
}

pub fn type_of_constant(c: ast::Const) -> types::Type {
    let temporal_undefined = TemporalType {
        when_available: types::TemporalAvailability::Undefined,
        when_dissipates: types::TemporalPersistency::Undefined,
        is_until: None,
    };

    match c {
        ast::Const::Number(_) => types::Type(temporal_undefined, types::SimpleType::Int),
        ast::Const::Bool(_) => types::Type(temporal_undefined, types::SimpleType::Bool),
    }
}

pub fn eval_expr(
    e: TypedExpr,
    ctx: &mut VarContext,
) -> Result<TypedExpr, errors::ExecutionTimeError> {
    match e {
        TypedExpr::TEConst(_, _) => Ok(e),
        TypedExpr::TEVar(var_name, _) => {
            let (ty, texpr) = ctx.look_up(&var_name)?;
            if types::is_currently_available(&ty.get_temporal()) {
                Ok(texpr)
            } else {
                Err(errors::CurrentlyUnavailableError{message : format!("You tried access a value that's only available in the next timestep... Temporal Type was {:?}", ty).to_string()})?
            }
        }
        TypedExpr::TEBinop(b, e1, e2, _) => {
            use ast::Binop;
            use ast::Const;

            // only desiring constants is gucci since we're only using FOL. Thus, we can't have "propositions as values."
            // Sure, propositions are expressions, but stuff like returning a proposition?
            // `f(x) = x > .3 & g` is illegal if `g : proposition`
            let (c1, c2) = match (eval_expr(*e1, ctx)?, eval_expr(*e2, ctx)?) {
                (TypedExpr::TEConst(c1, _), TypedExpr::TEConst(c2, _)) => (c1, c2),
                _ => panic!("Did not receive a constant after evaluation."),
            };

            match b {
                Binop::Plus => match (c1, c2) {
                    (Const::Number(n1), Const::Number(n2)) => {
                        let constant = Const::Number(n1 + n2);
                        Ok(TypedExpr::TEConst(
                            constant.clone(),
                            type_of_constant(constant),
                        ))
                    }
                    _ => panic!("Type-checking failed??"),
                },
                Binop::Minus => match (c1, c2) {
                    (Const::Number(n1), Const::Number(n2)) => {
                        let constant = Const::Number(n1 - n2);
                        Ok(TypedExpr::TEConst(
                            constant.clone(),
                            type_of_constant(constant),
                        ))
                    }
                    _ => panic!("Type-checking failed??"),
                },
                Binop::Times => match (c1, c2) {
                    (Const::Number(n1), Const::Number(n2)) => {
                        let constant = Const::Number(n1 * n2);
                        Ok(TypedExpr::TEConst(
                            constant.clone(),
                            type_of_constant(constant),
                        ))
                    }
                    _ => panic!("Type-checking failed??"),
                },
                Binop::Div => match (c1, c2) {
                    (Const::Number(n1), Const::Number(n2)) => {
                        let constant = Const::Number((n1 / n2) as i64);
                        Ok(TypedExpr::TEConst(
                            constant.clone(),
                            type_of_constant(constant),
                        ))
                    }
                    _ => panic!("Type-checking failed??"),
                },
                Binop::Or => match (c1, c2) {
                    (Const::Bool(b1), Const::Bool(b2)) => {
                        let constant = Const::Bool(b1 || b2);
                        Ok(TypedExpr::TEConst(
                            constant.clone(),
                            type_of_constant(constant),
                        ))
                    }
                    _ => panic!("Type-checking failed??"),
                },
                Binop::And => match (c1, c2) {
                    (Const::Bool(b1), Const::Bool(b2)) => {
                        let constant = Const::Bool(b1 && b2);
                        Ok(TypedExpr::TEConst(
                            constant.clone(),
                            type_of_constant(constant),
                        ))
                    }
                    _ => panic!("Type-checking failed??"),
                },
                Binop::Until => match (c1.clone(), c2) {
                    (_, Const::Bool(cond)) => {
                        if cond {
                            Err(errors::UntilVoidError {
                                message: "Until's condition is true by the time of evaluation"
                                    .to_string(),
                            })?
                        } else {
                            let constant = c1;
                            Ok(TypedExpr::TEConst(
                                constant.clone(),
                                type_of_constant(constant),
                            ))
                        }
                    }
                    _ => panic!("Not Implemented"),
                },
                Binop::SUntil => match (c1.clone(), c2) {
                    (_, Const::Bool(cond)) => {
                        if cond {
                            Err(errors::UntilVoidError {
                                message: "Until's condition is true by the time of evaluation"
                                    .to_string(),
                            })?
                        } else {
                            let constant = c1;
                            Ok(TypedExpr::TEConst(
                                constant.clone(),
                                type_of_constant(constant),
                            ))
                        }
                    }
                    _ => panic!("Not Implemented"),
                },
            }
        }
        TypedExpr::TEUnop(u, e1, _) => {
            use ast::Const;
            use ast::Unop;

            let c = match eval_expr(*e1, ctx)? {
                TypedExpr::TEConst(c, _) => c,
                // todo: untils are later
                _ => panic!("Did not receive a constant after evaluation."),
            };

            let constant = match u {
                Unop::Neg => match c {
                    Const::Bool(_) => panic!("Type-checking failed??"),
                    Const::Number(n) => Const::Number(-n),
                },
                Unop::Not => match c {
                    Const::Bool(b) => Const::Bool(!b),
                    Const::Number(_) => panic!("Type-checking failed??"),
                },
            };
            Ok(TypedExpr::TEConst(
                constant.clone(),
                type_of_constant(constant),
            ))
        }
        TypedExpr::TInput(t) => {
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
    }
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
            let c = match eval_expr(texpr, ctx)? {
                TypedExpr::TEConst(c, _) => c,
                _ => panic!("Evaluation did not result in a constant."),
            };
            match c {
                ast::Const::Bool(b) => println!("Print => {}", b),
                ast::Const::Number(n) => println!("Print => {}", n),
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

    for (name, values) in ctx.clone().vars.into_iter() {
        for value in values.into_iter() {
            ctx.destruct_value(name.clone(), value)?;
        }
    }

    Ok(())
}
