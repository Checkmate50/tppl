use std::collections::HashMap;
use std::collections::HashSet;

use crate::ast;
use crate::types;
use crate::typecheck;
use ast::TypedExpr;
use ast::TypedCommand;
use types::Type;
use types::SimpleType;
use types::TemporalType;
use ast::Var;

#[derive(Debug)]
pub struct NameError {
    message : String
}
#[derive(Debug)]
pub struct InputError {
    message : String
}
// `a <!> true`
#[derive(Debug)]
pub struct UntilVoidError {
    message : String
}
// `a <!> (some_val_that_ain't_ever_true)`
#[derive(Debug)]
pub struct SUntilConditionUnsatisfied {
    message : String
}
// example: trying to access a next
#[derive(Debug)]
pub struct CurrentlyUnavailableError {
    message : String
}
#[derive(Debug)]
pub enum ExecutionTimeError {
    // Should I put `Name`, `Input`, `Until`, `SUntil`, and `Immediate` in some new `AccessError`?
    Type(typecheck::TypeError),
    Assign(typecheck::AssignError),
    Name(NameError),
    Input(InputError),
    Until(UntilVoidError),
    SUntil(SUntilConditionUnsatisfied),
    Immediacy(CurrentlyUnavailableError)
}
impl From<typecheck::TypeError> for ExecutionTimeError {
    fn from(e: typecheck::TypeError) -> Self {
        ExecutionTimeError::Type(e)
    }
}
impl From<typecheck::AssignError> for ExecutionTimeError {
    fn from(e: typecheck::AssignError) -> Self {
        ExecutionTimeError::Assign(e)
    }
}
impl From<NameError> for ExecutionTimeError {
    fn from(e: NameError) -> Self {
        ExecutionTimeError::Name(e)
    }
}
impl From<InputError> for ExecutionTimeError {
    fn from(e: InputError) -> Self {
        ExecutionTimeError::Input(e)
    }
}
impl From<UntilVoidError> for ExecutionTimeError {
    fn from(e: UntilVoidError) -> Self {
        ExecutionTimeError::Until(e)
    }
}
impl From<SUntilConditionUnsatisfied> for ExecutionTimeError {
    fn from(e: SUntilConditionUnsatisfied) -> Self {
        ExecutionTimeError::SUntil(e)
    }
}
impl From<CurrentlyUnavailableError> for ExecutionTimeError {
    fn from(e: CurrentlyUnavailableError) -> Self {
        ExecutionTimeError::Immediacy(e)
    }
}

// impl fmt::Display for ExecutionTimeError {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         let err_msg = match self.code {
//             404 => "Sorry, Can not find the Page!",
//             _ => "Sorry, something is wrong! Please Try Again!",
//         };
// 
//         write!(f, "{}", err_msg)
//     }
// }


#[derive(Clone)]
#[derive(Debug)]
pub struct UntilLog {
    pub strong : Vec<TypedExpr>,
    pub weak : Vec<TypedExpr>
}

#[derive(Clone)]
#[derive(Debug)]
pub enum Value {
    Expression(types::Type, TypedExpr, UntilLog),
    Union(Box<Value>, Box<Value>)  // perhaps instead use `{current/global : val, next/update : val, finally : val}`?
}

// should this be in `impl Value {}`?
pub fn advance_time(val : Value) -> Option<Value> {
    match val {
        Value::Expression(t, v, u) => {
            let aged_type = *types::advance_type(Box::new(t))?;
            Some(Value::Expression(aged_type, v, u))
        },
        Value::Union(a, b) => {
            let aged_a = advance_time(*a);
            let aged_b = advance_time(*b);

            if let Some(ref a) = aged_a {
                if let Some(ref b) = aged_b {
                    Some(Value::Union(Box::new(a.clone()), Box::new(b.clone())))
                } else {
                    aged_a
                }
            } else {
                aged_b
            }
        }
    }
}


#[derive(Clone)]
#[derive(Debug)]
pub struct VarContext {
    pub vars: HashMap<Var, Value>,
    /*
    Of the form (independent, dependencies).
    For example, ```
        x = 3 <!!> y & c
        a = x <!> y
    ``` would be `{y: {x, a}, c : {x}}`
    */
    // one possible optimization: use *value* rather than the variable (which can be associated with multiple values)
    pub until_dependencies : HashMap<Var, HashSet<Var>>,
    pub clock: i32
}

impl VarContext {
    pub fn look_up(&mut self, var_name: &Var) -> Result<Value, NameError> {
        if let Some(val) = self.vars.get(var_name) {
            Ok(get_most_immediate_val(val).clone())
        } else {
            Err(NameError{message : format!("There is no variable with name {}", var_name).to_string()})   
        }
    }

    pub fn add_var(&mut self, var_name: &Var, data: &TypedExpr, typ: &Type, until_tracker : &UntilLog) -> () {
        let v = Value::Expression(typ.clone(), data.clone(), until_tracker.clone());
        if let Some(vals) = self.vars.get(var_name) {
            self.vars.insert(var_name.to_string(), Value::Union(Box::new(v), Box::new(vals.clone())));
        } else {
            self.vars.insert(var_name.to_string(), v);
        }
    }

    pub fn step_time(&mut self) -> () {
        self.clock += 1;
        self.vars = self.vars.iter().map(|(name, val)| (name.clone(), advance_time(val.clone()))).filter(|(_, val)| val.is_some()).map(|(name, val)| (name, val.unwrap())).collect();
    }

    pub fn add_until_dependencies(&mut self, target : &Var, until_tracker : &UntilLog) -> () {
        let weak = HashSet::<_>::from_iter(until_tracker.clone().weak.into_iter().map(free_vars_of_texpr).flatten());
        let strong = HashSet::<_>::from_iter(until_tracker.clone().strong.into_iter().map(free_vars_of_texpr).flatten());

        // add to until_dependencies
        for independent in weak.union(&strong) {
            // let dependents : HashMap<_> = self.until_dependencies.entry(independent.to_string()).or_insert(HashSet::new()).union(&HashSet::from_iter([target.clone()].iter())).collect();
            // self.until_dependencies.insert(independent.to_string(), dependents);

            let dependents = self.until_dependencies.entry(independent.to_string()).or_insert(HashSet::new());
            dependents.insert(target.clone());
        }
    }

    // checks and deletes values accordingly
    pub fn check_until_dependencies(&mut self, var_name : &Var) -> Result<(), ExecutionTimeError> {
        if let Some(dependents) = self.clone().until_dependencies.get(var_name) {
            for dependent in dependents {
                if let Some(dependent_val) = self.clone().vars.get(dependent) {
                    if let Some(val) = filter_value(dependent_val.clone(), self)? {
                        self.vars.insert(var_name.to_string(), val);
                    }
                }
            }
        }
        Ok(())
    }
}

pub fn filter_value(v : Value, ctx : &mut VarContext) -> Result<Option<Value>, ExecutionTimeError> {
    match v {
        Value::Expression(_, _, ref until_dependencies) => {
            let mut should_release = false;
            for u_dependency in until_dependencies.weak.iter() {
                match eval_expr(u_dependency.clone(), ctx, &mut None) {
                    Ok(cond) => match cond {
                        TypedExpr::TEConst(ast::Const::Bool(b), _) => {
                            if b {
                                should_release = true;
                            }
                        },
                        _ => panic!("Type checking failed? Until_e2 did not evaluate to a boolean...")
                    },
                    Err(ExecutionTimeError::Name(_)) => {}, // all fine and dandy
                    Err(e) => Err(e)?  // NameError is forgiveable, but any others are not.
                };
            }

            let mut false_suntil_conds : Option<TypedExpr> = None;
            let mut suntil_nameerror : Option<NameError> = None;
            for u_dependency in until_dependencies.strong.iter() {
                let res = eval_expr(u_dependency.clone(), ctx, &mut None);
                match res {
                    Ok(cond) => match cond {
                        TypedExpr::TEConst(ast::Const::Bool(b), _) => {
                            if b {
                                should_release = true;
                            } else {
                                false_suntil_conds = Some(u_dependency.clone());
                            }
                        },
                        _ => panic!("Type checking failed? Until_e2 did not evaluate to a boolean...")
                    },
                    Err(ExecutionTimeError::Name(NameError{message})) => {
                        suntil_nameerror = Some(NameError{message});
                    },
                    Err(e) => Err(e)?  // NameError is forgiveable, but any others are not.
                };
            }

            if should_release {
                match suntil_nameerror {
                    // `nameerror` means the condition is not true, which is bad for suntils.
                    Some(NameError{message}) => Err(SUntilConditionUnsatisfied {message : format!("If a strong until condition is not evaluatable, then it's not true. Evaluation was stopped because of: \n{:?}", message).to_string()})?,
                    None => {},
                };
                
                if let Some(false_cond) = false_suntil_conds {
                    Err(SUntilConditionUnsatisfied {message : format!("Condition {:?} was not true by the time of release.", false_cond).to_string()})?
                } else {
                    Ok(None)
                }
            } else {
                Ok(Some(v))
            }
        },
        Value::Union(v1, v2) => {
            if let Some(v1) = filter_value(*v1, ctx)? {
                if let Some(v2) = filter_value(*v2, ctx)? {
                    Ok(Some(Value::Union(Box::new(v1), Box::new(v2))))
                } else {
                    Ok(Some(v1))
                }
            } else {
                if let Some(v2) = filter_value(*v2, ctx)? {
                    Ok(Some(v2))
                } else {
                    Ok(None)
                }
            }
        }
    }
}

pub fn get_most_immediate_val(v: &Value) -> &Value {
    match v {
        Value::Expression(_, _, _) => v,
        Value::Union(v1, v2) => {
            let immediate_v1 = get_most_immediate_val(v1);
            let immediate_v2 = get_most_immediate_val(v2);
            
            let (ty1, ty2) = match (immediate_v1.clone(), immediate_v2.clone()) {
                (Value::Expression(ty1, _, _), Value::Expression(ty2, _, _)) => (ty1, ty2),
                _ => panic!("get_most_immediate_val should only return Value::Expression, but it didn't..?")
            };
            match (ty1, ty2) {
                (Type::Prod(TemporalType::Current, _), Type::Prod(_, _)) => immediate_v1,
                (Type::Prod(_, _), Type::Prod(TemporalType::Current, _)) => immediate_v2,
                (Type::Prod(TemporalType::Global, _), Type::Prod(_, _)) => immediate_v1,
                (Type::Prod(_, _), Type::Prod(TemporalType::Global, _)) => immediate_v2,
                (Type::Prod(TemporalType::Next, _), Type::Prod(_, _)) => immediate_v2,
                (Type::Prod(_, _), Type::Prod(TemporalType::Next, _)) => immediate_v1,
                _ => panic!("There is no way to decide which is more pressing between {:?} and {:?}.", immediate_v1, immediate_v2)
            }
        }
    }
}

pub fn free_vars_of_texpr(e : TypedExpr) -> ast::FreeVars {
    match e {
        TypedExpr::TEConst(_, _) => ast::FreeVars::new(),
        TypedExpr::TEVar(v, _) => {
            let mut free_vars = ast::FreeVars::new();
            free_vars.insert(v);
            free_vars
        },
        TypedExpr::TEBinop(_, e1, e2, _) => {
            free_vars_of_texpr(*e1).union(&free_vars_of_texpr(*e2)).cloned().collect()
        },
        TypedExpr::TEUnop(_, e1, _) => {
            free_vars_of_texpr(*e1)
        },
        TypedExpr::TInput(_) => ast::FreeVars::new(),
    }
}

pub fn type_of_constant(c : ast::Const) -> types::Type {
    match c {
        ast::Const::Number(_) => types::Type::Prod(types::TemporalType::Current, types::SimpleType::Int),
        ast::Const::Bool(_) => types::Type::Prod(types::TemporalType::Current, types::SimpleType::Bool)
    }
}

pub fn eval_expr(e : TypedExpr, ctx : &mut VarContext, until_tracker_opt : &mut Option<&mut UntilLog>) -> Result<TypedExpr, ExecutionTimeError> {
    match e {
        TypedExpr::TEConst(_, _) => Ok(e),
        TypedExpr::TEVar(var_name, _) => {
            match ctx.look_up(&var_name)? {
                Value::Expression(_, texpr, _) => {
                    let mut ty = typecheck::type_of_typedexpr(&texpr);
                    if types::is_currently_available(&ty.get_temporal()) {
                        Ok(texpr)
                    } else {
                        Err(CurrentlyUnavailableError{message : format!("You tried access a value that's only available in the next timestep... Temporal Type was {:?}", ty).to_string()})?
                    }
                },
                _ => panic!("VarContext.look_up should only return `Value::Expression`, but it didn't? `get_most_immediate_val` may have failed. May be something else.")
            }
        },
        TypedExpr::TEBinop(b, e1, e2, _) => {
            use ast::Const;
            use ast::Binop;

            let e1 = eval_expr(*e1, ctx, until_tracker_opt)?;
            let e2 = eval_expr(*e2, ctx, until_tracker_opt)?;
            let (c1, c2) = match (e1, e2.clone()) {
                (TypedExpr::TEConst(c1, _), TypedExpr::TEConst(c2, _)) => (c1, c2),
                _ => panic!("Did not receive a constant after evaluation.")
            };
            
            match b {
                Binop::Plus => match (c1, c2) {
                    (Const::Number(n1), Const::Number(n2)) => {
                        let constant = Const::Number(n1 + n2);
                        Ok(TypedExpr::TEConst(constant.clone(), type_of_constant(constant)))
                    },
                    _ => panic!("Type-checking failed??")
                },
                Binop::Minus => match (c1, c2) {
                    (Const::Number(n1), Const::Number(n2)) => {
                        let constant = Const::Number(n1 - n2);
                        Ok(TypedExpr::TEConst(constant.clone(), type_of_constant(constant)))
                    },
                    _ => panic!("Type-checking failed??")
                },
                Binop::Times => match (c1, c2) {
                    (Const::Number(n1), Const::Number(n2)) => {
                        let constant = Const::Number(n1 * n2);
                        Ok(TypedExpr::TEConst(constant.clone(), type_of_constant(constant)))
                    },
                    _ => panic!("Type-checking failed??")
                },
                Binop::Div => match (c1, c2) {
                    (Const::Number(n1), Const::Number(n2)) => {
                        let constant = Const::Number((n1 / n2) as i64);
                        Ok(TypedExpr::TEConst(constant.clone(), type_of_constant(constant)))
                    },
                    _ => panic!("Type-checking failed??")
                },
                Binop::Or => match (c1, c2) {
                    (Const::Bool(b1), Const::Bool(b2)) => {
                        let constant = Const::Bool(b1 || b2);
                        Ok(TypedExpr::TEConst(constant.clone(), type_of_constant(constant)))
                    },
                    _ => panic!("Type-checking failed??")
                },
                Binop::And => match (c1, c2) {
                    (Const::Bool(b1), Const::Bool(b2)) => {
                        let constant = Const::Bool(b1 && b2);
                        Ok(TypedExpr::TEConst(constant.clone(), type_of_constant(constant)))
                    },
                    _ => panic!("Type-checking failed??")
                },
                Binop::Until => match (c1.clone(), c2) {
                    (_, Const::Bool(cond)) => {
                        if cond {
                            Err(UntilVoidError{message : "Until's condition is true by the time of evaluation".to_string()})?
                        } else {
                            if let Some(&mut ref mut until_tracker) = until_tracker_opt {
                                until_tracker.weak.push(e2);
                            }
                            let constant = c1;
                            Ok(TypedExpr::TEConst(constant.clone(), type_of_constant(constant)))
                        }
                    }
                    _ => panic!("Not Implemented")
                },
                Binop::SUntil => match (c1.clone(), c2) {
                    (_, Const::Bool(cond)) => {
                        if cond {
                            Err(UntilVoidError{message : "Until's condition is true by the time of evaluation".to_string()})?
                        } else {
                            if let Some(until_tracker) = until_tracker_opt {
                                until_tracker.strong.push(e2);
                            }
                            let constant = c1;
                            Ok(TypedExpr::TEConst(constant.clone(), type_of_constant(constant)))
                        }
                    }
                    _ => panic!("Not Implemented")
                },
            }
        },
        TypedExpr::TEUnop(u, e1, _) => {
            use ast::Const;
            use ast::Unop;

            let e1 = eval_expr(*e1, ctx, until_tracker_opt)?;
            let c = match e1 {
                TypedExpr::TEConst(c, _) => c,
                // todo: untils are later
                _ => panic!("Did not receive a constant after evaluation.")
            };
            
            let constant = match u {
                Unop::Neg => {
                    match c {
                        Const::Bool(_) => panic!("Type-checking failed??"),
                        Const::Number(n) => Const::Number(-n)
                    }
                },
                Unop::Not => {
                    match c {
                        Const::Bool(b) => Const::Bool(!b),
                        Const::Number(_) => panic!("Type-checking failed??")
                    }
                }
            };
            Ok(TypedExpr::TEConst(constant.clone(), type_of_constant(constant)))
        },
        TypedExpr::TInput(t) => {
            let mut buffer = String::new();
            std::io::stdin().read_line(&mut buffer).expect("Failed to read line.");
            if let Ok(n) = buffer.trim().parse() {
                Ok(TypedExpr::TEConst(ast::Const::Number(n), t))
            } else {
                Err(InputError{message : "Input is not an integer!".to_string()})?
            }
        },
    }
}

// `false` if worked. `true` if error
pub fn exec_command(c : TypedCommand, ctx : &mut VarContext) -> Result<(), ExecutionTimeError> {
    let mut until_tracker = UntilLog {strong : Vec::new(), weak : Vec::new()};
    match c {
        TypedCommand::TGlobal(v, texpr) => {
            let val = eval_expr(texpr, ctx, &mut Some(&mut until_tracker))?;
            ctx.add_var(&v, &val, &Type::Prod(TemporalType::Global, SimpleType::Undefined), &until_tracker);
            ctx.add_until_dependencies(&v, &until_tracker);
            ctx.check_until_dependencies(&v)?;
            Ok(())
        },
        TypedCommand::TNext(v, texpr) => {
            let val = eval_expr(texpr, ctx, &mut Some(&mut until_tracker))?;
            ctx.add_var(&v, &val, &Type::Prod(TemporalType::Next, SimpleType::Undefined), &until_tracker);
            ctx.add_until_dependencies(&v, &until_tracker);
            ctx.check_until_dependencies(&v)?;
            Ok(())
        },
        TypedCommand::TUpdate(v, texpr) => {
            let val = eval_expr(texpr, ctx, &mut Some(&mut until_tracker))?;
            ctx.add_var(&v, &val, &Type::TempNest(TemporalType::Next, Box::new(Type::Prod(TemporalType::Future, SimpleType::Undefined))), &until_tracker);
            ctx.add_until_dependencies(&v, &until_tracker);
            ctx.check_until_dependencies(&v)?;
            Ok(())
        },
        TypedCommand::TFinally(v, texpr) => {
            let val = eval_expr(texpr, ctx, &mut Some(&mut until_tracker))?;
            ctx.add_var(&v, &val, &Type::TempNest(TemporalType::Global, Box::new(Type::Prod(TemporalType::Future, SimpleType::Undefined))), &until_tracker);
            ctx.add_until_dependencies(&v, &until_tracker);
            ctx.check_until_dependencies(&v)?;
            Ok(())
        },
        TypedCommand::TPrint(texpr) => {
            // todo: handle pdfs and propositions when implemented
            let c = match eval_expr(texpr, ctx, &mut None)? {
                TypedExpr::TEConst(c, _) => c,
                _ => panic!("Evaluation did not result in a constant.")
            };
            match c {
                ast::Const::Bool(b) => println!("{}", b),
                ast::Const::Number(n) => println!("{}", n)
            };
            Ok(())
        },
    }
}

pub fn exec_time_block(time_block : ast::TypedTimeBlock, ctx : &mut VarContext) -> Result<(), ExecutionTimeError> {
    time_block.iter().try_for_each(|cmd| exec_command(cmd.clone(), ctx))
}

pub fn exec_program(pgrm : ast::TypedProgram) -> Result<(), ExecutionTimeError> {
    let mut ctx = VarContext {
        vars : HashMap::new(),
        until_dependencies : HashMap::new(),
        clock : 0
    };

    pgrm.iter().try_for_each(|tb| exec_time_block(tb.clone(), &mut ctx))
}