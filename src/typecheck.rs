/* todo: block `input` as until_condition. for example:
```
    a <- b <!> input()
```
*/



use std::collections::{HashMap, HashSet};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use crate::ast;
use crate::types;
use crate::errors;
use ast::Var;

use petgraph::graph::Graph;
use petgraph::algo::toposort;
use petgraph::stable_graph::NodeIndex;
// use petgraph::algo::min_spanning_tree;
// use petgraph::algo::is_cyclic_directed;


#[derive(Debug)]
pub struct TypeContext {
    // only global scope
    pub vars: HashMap<Var, Vec<types::Type>>,
    pub clock: i32
}

impl TypeContext {
    // gives type of variable name.
    pub fn look_up(&mut self, var_name: &Var) -> Result<types::Type, errors::CompileTimeError> {
        if let Some(ty) = self.vars.get(var_name) {
            if let Some(t) = types::get_most_immediate_type(ty.to_vec()) {
                Ok(t)
            } else {
                Err(errors::CurrentlyUnavailableError{
                    message : format!(
                        "Name only has {:?}, which isn't immediately available.", ty).to_string()})?
            }
        } else {
            println!("ctx : {:?}", self.vars);
            Err(errors::NameError{message : format!("There is no variable with name {}", var_name).to_string()})?
        }
    }

    // adds new variable
    pub fn add_name(&mut self, var_name: &Var, data: types::Type) -> Result<(), errors::CompileTimeError> {
        if let Some(typs) = self.vars.clone().get(var_name) {
            // static-checking can't decide when/if `until`'s condition releases, thus we might as well forgive it for now. violations will be caught at run-time.
            self.vars.insert(var_name.to_string(), types::resolve_temporal_conflicts(typs.clone(), data, true)?);
        } else {
            self.vars.insert(var_name.to_string(), [data].to_vec());
        }
        Ok(())
    }

    pub fn step_time(&mut self) -> () {
        self.clock += 1;
        self.vars = self.vars.clone().into_iter().map(|(name, typs)| (name, typs.into_iter().filter_map(types::advance_type).collect())).filter(|(_, typs) : &(String, Vec<types::Type>)| !typs.is_empty()).collect();
    }
}

pub fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

pub fn new_texpr(e : ast::TypedExpr, constraint : types::Type, msg : String) -> Result<ast::TypedExpr, errors::ConstrainError> {
    use ast::TypedExpr;
    match e {
        TypedExpr::TEConst(c, old_type) => {
            let t = constrain(&old_type, &constraint, msg)?;
            Ok(TypedExpr::TEConst(c, t))
        },
        TypedExpr::TEVar(v, old_type) => {
            let t = constrain(&old_type, &constraint, msg)?;
            Ok(TypedExpr::TEVar(v, t))
        },
        TypedExpr::TEBinop(b, e1, e2, old_type) => {
            let t = constrain(&old_type, &constraint, msg)?;
            Ok(TypedExpr::TEBinop(b, Box::new(*e1), Box::new(*e2), t))
        },
        TypedExpr::TEUnop(u, e1, old_type) => {
            let t = constrain(&old_type, &constraint, msg)?;
            Ok(TypedExpr::TEUnop(u, Box::new(*e1), t))
        },
        TypedExpr::TInput(old_type) => {
            let t = constrain(&old_type, &constraint, msg)?;
            Ok(TypedExpr::TInput(t))
        }
    }
}

pub fn infer_expr(e : ast::Expr, ctx : &mut TypeContext, until_dependencies : &mut types::UntilDependencies, udep_map : &mut HashMap<u64, ast::TypedExpr>) -> Result<ast::TypedExpr, errors::CompileTimeError> {
    use ast::Expr;
    use ast::TypedExpr;
    use types::TemporalType;
    use types::SimpleType;
    use types::Type;

    let temporal_undefined = TemporalType {
        when_available : types::TemporalAvailability::Undefined,
        when_dissipates : types::TemporalPersistency::Undefined,
        is_until : None
    };

    match e {
        Expr::EBinop(b, e1, e2) => {
            use ast::Binop;
            let operand1 = infer_expr(*e1, ctx, until_dependencies, udep_map)?;
            let operand2 = infer_expr(*e2, ctx, until_dependencies, udep_map)?;

            match b {
                Binop::Plus | Binop::Minus | Binop::Times | Binop::Div => {
                    // temporal type is not (necessarily) encoded in arithmetic expression.
                    //     thus, perhaps `let ty1 = types::Type(Nil, SimpleType::Int)`?
                    let ty1 = Type(temporal_undefined.clone(), SimpleType::Int);
                    let ty2 = Type(temporal_undefined.clone(), SimpleType::Int);
                    let op1 = new_texpr(operand1, ty1, "Left operand of an arithmetic binary operator must be an integer.".to_string())?;
                    let op2 = new_texpr(operand2, ty2, "Right operand of an arithmetic binary operator must be an integer.".to_string())?;

                    // add up until_dependencies

                    let t = Type(temporal_undefined, SimpleType::Int);
                    Ok(TypedExpr::TEBinop(b, Box::new(op1), Box::new(op2), t))
                },
                Binop::Or | Binop::And => {
                    // todo: truthiness & split behaviors for `and` and `or`
                    // todo: negation as failure?
                    let ty1 = Type(temporal_undefined.clone(), SimpleType::Bool);
                    let ty2 = Type(temporal_undefined.clone(), SimpleType::Bool);
                    let op1 = new_texpr(operand1, ty1, "Left operand of a logical operator must be a boolean.".to_string())?;
                    let op2 = new_texpr(operand2, ty2, "Right operand of a logical operator must be a boolean.".to_string())?;

                    let t = Type(temporal_undefined, SimpleType::Bool);
                    Ok(TypedExpr::TEBinop(b, Box::new(op1), Box::new(op2), t))
                },
                Binop::Until => {
                    // todo: truthiness
                    let ty1 = Type(temporal_undefined.clone(), SimpleType::Undefined);
                    let op1 = new_texpr(operand1, ty1, "Left operand of Until should have type 'current'.".to_string())?;
                    let ty2 = Type(temporal_undefined, SimpleType::Bool);
                    let op2 = new_texpr(operand2, ty2, "Right operand of Until should have type 'until bool'. Basically a thunk if it has free variables...".to_string())?;
                    until_dependencies.weak.push(calculate_hash(&op2));
                    udep_map.insert(calculate_hash(&op2), op2.clone());

                    let t = ast::type_of_typedexpr(op1.clone());
                    Ok(TypedExpr::TEBinop(b, Box::new(op1), Box::new(op2), t))
                },
                Binop::SUntil => { // the same as `Until` because `SUntil(A, B) :- Until(A, B) and Finally(B)` can be done when translating to LTL/BA.
                    // todo: truthiness
                    let ty1 = Type(temporal_undefined.clone(), SimpleType::Undefined);
                    let op1 = new_texpr(operand1, ty1, "Left operand of SUntil should have type 'current'.".to_string())?;
                    let ty2 = Type(temporal_undefined, SimpleType::Bool);
                    let op2 = new_texpr(operand2, ty2, "Right operand of SUntil should have type 'until bool'. Basically a thunk if it has free variables...".to_string())?;
                    until_dependencies.strong.push(calculate_hash(&op2));
                    udep_map.insert(calculate_hash(&op2), op2.clone());

                    let t = ast::type_of_typedexpr(op1.clone());
                    Ok(TypedExpr::TEBinop(b, Box::new(op1), Box::new(op2), t))
                }
            }
        },
        Expr::EConst(c) => {
            use ast::Const;
            let t = match c {
                Const::Bool(_) => Type(temporal_undefined, SimpleType::Bool),
                Const::Number(_) => Type(temporal_undefined, SimpleType::Int)
            };
            Ok(TypedExpr::TEConst(c, t))
        },
        Expr::EUnop(u, e1) => {
            use ast::Unop;
            let operand1 = infer_expr(*e1, ctx, until_dependencies, udep_map)?;
            match u {
                Unop::Neg => {
                    let ty1 = Type(temporal_undefined.clone(), SimpleType::Int);
                    let op1 = new_texpr(operand1, ty1, "Neg operator only works on integers.".to_string())?;

                    let t = Type(temporal_undefined, SimpleType::Int);
                    Ok(TypedExpr::TEUnop(u, Box::new(op1), t))
                },
                Unop::Not => {
                    // todo: truthiness
                    let ty1 = Type(temporal_undefined.clone(), SimpleType::Bool);
                    let op1 = new_texpr(operand1, ty1, "Not operator only works on booleans.".to_string())?;

                    let t = Type(temporal_undefined, SimpleType::Bool);
                    Ok(TypedExpr::TEUnop(u, Box::new(op1), t))
                }
            }
        },
        Expr::EVar(s) => {
            let named_type = ctx.look_up(&s)?;
            if types::is_currently_available(&named_type.get_temporal()) {
                Ok(TypedExpr::TEVar(s, Type(temporal_undefined, named_type.get_simpl())))
            } else {
                Err(errors::CurrentlyUnavailableError{message : 
                    "Variable doesn't have `current` type.".to_string()})?
            }            
        },
        Expr::Input => Ok(TypedExpr::TInput(Type(temporal_undefined, types::SimpleType::Int)))
    }
}

// -> errors::TypeError since infer_expr may result in errors::ConstrainError
pub fn infer_command(cmd : ast::Command, ctx : &mut TypeContext, udep_map : &mut HashMap<u64, ast::TypedExpr>) -> Result<ast::TypedCommand, errors::CompileTimeError> {
    // println!("     TIMESTEP {:?}    Command {:?}", ctx.clock, cmd);
    use ast::Command;
    use ast::TypedCommand;
    use types::Type;
    use types::TemporalType;

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

    let mut blank_until_dependencies = types::UntilDependencies {weak : Vec::new(), strong : Vec::new()};
    match cmd {
        Command::Timestep => {
            panic!("Huh, timesteps should've been taken out with `split`... Why was `infer_command` called on one?")
        },
        Command::Global(v, e) => {
            let te = infer_expr(e, ctx, &mut blank_until_dependencies, udep_map)?;
            let t = if blank_until_dependencies.is_empty() {
                Type(TemporalType {
                    when_available : types::TemporalAvailability::Current,
                    when_dissipates : types::TemporalPersistency::Always,
                    is_until : None
                    },
                    ast::type_of_typedexpr(te.clone()).get_simpl()
                )
            } else {
                Type(TemporalType {
                    when_available : types::TemporalAvailability::Current,
                    when_dissipates : types::TemporalPersistency::Always,
                    is_until : Some(blank_until_dependencies)
                    },
                    ast::type_of_typedexpr(te.clone()).get_simpl()
                )
            };
            let te = new_texpr(te, t.clone(), "Idk how Command-lvl type inference failed. This is only used to make until-dependency analysis convenient later down the pipe.".to_string())?;
            ctx.add_name(&v, t)?;
            Ok(TypedCommand::TGlobal(v, te))
        },
        Command::Next(v, e) => {
            let te = infer_expr(e, ctx, &mut blank_until_dependencies, udep_map)?;
            let t = if blank_until_dependencies.is_empty() {
                Type(TemporalType {
                    when_available : types::TemporalAvailability::Next,
                    when_dissipates : types::TemporalPersistency::Fleeting,
                    is_until : None
                    },
                    ast::type_of_typedexpr(te.clone()).get_simpl()
                )
            } else {
                Type(TemporalType {
                    when_available : types::TemporalAvailability::Next,
                    when_dissipates : types::TemporalPersistency::Fleeting,
                    is_until : Some(blank_until_dependencies)
                    },
                    ast::type_of_typedexpr(te.clone()).get_simpl()
                )
            };
            let te = new_texpr(te, t.clone(), "Idk how Command-lvl type inference failed. This is only used to make until-dependency analysis convenient later down the pipe.".to_string())?;
            ctx.add_name(&v, t)?;
            Ok(TypedCommand::TNext(v, te))
        },
        Command::Update(v, e) => {
            let te = infer_expr(e, ctx, &mut blank_until_dependencies, udep_map)?;
            let t = if blank_until_dependencies.is_empty() {
                Type(TemporalType {
                    when_available : types::TemporalAvailability::Next,
                    when_dissipates : types::TemporalPersistency::Fleeting,
                    is_until : Some(blank_until_dependencies)
                    },
                    ast::type_of_typedexpr(te.clone()).get_simpl()
                )
            } else {
                Type(TemporalType {
                    when_available : types::TemporalAvailability::Next,
                    when_dissipates : types::TemporalPersistency::Fleeting,
                    is_until : Some(blank_until_dependencies)
                    },
                    ast::type_of_typedexpr(te.clone()).get_simpl()
                )
            };
            let te = new_texpr(te, t.clone(), "Idk how Command-lvl type inference failed. This is only used to make until-dependency analysis convenient later down the pipe.".to_string())?;
            ctx.add_name(&v, t)?;
            Ok(TypedCommand::TUpdate(v, te))
        },
        Command::Finally(v, e) => {
            let te = infer_expr(e, ctx, &mut blank_until_dependencies, udep_map)?;
            let t = if blank_until_dependencies.is_empty() {
                Type(TemporalType {
                    when_available : types::TemporalAvailability::Future,
                    when_dissipates : types::TemporalPersistency::Always,
                    is_until : None
                    },
                    ast::type_of_typedexpr(te.clone()).get_simpl()
                )
            } else {
                Type(TemporalType {
                    when_available : types::TemporalAvailability::Future,
                    when_dissipates : types::TemporalPersistency::Always,
                    is_until : Some(blank_until_dependencies)
                    },
                    ast::type_of_typedexpr(te.clone()).get_simpl()
                )
            };
            let te = new_texpr(te, t.clone(), "Idk how Command-lvl type inference failed. This is only used to make until-dependency analysis convenient later down the pipe.".to_string())?;
            ctx.add_name(&v, t)?;
            Ok(TypedCommand::TFinally(v, te))
        },
        Command::Print(e) => {
            let te = infer_expr(e, ctx, &mut blank_until_dependencies, udep_map)?;
            if blank_until_dependencies.is_empty() {
                Ok(TypedCommand::TPrint(te))
            } else {
                Err(errors::TemporalConflictError {message : "Why are you trying to print an until expression? Just simplify it...".to_string()})?
            }
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
pub fn dupe_check_cmd(cmd : ast::Command, var_assign : &mut HashMap<ast::Var, (bool, bool, bool)>) -> Result<(), errors::DupeAssignError> {
    use ast::Command;
    match cmd {
        Command::Timestep => {
            panic!("Huh, timesteps should've been taken out with `split`... Why was `dupe_check` called on one?")
        },
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
        }, 
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
        },
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
        },
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
        },
        Command::Print(_) => Ok(())
    }
}

pub fn dupe_check(block : Vec<ast::Command>) -> Result<(), errors::AssignError> {
    // {var_name : (is_global, is_next_or_update, is_future)}
    let mut var_assign = HashMap::new();
    block.iter().try_for_each(|cmd| dupe_check_cmd(cmd.clone(), &mut var_assign))?;
    Ok(())
}

pub fn infer_timeblock(block : Vec<ast::Command>, ctx : &mut TypeContext, udep_map : &mut HashMap<u64, ast::TypedExpr>) -> Result<ast::TypedTimeBlock, errors::CompileTimeError> {
    // println!("old variables: {:?}", ctx.vars);
    let cmds : Result<ast::TypedTimeBlock, _> = block.iter().map(|cmd| infer_command(cmd.clone(), ctx, udep_map)).collect();
    // println!("new variables: {:?}", ctx.vars);
    ctx.step_time();

    cmds
}

pub fn free_vars_of_expr(e : ast::Expr) -> ast::FreeVars {
    use ast::Expr;
    match e {
        Expr::EConst(_) => ast::FreeVars::new(),
        Expr::EVar(v) => {
            let mut free_vars = ast::FreeVars::new();
            free_vars.insert(v);
            free_vars
        },
        Expr::EBinop(_, e1, e2) => {
            free_vars_of_expr(*e1).union(&free_vars_of_expr(*e2)).cloned().collect()
        },
        Expr::EUnop(_, e1) => {
            free_vars_of_expr(*e1)
        },
        Expr::Input => ast::FreeVars::new(),
    }
}

pub fn free_vars_of_command(cmd : &ast::Command) -> ast::FreeVars {
    // todo: set subtraction if propositions are implemented. `f(x) = x * y` ==> `free_vars = {x, y} - {x} = {y}`
    use ast::Command;
    match cmd.clone() {
        Command::Timestep => panic!("Huh, timesteps should've been taken out with `split`... Why was `infer_command` called on one?"),
        Command::Global(_v, e) => free_vars_of_expr(e),
        Command::Next(_v, e) => free_vars_of_expr(e),
        Command::Update(_v, e) => free_vars_of_expr(e),
        Command::Finally(_v, e) => free_vars_of_expr(e),
        Command::Print(e) => free_vars_of_expr(e),
    }
}

// doesn't capture "undefined variables" errors.
pub fn defined_var_of_command(cmd : &ast::Command) -> Option<ast::Var> {
    use ast::Command;
    match cmd.clone() {
        Command::Timestep => panic!("Huh, timesteps should've been taken out with `split`... Why was `infer_command` called on one?"),
        Command::Global(v, _) => Some(v),
        Command::Next(_v, _) => None, // doesn't have an effect in the current time block. thus, doesn't matter in `arrange_by_dependencies`.
        Command::Update(_v, _) => None,
        Command::Finally(v, _) => Some(v),
        Command::Print(_) => None,
    }
}

pub fn arrange_by_dependencies(block : Vec<ast::Command>) -> Result<Vec<ast::Command>, errors::CircularAssignError> {
    // let defined_vars : ast::DefVars = HashSet::from_iter(block.iter().filter_map(defined_var_of_command));

    let a = block.iter().map(|cmd| (defined_var_of_command(cmd), cmd.clone()));
    let defined_vars : ast::DefVars = HashSet::from_iter(a.clone().filter(|(opt, _)| opt.is_some()).map(|(opt, _)| opt.unwrap()));
    let delayed_commands : Vec<ast::Command> = a.filter(|(opt, _)| opt.is_none()).map(|(_, cmd)| cmd).collect();

    let free_vars_by_cmd : HashMap<Var, ast::FreeVars> = HashMap::from_iter(
        block.iter().map(
            // `cmd.free_vars & defined_vars` because we don't care about timeblock-wide free variables. we care only about command's free variables.
            // also, `graph.add_edge` will give an error if it tries to connect a nonexistant node with an existing/nonexisting node.
            |cmd| (defined_var_of_command(cmd), free_vars_of_command(cmd).intersection(&defined_vars).map(|var| var.clone()).collect())
        ).filter(|(def_var, _)| def_var.is_some()).map(|(def_var, free_vars)| (def_var.unwrap(), free_vars)) // if `defined_vars.contains(tup.0)`
    );

    // println!("free_vars_by_cmd = {:?}", free_vars_by_cmd);

    // scan for recursive definitions. since `defined_vars_of_command` excludes next/update, all recursive definitions are blocked.
    
    let mut graph = Graph::<Var, i32>::new();

    let var_map : HashMap<Var, NodeIndex> = defined_vars.into_iter().map(|def_var| (def_var.clone(), graph.add_node(def_var))).collect();
    // for var in defined_vars.into_iter() {
    //     let origin = graph.add_node(var);
    // }
    for (origin, destinations) in free_vars_by_cmd.iter() {
        for dest in destinations {
            graph.add_edge(*var_map.get(dest).unwrap(), *var_map.get(origin).unwrap(), 1);
        }
    }

    match toposort(&graph, None) {
        Ok(order) => {
            let top_order : Vec<Var> = order.iter().map(|node| graph.node_weight(node.clone()).unwrap().clone()).collect();

            let mut cmd_by_var : HashMap<Var, Vec<ast::Command>> = HashMap::new();
            for cmd in block.into_iter() {
                if let Some(def_var) = defined_var_of_command(&cmd) {
                    cmd_by_var.entry(def_var).or_default().push(cmd);
                }
                
            }

            let mut cmd_order : Vec<ast::Command> = Vec::new();
            for var in top_order.iter() {
                cmd_order.append(&mut cmd_by_var.get(var).unwrap().clone());
            }
            cmd_order.append(&mut delayed_commands.clone());
            Ok(cmd_order)
        },
        Err(err) => {
            Err(
                errors::CircularAssignError {
                    message : format!("Error: Graph has cycle with node: {}", graph.node_weight(err.node_id()).unwrap().as_str()).to_string()
                }
            )
        }
    }
}

pub fn infer_program(program : ast::Program) -> Result<ast::TypedProgram, errors::CompileTimeError> {
    let mut ctx = TypeContext {
        vars : HashMap::new(),
        clock : 0
    };

    let mut udep_map : HashMap<u64, ast::TypedExpr> = HashMap::new();

    // don't filter out empty blocks since `---\n---` is valid.
    let time_blocks : Vec<_> = program.split(|cmd| *cmd == ast::Command::Timestep).map(|tb| tb.to_vec()).collect();

    time_blocks.clone().iter().try_for_each(|tb| dupe_check(tb.clone()))?;

    let arranged_program : Vec<Result<Vec<ast::Command>, errors::CircularAssignError>> = time_blocks.into_iter().map(|tb| arrange_by_dependencies(tb)).collect();
    let arranged_program : Result<Vec<Vec<ast::Command>>, errors::CircularAssignError> = arranged_program.into_iter().collect();
    let arranged_program = arranged_program?;

    // println!("Arranged : {:?}", arranged_program);
    let inferred_program : Vec<Vec<ast::TypedCommand>> = arranged_program.iter().map(|block| infer_timeblock(block.to_vec(), &mut ctx, &mut udep_map)).collect::<Result<_, _>>()?;
    Ok(ast::TypedProgram {
        code: inferred_program,
        udep_map: udep_map,
    })
}

// `con_ty` should be `crate::types::Type(Current, SimpleType)`
pub fn constrain(te : &types::Type, con_ty : &types::Type, msg : String) -> Result<types::Type, errors::ConstrainError> {
    use types::TemporalType;
    use types::SimpleType;
    use types::Type;

    let Type(temp1, simp1) = te;
    let Type(temp2, simp2) = con_ty;

    let simp = match (simp1, simp2) {
        (SimpleType::Undefined, b) => b,
        (a, SimpleType::Undefined) => a,
        (a, b) if a == b => a,
        _ => Err(errors::ConstrainError {message : msg.clone()})?
    };

    let temp = match (temp1, temp2) {
        (TemporalType {
            when_available : types::TemporalAvailability::Undefined,
            when_dissipates : types::TemporalPersistency::Undefined,
            is_until : None
        }, b) => b,
        (a, TemporalType {
            when_available : types::TemporalAvailability::Undefined,
            when_dissipates : types::TemporalPersistency::Undefined,
            is_until : None
        }) => a,
        // I doubt I'll need to add cases like `(Global, Current)`, but perhaps I'll need to
        (a, b) if a == b => a,
        _ => Err(errors::ConstrainError {message : msg})?
    };

    Ok(Type(temp.clone(), simp.clone()))
}