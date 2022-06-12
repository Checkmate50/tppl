// use std::fmt;
use std::collections::HashMap;
use std::collections::HashSet;

use crate::ast;
use crate::types;
use ast::Var;

use petgraph::graph::Graph;
use petgraph::algo::toposort;
use petgraph::stable_graph::NodeIndex;
// use petgraph::algo::min_spanning_tree;
// use petgraph::algo::is_cyclic_directed;


// pub(crate) type InferResult<T> = Result<T, ()>;


// #[derive(Debug, Clone)]
// pub struct ConstrainError;
// impl fmt::Display for ConstrainError {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         write!(f, "couldn't constrain types")
//     }
// }

#[derive(Debug)]
pub struct InferError {
    message: String,
}
#[derive(Debug)]
pub struct ConstrainError {
    message: String,
}
#[derive(Debug)]
pub struct CurrentlyUnavailableError {
    message : String,
}
#[derive(Debug)]
pub struct DupeAssignError {
    message: String,
}
#[derive(Debug)]
pub struct RecursiveAssignError {
    message: String,
}
#[derive(Debug)]
pub struct CircularAssignError {
    message: String,
}
#[derive(Debug)]
pub enum TypeError {
    Infer(InferError),
    Constrain(ConstrainError),
    Immediacy(CurrentlyUnavailableError)
}
#[derive(Debug)]
pub enum AssignError {
    Dupe(DupeAssignError),
    Recursive(RecursiveAssignError),
    Circular(CircularAssignError)
}
#[derive(Debug)]
pub struct NameError {
    message: String,
}
#[derive(Debug)]
pub enum CompileTimeError {
    Type(TypeError),
    Assign(AssignError),
    Access(NameError)
}
impl From<InferError> for TypeError {
    fn from(e: InferError) -> Self {
        TypeError::Infer(e)
    }
}
impl From<ConstrainError> for TypeError {
    fn from(e: ConstrainError) -> Self {
        TypeError::Constrain(e)
    }
}
impl From<CurrentlyUnavailableError> for TypeError {
    fn from(e: CurrentlyUnavailableError) -> Self {
        TypeError::Immediacy(e)
    }
}
impl From<DupeAssignError> for AssignError {
    fn from(e: DupeAssignError) -> Self {
        AssignError::Dupe(e)
    }
}
impl From<RecursiveAssignError> for AssignError {
    fn from(e: RecursiveAssignError) -> Self {
        AssignError::Recursive(e)
    }
}
impl From<CircularAssignError> for AssignError {
    fn from(e: CircularAssignError) -> Self {
        AssignError::Circular(e)
    }
}
impl From<TypeError> for CompileTimeError {
    fn from(e: TypeError) -> Self {
        CompileTimeError::Type(e)
    }
}
impl From<AssignError> for CompileTimeError {
    fn from(e: AssignError) -> Self {
        CompileTimeError::Assign(e)
    }
}
impl From<NameError> for CompileTimeError {
    fn from(e: NameError) -> Self {
        CompileTimeError::Access(e)
    }
}


impl From<CircularAssignError> for CompileTimeError {
    fn from(e: CircularAssignError) -> Self {
        CompileTimeError::Assign(AssignError::Circular(e))
    }
}
impl From<ConstrainError> for CompileTimeError {
    fn from(e: ConstrainError) -> Self {
        CompileTimeError::Type(TypeError::Constrain(e))
    }
}
impl From<CurrentlyUnavailableError> for CompileTimeError {
    fn from(e: CurrentlyUnavailableError) -> Self {
        CompileTimeError::Type(TypeError::Immediacy(e))
    }
}

#[derive(Debug)]
pub struct TypeContext {
    // only global scope
    pub vars: HashMap<Var, types::Type>,
    pub clock: i32
}

impl TypeContext {
    // gives type of variable name.
    pub fn look_up(&mut self, var_name: &Var) -> Result<types::Type, NameError> {
        if let Some(val) = self.vars.get(var_name) {
            Ok(types::get_most_immediate_type(val).clone())
        } else {
            Err(NameError{message : format!("There is no variable with name {}", var_name).to_string()})   
        }
    }

    // adds new variable
    pub fn add_name(&mut self, var_name: &Var, data: types::Type) -> Result<(), TypeError> {
        if let Some(typs) = self.vars.get(var_name) {
            let t = resolve_temporal_conflicts(&typs, &data)?;
            self.vars.insert(var_name.to_string(), types::Type::Union(Box::new(data), Box::new(typs.clone())));
        } else {
            self.vars.insert(var_name.to_string(), data);
        }
    }

    pub fn step_time(&mut self) -> () {
        self.clock += 1;
        self.vars = self.vars.clone().into_iter().map(|(name, ty)| (name, types::advance_type(Box::new(ty.clone())))).filter(|(_, ty)| ty.is_some()).map(|(name, ty)| (name, *ty.unwrap())).collect();
    }
}

pub fn resolve_temporal_conflicts(og_ty : &Type, new_ty : &Type) -> Result<Type, ConstrainError> {
    use types::TemporalType;
    use types::Type;
    match (og_ty, new_ty.clone()) {
        (Prod(temp1, simpl1), Prod(temp2, simpl2)) => {
            match (temp1, temp2) {
                (TemporalType::Global, TemporalType::Global) => Err(ConstrainError {message : "Global and Global conflict".to_string()}),
                (TemporalType::Global, TemporalType::Next) => Err(ConstrainError {message : "Global and Next conflict".to_string()}),
                (TemporalType::Next, TemporalType::Global) => Err(ConstrainError {message : "Next and Global conflict".to_string()}),
                (TemporalType::Next, TemporalType::Next) => Err(ConstrainError {message : "Next and Next conflict".to_string()}),
                (TemporalType::Current, TemporalType::Global) => Err(ConstrainError {message : "Current and Global conflict".to_string()}),
                (TemporalType::Current, TemporalType::Current) => Err(ConstrainError {message : "Current and Current conflict".to_string()}),
                (TemporalType::Global, TemporalType::Current) => Err(ConstrainError {message : "Global and Current conflict".to_string()}),

                (TemporalType::Next, TemporalType::Current) => Ok(Type::Union(Box::new(new_ty.clone()), Box::new(og_ty.clone()))),
                (TemporalType::Current, TemporalType::Next) => Ok(Type::Union(Box::new(new_ty.clone()), Box::new(og_ty.clone()))),
                (TemporalType::Until, TemporalType::Next) => Ok(Type::Union(Box::new(new_ty.clone()), Box::new(og_ty.clone()))),

                (TemporalType::Until, TemporalType::Global) => Ok(new_ty.clone()),
                (TemporalType::Until, TemporalType::Current) => Ok(new_ty.clone()),

                // these cases shouldn't happen, but just in case `Next(Until)` isn't the only way `Until` occurs.
                (TemporalType::Global, TemporalType::Until) => Err(ConstrainError {message : "Global and Until conflict".to_string()}),
                (TemporalType::Next, TemporalType::Until) => Ok(Type::Union(Box::new(new_ty.clone()), Box::new(og_ty.clone()))),
                (TemporalType::Until, TemporalType::Until) => Ok(new_ty.clone())
                (TemporalType::Current, TemporalType::Until) => Err(ConstrainError {message : "Current and Until conflict".to_string()}),
            }
        },
        (Prod(temp1, simpl1), TempNest(temp2outie, (temp2innie, _simpl))) => {
            match (temp1, (temp2outie, temp2innie)) {
                (TemporalType::Global, (TemporalType::Next, TemporalType::Until)) => Err(ConstrainError {message : "Global and Next(Until) conflict".to_string()}),
                (TemporalType::Global, (TemporalType::Global, TemporalType::Future)) => Err(ConstrainError {message : "Global and Global(Future) conflict".to_string()}),
                (TemporalType::Next, (TemporalType::Next, TemporalType::Until)) => Err(ConstrainError {message : "Next and Next(Until) conflict".to_string()}),
                
                (TemporalType::Next, (TemporalType::Global, TemporalType::Future)) => Ok(Type::Union(Box::new(new_ty.clone()), Box::new(og_ty.clone()))),
                (TemporalType::Until, (TemporalType::Next, TemporalType::Until)) => Ok(Type::Union(Box::new(new_ty.clone()), Box::new(og_ty.clone()))),
                (TemporalType::Until, (TemporalType::Global, TemporalType::Future)) => Ok(Type::Union(Box::new(new_ty.clone()), Box::new(og_ty.clone()))),
                (TemporalType::Current, (TemporalType::Next, TemporalType::Until)) => Ok(Type::Union(Box::new(new_ty.clone()), Box::new(og_ty.clone()))),
                (TemporalType::Current, (TemporalType::Global, TemporalType::Future)) => Ok(Type::Union(Box::new(new_ty.clone()), Box::new(og_ty.clone())))
            }
        },
        (TempNest(temp1outie, (temp1innie, _simpl)), Prod(temp2, simpl2)) => {
            // Global, Next, Until, Current, Next(Until), Global(Future), Global(Until), Next(Until)
            match ((temp1outie, temp1innie), temp2) {
                ((TemporalType::Next, TemporalType::Until), TemporalType::Global) => Err(ConstrainError {message : "Next(Until) and Global conflict".to_string()}),
                ((TemporalType::Global, TemporalType::Future), TemporalType::Global) => 
                ((TemporalType::Next, TemporalType::Until), TemporalType::Next) => 
                ((TemporalType::Global, TemporalType::Future), TemporalType::Next) => 
                ((TemporalType::Next, TemporalType::Until), TemporalType::Until) => 
                ((TemporalType::Global, TemporalType::Future), TemporalType::Until) => 
                ((TemporalType::Next, TemporalType::Until), TemporalType::Current) => 
                ((TemporalType::Global, TemporalType::Future), TemporalType::Current) => 
            }
        },
        (TempNest(temp1, type1), TempNest(temp2, type2)) => {
            (Next(Until), Global(Future))
            (Global(Future), Next(Until))
            (Global(Future), Global(Future))
            (Next(Until), Next(Until))
        },
        (Union(t1a, t1b), _) => {
            check_temporal_conflicts(t1, new_ty)?;
            check_temporal_conflicts(t2, new_ty)?;
        },
        (_, Union(t1a, t1b)) => {
            panic!("This function is meant to accept only non-union `new_ty` arguments")
        }
    }
}

pub fn new_texpr(e : ast::TypedExpr, constraint : &types::Type, msg : String) -> Result<ast::TypedExpr, ConstrainError> {
    use ast::TypedExpr;
    match e {
        TypedExpr::TEConst(c, old_type) => {
            let t = constrain(old_type, constraint, msg)?;
            Ok(TypedExpr::TEConst(c, t))
        },
        TypedExpr::TEVar(v, old_type) => {
            let t = constrain(old_type, constraint, msg)?;
            Ok(TypedExpr::TEVar(v, t))
        },
        TypedExpr::TEBinop(b, e1, e2, old_type) => {
            // let e1 = new_texpr(*e1, constraint, msg);
            // let e2 = new_texpr(*e2, constraint, msg);
            let t = constrain(old_type, constraint, msg)?;
            Ok(TypedExpr::TEBinop(b, Box::new(*e1), Box::new(*e2), t))
        },
        TypedExpr::TEUnop(u, e1, old_type) => {
            // let e1 = new_texpr(*e1, constraint, msg);
            let t = constrain(old_type, constraint, msg)?;
            Ok(TypedExpr::TEUnop(u, Box::new(*e1), t))
        },
        TypedExpr::TInput(old_type) => {
            let t = constrain(old_type, constraint, msg)?;
            Ok(TypedExpr::TInput(t))
        }
    }
}

pub fn type_of_typedexpr(e : &ast::TypedExpr) -> types::Type {
    use ast::TypedExpr;
    match e {
        TypedExpr::TEConst(_, t) => t.clone(),
        TypedExpr::TEVar(_, t) => t.clone(),
        TypedExpr::TEBinop(_, _, _, t) => t.clone(),
        TypedExpr::TEUnop(_, _, t) => t.clone(),
        TypedExpr::TInput(t) => t.clone(),
    }
}

pub fn infer_expr(e : ast::Expr, ctx : &mut TypeContext) -> Result<ast::TypedExpr, CompileTimeError> {
    use ast::Expr;
    use ast::TypedExpr;
    match e {
        Expr::EBinop(b, e1, e2) => {
            use ast::Binop;
            let operand1 = infer_expr(*e1, ctx)?;
            let operand2 = infer_expr(*e2, ctx)?;

            match b {
                Binop::Plus | Binop::Minus | Binop::Times | Binop::Div => {
                    // temporal type is not (necessarily) encoded in arithmetic expression.
                    //     thus, perhaps `let ty1 = types::Type::Prod(Nil, types::SimpleType::Int)`?
                    let ty1 = types::Type::Prod(types::TemporalType::Current, types::SimpleType::Int);
                    let ty2 = types::Type::Prod(types::TemporalType::Current, types::SimpleType::Int);
                    let op1 = new_texpr(operand1, &ty1, "Left operand of an arithmetic binary operator must be an integer.".to_string())?;
                    let op2 = new_texpr(operand2, &ty2, "Right operand of an arithmetic binary operator must be an integer.".to_string())?;

                    let t = types::Type::Prod(types::TemporalType::Current, types::SimpleType::Int);
                    Ok(TypedExpr::TEBinop(b, Box::new(op1), Box::new(op2), t))
                },
                Binop::Or | Binop::And => {
                    // todo: truthiness & split behaviors for `and` and `or`
                    // todo: negation as failure?
                    let ty1 = types::Type::Prod(types::TemporalType::Current, types::SimpleType::Bool);
                    let ty2 = types::Type::Prod(types::TemporalType::Current, types::SimpleType::Bool);
                    let op1 = new_texpr(operand1, &ty1, "Left operand of a logical operator must be a boolean.".to_string())?;
                    let op2 = new_texpr(operand2, &ty2, "Right operand of a logical operator must be a boolean.".to_string())?;

                    let t = types::Type::Prod(types::TemporalType::Current, types::SimpleType::Bool);
                    Ok(TypedExpr::TEBinop(b, Box::new(op1), Box::new(op2), t))
                },
                Binop::Until => {
                    // todo: truthiness
                    let ty1 = types::Type::Prod(types::TemporalType::Current, types::SimpleType::Undefined);
                    let op1 = new_texpr(operand1, &ty1, "Left operand of Until should have type 'current'.".to_string())?;
                    let ty2 = types::Type::Prod(types::TemporalType::Until, types::SimpleType::Bool);
                    let op2 = new_texpr(operand2, &ty2, "Right operand of Until should have type 'until bool'. Basically a thunk if it has free variables...".to_string())?;

                    let t = type_of_typedexpr(&op1);
                    Ok(TypedExpr::TEBinop(b, Box::new(op1), Box::new(op2), t))
                },
                Binop::SUntil => { // the same as `Until` because `SUntil(A, B) :- Until(A, B) and Finally(B)` can be done when translating to LTL/BA.
                    // todo: truthiness
                    let ty1 = types::Type::Prod(types::TemporalType::Current, types::SimpleType::Undefined);
                    let op1 = new_texpr(operand1, &ty1, "Left operand of SUntil should have type 'current'.".to_string())?;
                    let ty2 = types::Type::Prod(types::TemporalType::Until, types::SimpleType::Bool);
                    let op2 = new_texpr(operand2, &ty2, "Right operand of SUntil should have type 'until bool'. Basically a thunk if it has free variables...".to_string())?;

                    let t = type_of_typedexpr(&op1);
                    Ok(TypedExpr::TEBinop(b, Box::new(op1), Box::new(op2), t))
                }
            }
        },
        Expr::EConst(c) => {
            use ast::Const;
            let t = match c {
                Const::Bool(_) => types::Type::Prod(types::TemporalType::Current, types::SimpleType::Bool),
                Const::Number(_) => types::Type::Prod(types::TemporalType::Current, types::SimpleType::Int)
            };
            Ok(TypedExpr::TEConst(c, t))
        },
        Expr::EUnop(u, e1) => {
            use ast::Unop;
            let operand1 = infer_expr(*e1, ctx)?;
            match u {
                Unop::Neg => {
                    let ty1 = types::Type::Prod(types::TemporalType::Current, types::SimpleType::Int);
                    let op1 = new_texpr(operand1, &ty1, "Neg operator only works on integers.".to_string())?;

                    let t = types::Type::Prod(types::TemporalType::Current, types::SimpleType::Int);
                    Ok(TypedExpr::TEUnop(u, Box::new(op1), t))
                },
                Unop::Not => {
                    // todo: truthiness
                    let ty1 = types::Type::Prod(types::TemporalType::Current, types::SimpleType::Bool);
                    let op1 = new_texpr(operand1, &ty1, "Not operator only works on booleans.".to_string())?;

                    let t = types::Type::Prod(types::TemporalType::Current, types::SimpleType::Bool);
                    Ok(TypedExpr::TEUnop(u, Box::new(op1), t))
                }
            }
        },
        Expr::EVar(s) => {
            let mut named_type = ctx.look_up(&s)?;
            if types::is_currently_available(&named_type.get_temporal()) {
                Ok(TypedExpr::TEVar(s, named_type))
            } else {
                Err(CurrentlyUnavailableError{message : 
                    "Variable doesn't have `current` type.".to_string()})?
            }            
        },
        Expr::Input => Ok(TypedExpr::TInput(types::Type::Prod(types::TemporalType::Current, types::SimpleType::Int)))
    }
}

// -> TypeError since infer_expr may result in ConstrainError
pub fn infer_command(cmd : ast::Command, ctx : &mut TypeContext) -> Result<ast::TypedCommand, CompileTimeError> {
    println!("     TIMESTEP {:?}    Command {:?}", ctx.clock, cmd);
    use ast::Command;
    use ast::TypedCommand;
    use types::Type;
    use types::TemporalType;
    match cmd {
        Command::Timestep => {
            panic!("Huh, timesteps should've been taken out with `split`... Why was `infer_command` called on one?")
        },
        Command::Global(v, e) => {
            let te = infer_expr(e, ctx)?;
            let t = Type::Prod(TemporalType::Global, type_of_typedexpr(&te).get_simpl());
            ctx.add_name(&v, t);
            Ok(TypedCommand::TGlobal(v, te))
        },
        Command::Next(v, e) => {
            let te = infer_expr(e, ctx)?;
            let t = Type::Prod(TemporalType::Next, type_of_typedexpr(&te).get_simpl());
            ctx.add_name(&v, t);
            Ok(TypedCommand::TNext(v, te))
        },
        Command::Update(v, e) => {
            let te = infer_expr(e, ctx)?;
            let t = Type::Prod(TemporalType::Future, type_of_typedexpr(&te).get_simpl());
            ctx.add_name(&v, t);
            Ok(TypedCommand::TUpdate(v, te))
        },
        Command::Finally(v, e) => {
            let te = infer_expr(e, ctx)?;
            let t = Type::TempNest(TemporalType::Global, Box::new(Type::Prod(TemporalType::Future, type_of_typedexpr(&te).get_simpl())));
            ctx.add_name(&v, t);
            Ok(TypedCommand::TFinally(v, te))
        },
        Command::Print(e) => {
            let te = infer_expr(e, ctx)?;
            Ok(TypedCommand::TPrint(te))
        }
    }
}

// x <- 3
/*
    Next(Until(x = 3, true))  # this allows reassignment, but it maintains forever if not violated (or reassigned).
*/

/*
pub enum TemporalType {
    Undefined,
    Until,
    Future,
}
*/
pub fn dupe_check_cmd(cmd : ast::Command, var_assign : &mut HashMap<ast::Var, (bool, bool, bool)>) -> Result<(), DupeAssignError> {
    use ast::Command;
    match cmd {
        Command::Timestep => {
            panic!("Huh, timesteps should've been taken out with `split`... Why was `dupe_check` called on one?")
        },
        Command::Global(v, _) => {
            if var_assign.get(&v).unwrap_or(&(false, false, false)).0 {
                Err(DupeAssignError 
                    {message : "Attempted Global assignment to a variable that was already Globally assigned to inside this timestep.".to_string()})
            } else {
                if let Some(tup) = var_assign.get(&v) {
                    var_assign.insert(v, (true, tup.1, tup.2));
                } else {
                    var_assign.insert(v, (false, false, false));
                }
                Ok(())
            }
        }, 
        Command::Next(v, _) => {
            if var_assign.get(&v).unwrap_or(&(false, false, false)).1 {
                Err(DupeAssignError
                    {message : "Attempted Next assignment to a variable that was already Nextly or Updatedly assigned to inside this timestep.".to_string()})
            } else {
                if let Some(tup) = var_assign.get(&v) {
                    var_assign.insert(v, (tup.1, true, tup.2));
                } else {
                    var_assign.insert(v, (false, false, false));
                }
                Ok(())
            }
        },
        Command::Update(v, _) => {
            if var_assign.get(&v).unwrap_or(&(false, false, false)).1 {
                Err(DupeAssignError 
                    {message : "Attempted Update assignment to a variable that was already Nextly or Updatedly assigned to inside this timestep.".to_string()})
            } else {
                if let Some(tup) = var_assign.get(&v) {
                    var_assign.insert(v, (tup.1, true, tup.2));
                } else {
                    var_assign.insert(v, (false, false, false));
                }
                Ok(())
            }
        },
        Command::Finally(v, _) => {
            if var_assign.get(&v).unwrap_or(&(false, false, false)).1 {
                Err(DupeAssignError
                    {message : "Attempted Finally assignment to a variable that was already Finally assigned to inside this timestep.".to_string()})
            } else {
                if let Some(tup) = var_assign.get(&v) {
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

pub fn dupe_check(block : Vec<ast::Command>) -> Result<(), AssignError> {
    // {var_name : (is_global, is_next_or_update, is_future)}
    let mut var_assign = HashMap::new();
    block.iter().try_for_each(|cmd| dupe_check_cmd(cmd.clone(), &mut var_assign))?;
    Ok(())
}

pub fn infer_timeblock(block : Vec<ast::Command>, ctx : &mut TypeContext) -> Result<ast::TypedTimeBlock, CompileTimeError> {
    println!("old variables: {:?}", ctx.vars);
    let cmds : Result<ast::TypedTimeBlock, _> = block.iter().map(|cmd| infer_command(cmd.clone(), ctx)).collect();
    println!("new variables: {:?}", ctx.vars);
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

pub fn arrange_by_dependencies(block : Vec<ast::Command>) -> Result<Vec<ast::Command>, CircularAssignError> {
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

    // scan for recursive definitions. since `defined_vars_of_command` excludes next/update, all recursive definitions are blocked.
    
    let mut graph = Graph::<Var, i32>::new();

    let var_map : HashMap<Var, NodeIndex> = defined_vars.into_iter().map(|def_var| (def_var.clone(), graph.add_node(def_var))).collect();
    // for var in defined_vars.into_iter() {
    //     let origin = graph.add_node(var);
    // }
    for (origin, destinations) in free_vars_by_cmd.iter() {
        for dest in destinations {
            graph.add_edge(*var_map.get(origin).unwrap(), *var_map.get(dest).unwrap(), 1);
        }
    }

    match toposort(&graph, None) {
        Ok(order) => {
            let top_order : Vec<Var> = order.iter().map(|node| graph.node_weight(node.clone()).unwrap().clone()).collect();
            let cmd_by_var : HashMap<Var, ast::Command> = block.iter().map(|cmd| (defined_var_of_command(&cmd), cmd)).filter(|(v, _)| v.is_some()).map(|(v, cmd)| (v.unwrap(), cmd.clone())).collect();
            Ok(top_order.iter().map(|var| cmd_by_var.get(var).unwrap().clone()).chain(delayed_commands.into_iter()).collect())
        },
        Err(err) => {
            Err(
                CircularAssignError {
                    message : format!("Error: Graph has cycle with node: {}", graph.node_weight(err.node_id()).unwrap().as_str()).to_string()
                }
            )
        }
    }
}

pub fn infer_program(program : ast::Program) -> Result<ast::TypedProgram, CompileTimeError> {
    let mut ctx = TypeContext {
        vars : HashMap::new(),
        clock : 0
    };

    // don't filter out empty blocks since `---\n---` is valid.
    let time_blocks : Vec<_> = program.split(|cmd| *cmd == ast::Command::Timestep).map(|tb| tb.to_vec()).collect();

    time_blocks.clone().iter().try_for_each(|tb| dupe_check(tb.clone()))?;

    let arranged_program : Vec<Result<Vec<ast::Command>, CircularAssignError>> = time_blocks.into_iter().map(|tb| arrange_by_dependencies(tb)).collect();
    let arranged_program : Result<Vec<Vec<ast::Command>>, CircularAssignError> = arranged_program.into_iter().collect();
    let arranged_program = arranged_program?;
    let inferred_program : Result<Vec<Vec<ast::TypedCommand>>, CompileTimeError> = arranged_program.iter().map(|block| infer_timeblock(block.to_vec(), &mut ctx)).collect();
    Ok(inferred_program?)
}


// wrapper's SimpleType should be `Nil`
pub fn wrap(ty : types::Type, wrapper : types::Type) -> types::Type {
    use types::Type;
    use types::TemporalType;
    use types::SimpleType;

    match (ty.clone(), wrapper) {
        (Type::Prod(TemporalType::Current, simpl), Type::Prod(tem, SimpleType::Undefined)) => Type::Prod(tem, simpl),
        (Type::Prod(TemporalType::Current, _), Type::TempNest(tem, nested)) => Type::Prod(tem, wrap(ty, *nested).get_simpl()),
        (Type::Prod(TemporalType::Undefined, simpl), Type::Prod(tem, SimpleType::Undefined)) => Type::Prod(tem, simpl),
        (Type::Prod(TemporalType::Undefined, _), Type::TempNest(tem, nested)) => Type::Prod(tem, wrap(ty, *nested).get_simpl()),
        _ => panic!("How the heckings did it reach this?")
    }
}


// `con_ty` should be `crate::types::Type::Prod(Current, SimpleType)`
pub fn constrain(te : types::Type, con_ty : &types::Type, msg : String) -> Result<types::Type, ConstrainError> {
    use types::TemporalType;
    use types::SimpleType;
    use types::Type;

    match (te, (*con_ty).clone()) {
        (Type::Union(a, b), _) => {
            let constrained_a = constrain(*a, con_ty, msg.clone());
            let constrained_b = constrain(*b, con_ty, msg.clone());

            if constrained_a.is_ok() {
                if constrained_b.is_ok() {
                    Ok(Type::Union(Box::new(constrained_a.unwrap()), Box::new(constrained_b.unwrap())))
                } else {
                    Ok(constrained_a.unwrap())
                }
            } else {
                if constrained_b.is_ok() {
                    Ok(constrained_b.unwrap())
                } else {
                    Err(ConstrainError {message : msg})
                }
            }
        },
        (Type::TempNest(a, b), Type::Prod(c, _)) => {
            if a == TemporalType::Undefined {
                constrain(*b, con_ty, msg)
            } else if c == TemporalType::Undefined {
                Ok(Type::TempNest(a, Box::new(constrain(*b, con_ty, msg).expect("Constraint error"))))
            } else if (a == TemporalType::Global) || (a == TemporalType::Current) || (a == TemporalType::Until) || (a == TemporalType::Future) {
                constrain(*b, con_ty, msg)
            } else {
                Err(ConstrainError {message : msg})
            }
        },
        (Type::Prod(a, b), Type::Prod(c, d)) => {
            if b == d || b == SimpleType::Undefined || d == SimpleType::Undefined {
                // if both `b` and `d` are SimpleType::Undefined, `simp_t` should be SimpleType::Undefined as well.
                let simp_t = if b == SimpleType::Undefined { d } else { b };
                match (a.clone(), c.clone()) {
                    (TemporalType::Undefined, _) => Ok(Type::Prod(c, simp_t)),
                    (_, TemporalType::Undefined) => Ok(Type::Prod(a, simp_t)),  // if both `a` and `c` are Nil, the temporaltype is just Nil.
                    (TemporalType::Global, TemporalType::Current) => Ok(Type::Prod(TemporalType::Current, simp_t)),  // the other way won't work since `Current` doesn't necessarily mean `Global`.
                    (TemporalType::Until, TemporalType::Current) => Ok(Type::Prod(TemporalType::Current, simp_t)),
                    // (TemporalType::Future, TemporalType::Current) => Ok(Type::Prod(TemporalType::Current, simp_t)),
                    (TemporalType::Current, TemporalType::Current) => Ok(Type::Prod(TemporalType::Current, simp_t)),
                    _ => Err(ConstrainError {message : msg})
                }
            } else {
                Err(ConstrainError {message : msg})
            }
        },
        _ => Err(ConstrainError {message : msg})
    }
}