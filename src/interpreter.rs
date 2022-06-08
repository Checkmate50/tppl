use std::collections::HashMap;

use crate::ast;
use crate::types;
use crate::typecheck;
use ast::TypedExpr;
use ast::TypedCommand;
use types::Type;
use types::SimpleType;
use types::TemporalType;


#[derive(Clone)]
#[derive(Debug)]
pub enum Value {
    Expression(types::Type, ast::TypedExpr),
    Union(Box<Value>, Box<Value>)
}

#[derive(Debug)]
pub struct NameError {
    message : String
}
#[derive(Debug)]
pub struct InputError {
    message : String
}
#[derive(Debug)]
pub enum ExecutionTimeError {
    Type(typecheck::TypeError),
    Assign(typecheck::AssignError),
    Name(NameError),
    Input(InputError)
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


pub fn advance_time(val : Value) -> Option<Value> {
    match val {
        Value::Expression(t, v) => {
            let aged_type = *types::advance_type(Box::new(t))?;
            Some(Value::Expression(aged_type, v))
        },
        Value::Union(a, b) => {
            let aged_a = advance_time(*a);
            let aged_b = advance_time(*b);

            if let Some(a) = aged_a {
                if let Some(b) = aged_b {
                    Some(Value::Union(Box::new(a), Box::new(b)))
                } else {
                    aged_a
                }
            } else {
                aged_b
            }
        }
    }
}

pub struct VarContext {
    pub vars: HashMap<String, Value>,
    pub clock: i32
}

impl VarContext {
    pub fn look_up(&mut self, var_name: &String) -> Result<Value, NameError> {
        if let Some(val) = self.vars.get(var_name) {
            Ok(get_most_immediate_val(*val))
        } else {
            Err(NameError{message : format!("There is no variable with name {}", var_name).to_string()})   
        }
    }

    pub fn add_var(&mut self, var_name: &String, data: &TypedExpr, typ: &Type,) -> () {
        if let Some(vals) =self.vars.get(var_name) {
            self.vars.insert(var_name.to_string(), Value::Union(Box::new(Value::Expression(*typ, *data)), Box::new(vals.clone())));
        } else {
            self.vars.insert(var_name.to_string(), Value::Expression(*typ, *data));
        }
    }

    pub fn step_time(&mut self) -> () {
        self.clock += 1;
        self.vars = self.vars.iter().map(|(name, val)| (name.clone(), advance_time(val.clone()))).filter(|(_, val)| val.is_some()).map(|(name, val)| (name, val.unwrap())).collect();
    }
}

pub fn get_most_immediate_val(v: Value) -> Value {
    match v {
        Value::Expression(_, _) => v,
        Value::Union(v1, v2) => {
            let immediate_v1 = get_most_immediate_val(*v1);
            let immediate_v2 = get_most_immediate_val(*v2);
            let (Value::Expression(ty1, _), Value::Expression(ty2, _)) = (immediate_v1.clone(), immediate_v2.clone());
            match (ty1, ty2) {
                (Type::Prod(TemporalType::Future, _), Type::Prod(_, _)) => immediate_v2,
                (Type::Prod(_, _), Type::Prod(TemporalType::Future, _)) => immediate_v1,
                (Type::Prod(TemporalType::Current, _), Type::Prod(_, _)) => immediate_v1,
                (Type::Prod(_, _), Type::Prod(TemporalType::Current, _)) => immediate_v2,
                (Type::Prod(TemporalType::Global, _), Type::Prod(_, _)) => immediate_v1,
                (Type::Prod(_, _), Type::Prod(TemporalType::Global, _)) => immediate_v2,
                _ => panic!("There is no way to decide which is more pressing between {:?} and {:?}.", immediate_v1, immediate_v2)
            }
        }
    }
}

pub fn type_of_constant(c : ast::Const) -> types::Type {
    match c {
        ast::Const::Number(_) => types::Type::Prod(types::TemporalType::Current, types::SimpleType::Int),
        ast::Const::Bool(_) => types::Type::Prod(types::TemporalType::Current, types::SimpleType::Bool)
    }
}

pub fn eval_expr(e : TypedExpr, ctx : &mut VarContext) -> Result<TypedExpr, ExecutionTimeError> {
    match e {
        TypedExpr::TEConst(_, _) => Ok(e),
        TypedExpr::TEVar(var_name, t) => {
            let Value::Expression(_, texpr) = ctx.look_up(&var_name)?;
            Ok(texpr)
        },
        TypedExpr::TEBinop(b, e1, e2, t) => {
            use ast::Const;

            let e1 = eval_expr(*e1, ctx)?;
            let e2 = eval_expr(*e2, ctx)?;
            let (c1, c2) = match (e1, e2) {
                (TypedExpr::TEConst(c1, _), ast::TypedExpr::TEConst(c2, _)) => (c1, c2),
                _ => panic!("Did not receive a constant after evaluation.")
            };
            
            let constant = match b {
                Plus => {
                    match (c1, c2) {
                        (Const::Number(n1), Const::Number(n2)) => Const::Number(n1 + n2),
                        _ => panic!("Type-checking failed??")
                    }
                },
                Minus => match (c1, c2) {
                    (Const::Number(n1), Const::Number(n2)) => Const::Number(n1 - n2),
                    _ => panic!("Type-checking failed??")
                },
                Times => match (c1, c2) {
                    (Const::Number(n1), Const::Number(n2)) => Const::Number(n1 * n2),
                    _ => panic!("Type-checking failed??")
                },
                Div => match (c1, c2) {
                    (Const::Number(n1), Const::Number(n2)) => Const::Number((n1 / n2) as i64),
                    _ => panic!("Type-checking failed??")
                },
                Or => match (c1, c2) {
                    (Const::Bool(b1), Const::Bool(b2)) => Const::Bool(b1 || b2),
                    _ => panic!("Type-checking failed??")
                },
                And => match (c1, c2) {
                    (Const::Bool(b1), Const::Bool(b2)) => Const::Bool(b1 && b2),
                    _ => panic!("Type-checking failed??")
                },
                Until => match (c1, c2) {
                    // will have to preserve `until`
                    // Const::Bool(b1), Const::Bool(cond) => ,
                    // Const::Int(n1), Const::Bool(cond) => ,
                    // _ => panic!("Type-checking failed??")
                    _ => panic!("Not Implemented")
                },
                SUntil => match (c1, c2) {
                    _ => panic!("Not Implemented")
                }
            };
            Ok(TypedExpr::TEConst(constant, type_of_constant(constant)))
        },
        TypedExpr::TEUnop(u, e1, t) => {
            use ast::Const;

            let e1 = eval_expr(*e1, ctx)?;
            let c = match e1 {
                TypedExpr::TEConst(c, _) => c,
                // todo: untils are later
                _ => panic!("Did not receive a constant after evaluation.")
            };
            
            let constant = match u {
                Neg => {
                    match c {
                        Const::Bool(b) => panic!("Type-checking failed??"),
                        Const::Number(n) => Const::Number(-n)
                    }
                },
                Not => {
                    match c {
                        Const::Bool(b) => Const::Bool(!b),
                        Const::Number(n) => panic!("Type-checking failed??")
                    }
                }
            };
            Ok(TypedExpr::TEConst(constant, type_of_constant(constant)))
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
    match c {
        TypedCommand::TGlobal(v, texpr) => {
            let val = eval_expr(texpr, ctx)?;
            ctx.add_var(&v, &val, &Type::Prod(TemporalType::Global, SimpleType::Undefined));
            Ok(())
        },
        TypedCommand::TNext(v, texpr) => {
            let val = eval_expr(texpr, ctx)?;
            ctx.add_var(&v, &val, &Type::Prod(TemporalType::Next, SimpleType::Undefined));
            Ok(())
        },
        TypedCommand::TUpdate(v, texpr) => {
            let val = eval_expr(texpr, ctx)?;
            ctx.add_var(&v, &val, &Type::TempNest(TemporalType::Next, Box::new(Type::Prod(TemporalType::Until, SimpleType::Undefined))));
            Ok(())
        },
        TypedCommand::TFinally(v, texpr) => {
            let val = eval_expr(texpr, ctx)?;
            ctx.add_var(&v, &val, &Type::TempNest(TemporalType::Global, Box::new(Type::Prod(TemporalType::Future, SimpleType::Undefined))));
            Ok(())
        },
        TypedCommand::TPrint(texpr) => {
            let TypedExpr::TEConst(c, _) = eval_expr(texpr, ctx)?;
            match c {
                ast::Const::Bool(b) => println!("{}", b),
                ast::Const::Number(n) => println!("{}", n)
            };
            Ok(())
        },
    }
}

pub fn exec_time_block(time_block : ast::TypedTimeBlock, ctx : &mut VarContext) -> Result<(), ExecutionTimeError> {
    time_block.iter().try_for_each(|cmd| exec_command(*cmd, ctx))
}

pub fn exec_program(pgrm : ast::TypedProgram) -> Result<(), ExecutionTimeError> {
    let mut ctx = VarContext {
        vars : HashMap::new(),
        clock : 0
    };

    pgrm.iter().try_for_each(|tb| exec_time_block(*tb, &mut ctx))
}