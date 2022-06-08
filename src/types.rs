use std::collections::HashMap;

#[derive(Clone)]
#[derive(PartialEq)]
#[derive(Debug)]
pub enum SimpleType {
    Bool,
    // Float,
    Int, 
    // Pdf
    Undefined
}
#[derive(Clone)]
#[derive(PartialEq)]
#[derive(Debug)]
pub enum TemporalType {
    // should this hold the time? t=0, t=1, t=2, t=final, etc.
    Global,
    Next,
    Current,
    Undefined,
    Until,
    Future
}

#[derive(Clone)]
#[derive(PartialEq)]
#[derive(Debug)]
pub enum Type {
    Prod(TemporalType, SimpleType),
    TempNest(TemporalType, Box<Type>),
    Union(Box<Type>, Box<Type>)
}

impl Type {
    pub fn get_simpl(&mut self) -> SimpleType {
        match self {
            Type::Prod(_, s) => (*s).clone(),
            Type::TempNest(_, ty) => ty.get_simpl(),
            Type::Union(_, _) => panic!("Rn, I don't feel like figuring out whether I want a set of simpletypes, an error, or whatever.")
        }
    }

    // pub fn get_temporal(&mut self) -> TemporalType {}
}

pub fn advance_type(t : Box<Type>) -> Option<Box<Type>> {
    match *t {
        Type::Prod(ref temp, ref simpl) => 
            match temp {
                TemporalType::Next => {println!("reached a {:?}", t); Some(Box::new(Type::Prod(TemporalType::Current, simpl.clone())))},
                TemporalType::Current => None,
                TemporalType::Until | TemporalType::Global | TemporalType::Future | TemporalType::Undefined => Some(t)
            }
        ,
        Type::TempNest(ref temp, ref ty) => {
            match temp {
                TemporalType::Next => Some(Box::new((**ty).clone())),
                TemporalType::Current => advance_type(Box::new((**ty).clone())),
                TemporalType::Global | TemporalType::Until | TemporalType::Future | TemporalType::Undefined => Some(t)
            }
        },
        Type::Union(a, b) => {
            if let Some(a) = advance_type(Box::new(*a)) {
                match *a.clone() {
                    Type::Prod(TemporalType::Current, _) => Some(b),
                    _ => {
                        if let Some(b) = advance_type(Box::new(*b)) {
                            Some(Box::new(Type::Union(Box::new(*a),  Box::new(*b))))
                        } else {
                            Some(a)
                        }
                    }
                }
            } else {
                Some(b)
            }
        }
    }
}

#[derive(Debug)]
pub struct TypeContext {
    // only global scope
    pub vars: HashMap<String, Type>,
    pub clock: i32
}

impl TypeContext {
    // gives type of variable name.
    pub fn look_up(&mut self, var_name: &String) -> Option<&Type> {
        self.vars.get(var_name)
    }
    // adds new variable
    pub fn add_name(&mut self, var_name: &String, data: Type) -> () {
        if let Some(typs) = self.vars.get(var_name) {
            self.vars.insert(var_name.to_string(), Type::Union(Box::new(data), Box::new(typs.clone())));
        } else {
            self.vars.insert(var_name.to_string(), data);
        }
    }

    pub fn step_time(&mut self) -> () {
        self.clock += 1;

        // hashmaps aren't ordered in Rust...
        /*
        self.vars = self.vars.keys().map(|s| (*s).clone()).zip(
        //     self.vars.values().filter_map(|t| advance_type(Box::new((*t).clone()))).map(|t| *t)
        ).collect();
        */

        self.vars = self.vars.clone().into_iter().map(|(name, ty)| (name, advance_type(Box::new(ty.clone())))).filter(|(_, ty)| ty.is_some()).map(|(name, ty)| (name, *ty.unwrap())).collect();
    }
}