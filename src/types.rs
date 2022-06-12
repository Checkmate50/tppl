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
            Type::Prod(_, s) => s.clone(),
            Type::TempNest(_, ty) => ty.get_simpl(),
            Type::Union(_, _) => panic!("Rn, I don't feel like figuring out whether I want a set of simpletypes, an error, or whatever.")
        }
    }

    // getting the first layer is good enough rn. perhaps I'll later return nested temporal types
    pub fn get_temporal(&mut self) -> TemporalType {
        match self {
            Type::Prod(t, _) => t.clone(),
            Type::TempNest(temp, _) => temp.clone(),
            Type::Union(_, _) => panic!("Rn, I don't feel like figuring out whether I want a set of temporaltypes, an error, or whatever.")
        }
    }
}

pub fn advance_type(t : Box<Type>) -> Option<Box<Type>> {
    match *t {
        Type::Prod(ref temp, ref simpl) => 
            match temp {
                TemporalType::Next => {println!("reached a {:?}", t); Some(Box::new(Type::Prod(TemporalType::Current, simpl.clone())))},
                TemporalType::Current => None,
                TemporalType::Future => Type::Prod(TemporalType::Until, simp.clone()),
                TemporalType::Until | TemporalType::Global | TemporalType::Undefined => Some(t)
            }
        ,
        Type::TempNest(ref temp, ref ty) => {
            match temp {
                TemporalType::Next => Some(Box::new((**ty).clone())),
                TemporalType::Current => advance_type(Box::new((**ty).clone())),
                TemporalType::Future => Type::TempNest(TemporalType::Until, Box::new(ty.clone())),
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

pub fn get_most_immediate_type(ty: &Type) -> &Type {
    match ty {
        Type::Prod(_, _) => ty,
        &Type::TempNest(_, _) => ty,
        Type::Union(ty1, ty2) => {
            let immediate_ty1 = get_most_immediate_type(ty1);
            let immediate_ty2 = get_most_immediate_type(ty2);

            match (immediate_ty1, immediate_ty2) {
                (Type::Prod(TemporalType::Current, _), Type::Prod(_, _)) => immediate_ty1,
                (Type::Prod(_, _), Type::Prod(TemporalType::Current, _)) => immediate_ty2,
                (Type::Prod(TemporalType::Global, _), Type::Prod(_, _)) => immediate_ty1,
                (Type::Prod(_, _), Type::Prod(TemporalType::Global, _)) => immediate_ty2,
                (Type::Prod(TemporalType::Next, _), Type::Prod(_, _)) => immediate_ty2,
                (Type::Prod(_, _), Type::Prod(TemporalType::Next, _)) => immediate_ty1,
                _ => panic!("There is no way to decide which is more pressing between {:?} and {:?}.", immediate_ty1, immediate_ty2)
            }
        }
    }
}

pub fn is_currently_available(ty : &TemporalType) -> bool {
    match ty {
        TemporalType::Global | TemporalType::Current | TemporalType::Future | TemporalType::Until => true,
        TemporalType::Next => false,
        TemporalType::Undefined => false
    }
}