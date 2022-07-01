use crate::errors;


#[derive(Clone, PartialEq, Hash, Debug)]
pub enum SimpleType {
    Bool,
    // Float,
    Int,
    // Pdf
    Undefined
}

// the deepest temporal type would be `Global(Future(Until))`. Note that `Until(Until)` should be simplified to `Until`

// `when_available? current/next/future; when_dissipates? fleeting/lingering/always; is_until (ie. can be cancelled)?`
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
#[derive(Clone, PartialEq, Hash, Debug)]
pub enum TemporalAvailability {
    Current,
    Next,
    Future,
    Undefined
}
#[derive(Clone, PartialEq, Hash, Debug)]
pub enum TemporalPersistency {
    Fleeting,
    Lingering,
    Always,
    Undefined
}

#[derive(Clone, PartialEq, Hash, Debug)]
pub struct UntilDependencies {pub weak : Vec<u64>, pub strong : Vec<u64>}

impl UntilDependencies {
    pub fn is_empty(&mut self) -> bool {
        self.weak.is_empty() && self.strong.is_empty()
    }
}

#[derive(Clone, PartialEq, Hash, Debug)]
pub struct TemporalType {
    pub when_available: TemporalAvailability,
    pub when_dissipates: TemporalPersistency,
    pub is_until: Option<UntilDependencies>,
}

#[derive(Clone, PartialEq, Hash, Debug)]
pub struct Type(pub TemporalType, pub SimpleType);

impl Type {
    pub fn get_simpl(&self) -> SimpleType {
        let Type(_, s) = self;
        s.clone()
    }
    pub fn get_temporal(&self) -> TemporalType {
        let Type(t, _) = self;
        t.clone()
    }
}

pub fn is_currently_available(ty: &TemporalType) -> bool {
    match ty.when_available {
        TemporalAvailability::Current => true,
        TemporalAvailability::Next => false,
        TemporalAvailability::Future => true,
        TemporalAvailability::Undefined => panic!("The immediacy of `undefined` is meaningless.")
    }
}

pub fn advance_type(t: Type) -> Option<Type> {
    let temp = t.get_temporal();
    let simpl = t.get_simpl();

    match temp.when_dissipates {
        TemporalPersistency::Fleeting => {
            match temp.when_available {
                TemporalAvailability::Current => None,
                TemporalAvailability::Next => {
                    if temp.is_until.is_some() {
                        Some(Type(
                            TemporalType {
                                when_available : TemporalAvailability::Current,
                                when_dissipates : TemporalPersistency::Lingering,
                                is_until : temp.is_until
                            }, simpl
                        ))
                    } else {
                        Some(Type(
                            TemporalType {
                                when_available : TemporalAvailability::Current,
                                when_dissipates : TemporalPersistency::Fleeting,
                                is_until : None
                            }, simpl
                        ))
                    }    
                },
                TemporalAvailability::Future => panic!("While `future` should be advanced to `future` (since finally), but it should be impossible for there to be a non-always future. `Future` should only occur with `Global(Future[Until])`."),
                TemporalAvailability::Undefined => panic!("The immediacy of `undefined` is meaningless.")
            }
        }
        TemporalPersistency::Lingering => Some(t),
        TemporalPersistency::Always => Some(t),
        TemporalPersistency::Undefined => panic!("The persistency of `undefined` is meaningless.")
    }
}

pub fn get_most_immediate_type(types: Vec<Type>) -> Option<Type> {
    let most_immediate = types.into_iter().max_by_key(|t| match t.get_temporal().when_available {
            TemporalAvailability::Current => 1,
            TemporalAvailability::Next => -1,
            TemporalAvailability::Future => 0,
            TemporalAvailability::Undefined => panic!("The immediacy of `undefined` is meaningless.")
        }
    )?;

    if most_immediate.get_temporal().when_available == TemporalAvailability::Next {
        return None
    } else {
        Some(most_immediate)
    }
}

// Global, Next, Until, Current, Future
// Next(Until), Global(Until), Future(Until)     Next(Future), Global(Future)
// Global(Future(Until)), Next(Future(Until))
// ~~~Current(Until)~~~

pub fn resolve_temporal_conflicts(mut types: Vec<Type>, candidate: Type, forgive_until : bool) -> Result<Vec<Type>, errors::TemporalConflictError> {
    if types.len() > 0 {
        let mut resolved = Vec::new();

        while !types.is_empty() {
            let ty = types.pop().unwrap();

            if ty.get_temporal().when_available == candidate.get_temporal().when_available {
                if forgive_until {
                    if ty.get_temporal().is_until.is_none() {
                        Err(errors::TemporalConflictError { message : format!("Conflicting types {:?} and {:?}", ty, candidate).to_string()})?
                    }
                } else {
                    Err(errors::TemporalConflictError { message : format!("Conflicting types {:?} and {:?}", ty, candidate).to_string()})?
                }
            } else {
                resolved.push(ty.clone());
            }
        }
        resolved.push(candidate);
        Ok(resolved)
    } else {
        Ok([candidate].to_vec())
    }
}
