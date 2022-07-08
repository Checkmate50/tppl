use crate::errors;

#[derive(Clone, PartialEq, Hash, Debug)]
pub enum SimpleType {
    Bool,
    // Float,
    Int,
    // Pdf,
    Predicate(Vec<Box<SimpleType>>, Box<SimpleType>),
    Option(Box<SimpleType>),
    Undefined,
}

impl SimpleType {
    pub fn is_predicate(&self) -> bool {
        match self {
            SimpleType::Predicate(_, _) => true,
            _ => false,
        }
    }
    // excludes PDFs
    pub fn is_numeric(&self) -> bool {
        match self {
            SimpleType::Int => true,
            _ => false,
        }
    }
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
    Undefined,
}
#[derive(Clone, PartialEq, Hash, Debug)]
pub enum TemporalPersistency {
    Fleeting,
    Lingering,
    Always,
    Undefined,
}

#[derive(Clone, PartialEq, Hash, Debug)]
pub struct UntilDependencies {
    pub weak: Vec<u64>,
    pub strong: Vec<u64>,
}

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
        TemporalAvailability::Undefined => panic!("The immediacy of `undefined` is meaningless."),
    }
}

pub fn advance_type(temp: TemporalType) -> Option<TemporalType> {
    match temp.when_dissipates {
        TemporalPersistency::Fleeting => {
            match temp.when_available {
                TemporalAvailability::Current => None,
                TemporalAvailability::Next => {
                    if temp.is_until.is_some() {
                        Some(
                            TemporalType {
                                when_available : TemporalAvailability::Current,
                                when_dissipates : TemporalPersistency::Lingering,
                                is_until : temp.is_until
                            }
                        )
                    } else {
                        Some(
                            TemporalType {
                                when_available : TemporalAvailability::Current,
                                when_dissipates : TemporalPersistency::Fleeting,
                                is_until : None
                            }
                        )
                    }
                },
                TemporalAvailability::Future => panic!("While `future` should be advanced to `future` (since finally), but it should be impossible for there to be a non-always future. `Future` should only occur with `Global(Future[Until])`."),
                TemporalAvailability::Undefined => panic!("The immediacy of `undefined` is meaningless.")
            }
        }
        TemporalPersistency::Lingering => Some(temp),
        TemporalPersistency::Always => Some(temp),
        TemporalPersistency::Undefined => panic!("The persistency of `undefined` is meaningless.")
    }
}

// Global, Next, Until, Current, Future
// Next(Until), Global(Until), Future(Until)     Next(Future), Global(Future)
// Global(Future(Until)), Next(Future(Until))
// ~~~Current(Until)~~~

pub fn resolve_temporal_conflicts(
    temps: Vec<TemporalType>,
    candidate: TemporalType,
    forgive_until: bool,
) -> Result<Vec<TemporalType>, errors::TemporalConflictError> {
    let mut resolved = Vec::new();
    let mut temps = temps.clone();

    while !temps.is_empty() {
        let temp = temps.pop().unwrap();

        if temp.when_available == candidate.when_available {
            if !forgive_until || temp.is_until.is_none() {
                Err(errors::TemporalConflictError {
                    message: format!("Conflicting types {:?} and {:?}", temp, candidate)
                        .to_string(),
                })?
            }
        } else {
            resolved.push(temp);
        }
    }
    resolved.push(candidate);
    Ok(resolved)
}

pub fn resolve_simple_conflicts(
    a: SimpleType,
    b: SimpleType,
) -> Result<SimpleType, errors::SimpleConflictError> {
    match (a.clone(), b.clone()) {
        (_, SimpleType::Undefined) => Ok(a),
        (SimpleType::Undefined, _) => Ok(b),
        (a, b) if a == b => Ok(a),
        _ => Err(errors::SimpleConflictError {
            message: format!("").to_string(),
        })?,
    }
}
