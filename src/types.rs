use crate::errors;

#[derive(Clone, PartialEq, Hash, Debug)]
pub enum SimpleType {
    Bool,
    // Float,
    Int,
    // Pdf,
    Predicate(Vec<Box<SimpleType>>, Box<SimpleType>),
    // currently, `interpreters::run_predicate_definitions` doesn't use `Option` in its fullness.
    // It first checks if it's correct (ie. a "Some"). Then, it does type-checking.
    // Failure is handled without using `Option` since `interpreters::is_success` pattern matches on texpr (ignoring type).
    // Note: `ast::new_texpr` handles `Option(x) = false | x_instance`
    Option(Box<SimpleType>), // these remind me of Python's Options atp. It isn't used as a tagged-sum.
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
    pub fn is_option(&self) -> bool {
        match self {
            SimpleType::Option(_) => true,
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
        (SimpleType::Option(a), b) => resolve_simple_conflicts(*a, b),
        (a, SimpleType::Option(b)) => resolve_simple_conflicts(a, *b),
        (SimpleType::Predicate(arg_ts1, ret1), SimpleType::Predicate(arg_ts2, ret2)) => {
            let arg_ts: Result<Vec<SimpleType>, errors::SimpleConflictError> = arg_ts1
                .into_iter()
                .zip(arg_ts2.into_iter())
                .map(|(a, b)| resolve_simple_conflicts(*a, *b))
                .collect();
            let arg_ts: Vec<Box<SimpleType>> = arg_ts?.into_iter().map(|s| Box::new(s)).collect();
            Ok(SimpleType::Predicate(
                arg_ts,
                Box::new(resolve_simple_conflicts(*ret1, *ret2)?),
            ))
        }
        (a, b) if a == b => Ok(a),
        _ => Err(errors::SimpleConflictError {
            message: format!("Types {:?}\nand {:?}\ncannot be resolved.", a, b).to_string(),
        })?,
    }
}

pub fn constrain(ty: &Type, con_ty: &Type, msg: String) -> Result<Type, errors::ConstrainError> {
    let Type(temp1, simp1) = ty;
    let Type(temp2, simp2) = con_ty;

    let simp: SimpleType = match (simp1, simp2) {
        (SimpleType::Undefined, b) => b.to_owned(),
        (a, SimpleType::Undefined) => a.to_owned(),
        (SimpleType::Option(a), b) => constrain(
            &Type(temp1.to_owned(), *(a.to_owned())),
            &Type(temp2.to_owned(), b.to_owned()),
            msg.clone(),
        )?
        .get_simpl(),
        (a, SimpleType::Option(b)) => constrain(
            &Type(temp1.to_owned(), a.to_owned()),
            &Type(temp2.to_owned(), *(b.to_owned())),
            msg.clone(),
        )?
        .get_simpl(),
        (SimpleType::Predicate(args_types1, ret1), SimpleType::Predicate(args_types2, ret2)) => {
            let arg_ts: Result<Vec<Type>, errors::ConstrainError> = args_types1
                .iter()
                .zip(args_types2.iter())
                .map(|(a, b)| {
                    constrain(
                        &Type(temp1.clone(), *(a.to_owned())),
                        &Type(temp2.clone(), *(b.to_owned())),
                        msg.clone(),
                    )
                })
                .collect();
            let arg_ts: Vec<Box<SimpleType>> =
                arg_ts?.iter().map(Type::get_simpl).map(Box::new).collect();
            let ret = constrain(
                &Type(temp1.clone(), *(ret1.to_owned())),
                &Type(temp2.clone(), *(ret2.to_owned())),
                msg.clone(),
            );
            SimpleType::Predicate(arg_ts, Box::new(ret?.get_simpl()))
        }
        (a, b) if a == b => a.to_owned(),
        _ => Err(errors::ConstrainError {
            message: msg.clone(),
        })?,
    };

    let temp = match (temp1, temp2) {
        (
            TemporalType {
                when_available: TemporalAvailability::Undefined,
                when_dissipates: TemporalPersistency::Undefined,
                is_until: None,
            },
            b,
        ) => b,
        (
            a,
            TemporalType {
                when_available: TemporalAvailability::Undefined,
                when_dissipates: TemporalPersistency::Undefined,
                is_until: None,
            },
        ) => a,
        // I doubt I'll need to add cases like `(Global, Current)`, but perhaps I'll need to
        (a, b) if a == b => a,
        _ => Err(errors::ConstrainError { message: msg })?,
    };

    Ok(Type(temp.to_owned(), simp))
}
