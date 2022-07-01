// pub(crate) type InferResult<T> = Result<T, ()>;

// #[derive(Debug, Clone)]
// pub struct ConstrainError;
// impl fmt::Display for ConstrainError {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         write!(f, "couldn't constrain types")
//     }
// }
#[derive(Debug)]
pub struct CyclicalUntilDependency {
    pub message: String,
}
#[derive(Debug)]
pub struct InferError {
    pub message: String,
}
#[derive(Debug)]
pub struct ConstrainError {
    pub message: String,
}
#[derive(Debug)]
pub struct TemporalConflictError {
    pub message: String,
}
#[derive(Debug)]
pub struct CurrentlyUnavailableError {
    pub message: String,
}
#[derive(Debug)]
pub struct DupeAssignError {
    pub message: String,
}
#[derive(Debug)]
pub struct RecursiveAssignError {
    pub message: String,
}
#[derive(Debug)]
pub struct CircularAssignError {
    pub message: String,
}
#[derive(Debug)]
pub struct NameError {
    pub message: String,
}
#[derive(Debug)]
pub struct InputError {
    pub message: String,
}
// `a <!> true`
#[derive(Debug)]
pub struct UntilVoidError {
    pub message: String,
}
// `a <!> (some_val_that_ain't_ever_true)`
#[derive(Debug)]
pub struct SUntilConditionUnsatisfied {
    pub message: String,
}
#[derive(Debug)]
pub enum ExecutionTimeError {
    // Should I put `Name`, `Input`, `Until`, `SUntil`, and `Immediate` in some new `AccessError`?
    Type(TypeError),
    Assign(AssignError),
    Name(NameError),
    Input(InputError),
    Until(UntilVoidError),
    SUntil(SUntilConditionUnsatisfied),
}
#[derive(Debug)]
pub enum Error {
    Static(CompileTimeError),
    Dynamic(ExecutionTimeError),
}
#[derive(Debug)]
pub enum TypeError {
    Infer(InferError),
    Constrain(ConstrainError),
    Immediacy(CurrentlyUnavailableError),
    Conflict(TemporalConflictError),
}
#[derive(Debug)]
pub enum AssignError {
    Dupe(DupeAssignError),
    Recursive(RecursiveAssignError),
    Circular(CircularAssignError),
    Cyclical(CyclicalUntilDependency),
}
#[derive(Debug)]
pub enum CompileTimeError {
    Type(TypeError),
    Assign(AssignError),
    Access(NameError),
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
impl From<TypeError> for ExecutionTimeError {
    fn from(e: TypeError) -> Self {
        ExecutionTimeError::Type(e)
    }
}
impl From<AssignError> for ExecutionTimeError {
    fn from(e: AssignError) -> Self {
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
        ExecutionTimeError::Type(TypeError::Immediacy(e))
    }
}
impl From<TemporalConflictError> for TypeError {
    fn from(e: TemporalConflictError) -> Self {
        TypeError::Conflict(e)
    }
}
impl From<TemporalConflictError> for CompileTimeError {
    fn from(e: TemporalConflictError) -> Self {
        CompileTimeError::Type(TypeError::Conflict(e))
    }
}
impl From<TemporalConflictError> for ExecutionTimeError {
    fn from(e: TemporalConflictError) -> Self {
        ExecutionTimeError::Type(TypeError::Conflict(e))
    }
}
impl From<CyclicalUntilDependency> for AssignError {
    fn from(e: CyclicalUntilDependency) -> Self {
        AssignError::Cyclical(e)
    }
}
