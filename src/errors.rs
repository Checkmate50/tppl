#[derive(Debug)]
pub struct BuiltinNameConflictError {
    pub message: String,
}
#[derive(Debug)]
pub struct CyclicalUntilDependency {
    pub message: String,
}
#[derive(Debug)]
pub struct PredicateExprError {
    pub message: String,
}
#[derive(Debug)]
pub struct NoPredicateError {
    pub message: String,
}
#[derive(Debug)]
pub struct MultiplePredicateError {
    pub message: String,
}
#[derive(Debug)]
pub struct ImproperCallError {
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
pub struct SimpleConflictError {
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
pub struct AssertionError {
    pub message: String,
}
#[derive(Debug)]
pub enum ExecutionTimeError {
    // Should I put `Name`, `Input`, `Until`, `SUntil`, and `Immediate` in some new `AccessError`?
    Type(TypeError),
    Predicate(PredError),
    Assign(AssignError),
    Name(NameError),
    Input(InputError),
    Until(UntilVoidError),
    SUntil(SUntilConditionUnsatisfied),
    Access(AccessError),
    Assert(AssertionError),
}
#[derive(Debug)]
pub enum PredError {
    Zero(NoPredicateError),
    Mult(MultiplePredicateError),
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
    TempConflict(TemporalConflictError),
    SimplConflict(SimpleConflictError),
    Call(ImproperCallError),
}
#[derive(Debug)]
pub enum AssignError {
    Dupe(DupeAssignError),
    Recursive(RecursiveAssignError),
    Circular(CircularAssignError),
    Cyclical(CyclicalUntilDependency),
    BuiltIn(BuiltinNameConflictError),
}
#[derive(Debug)]
pub enum AccessError {
    Name(NameError),
    PredExpr(PredicateExprError),
}
#[derive(Debug)]
pub enum CompileTimeError {
    Type(TypeError),
    Assign(AssignError),
    Access(AccessError),
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
impl From<PredicateExprError> for AccessError {
    fn from(e: PredicateExprError) -> Self {
        AccessError::PredExpr(e)
    }
}
impl From<PredicateExprError> for CompileTimeError {
    fn from(e: PredicateExprError) -> Self {
        CompileTimeError::Access(AccessError::PredExpr(e))
    }
}
impl From<AccessError> for CompileTimeError {
    fn from(e: AccessError) -> Self {
        CompileTimeError::Access(e)
    }
}
impl From<AccessError> for ExecutionTimeError {
    fn from(e: AccessError) -> Self {
        ExecutionTimeError::Access(e)
    }
}
impl From<AssertionError> for ExecutionTimeError {
    fn from(e: AssertionError) -> Self {
        ExecutionTimeError::Assert(e)
    }
}
impl From<PredicateExprError> for ExecutionTimeError {
    fn from(e: PredicateExprError) -> Self {
        ExecutionTimeError::Access(AccessError::PredExpr(e))
    }
}
impl From<NameError> for AccessError {
    fn from(e: NameError) -> Self {
        AccessError::Name(e)
    }
}
impl From<NameError> for CompileTimeError {
    fn from(e: NameError) -> Self {
        CompileTimeError::Access(AccessError::Name(e))
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
impl From<ConstrainError> for ExecutionTimeError {
    fn from(e: ConstrainError) -> Self {
        ExecutionTimeError::Type(TypeError::Constrain(e))
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
        TypeError::TempConflict(e)
    }
}
impl From<TemporalConflictError> for CompileTimeError {
    fn from(e: TemporalConflictError) -> Self {
        CompileTimeError::Type(TypeError::TempConflict(e))
    }
}
impl From<TemporalConflictError> for ExecutionTimeError {
    fn from(e: TemporalConflictError) -> Self {
        ExecutionTimeError::Type(TypeError::TempConflict(e))
    }
}
impl From<CyclicalUntilDependency> for AssignError {
    fn from(e: CyclicalUntilDependency) -> Self {
        AssignError::Cyclical(e)
    }
}
impl From<SimpleConflictError> for TypeError {
    fn from(e: SimpleConflictError) -> Self {
        TypeError::SimplConflict(e)
    }
}
impl From<SimpleConflictError> for CompileTimeError {
    fn from(e: SimpleConflictError) -> Self {
        CompileTimeError::Type(TypeError::SimplConflict(e))
    }
}
impl From<SimpleConflictError> for ExecutionTimeError {
    fn from(e: SimpleConflictError) -> Self {
        ExecutionTimeError::Type(TypeError::SimplConflict(e))
    }
}
impl From<NoPredicateError> for PredError {
    fn from(e: NoPredicateError) -> Self {
        PredError::Zero(e)
    }
}
impl From<NoPredicateError> for ExecutionTimeError {
    fn from(e: NoPredicateError) -> Self {
        ExecutionTimeError::Predicate(PredError::Zero(e))
    }
}
impl From<MultiplePredicateError> for PredError {
    fn from(e: MultiplePredicateError) -> Self {
        PredError::Mult(e)
    }
}
impl From<MultiplePredicateError> for ExecutionTimeError {
    fn from(e: MultiplePredicateError) -> Self {
        ExecutionTimeError::Predicate(PredError::Mult(e))
    }
}
impl From<ImproperCallError> for TypeError {
    fn from(e: ImproperCallError) -> Self {
        TypeError::Call(e)
    }
}
impl From<ImproperCallError> for ExecutionTimeError {
    fn from(e: ImproperCallError) -> Self {
        ExecutionTimeError::Type(TypeError::Call(e))
    }
}
impl From<ImproperCallError> for CompileTimeError {
    fn from(e: ImproperCallError) -> Self {
        CompileTimeError::Type(TypeError::Call(e))
    }
}
impl From<PredError> for ExecutionTimeError {
    fn from(e: PredError) -> Self {
        ExecutionTimeError::Predicate(e)
    }
}
impl From<BuiltinNameConflictError> for AssignError {
    fn from(e: BuiltinNameConflictError) -> Self {
        AssignError::BuiltIn(e)
    }
}
impl From<BuiltinNameConflictError> for CompileTimeError {
    fn from(e: BuiltinNameConflictError) -> Self {
        CompileTimeError::Assign(AssignError::BuiltIn(e))
    }
}
impl From<BuiltinNameConflictError> for ExecutionTimeError {
    fn from(e: BuiltinNameConflictError) -> Self {
        ExecutionTimeError::Assign(AssignError::BuiltIn(e))
    }
}
