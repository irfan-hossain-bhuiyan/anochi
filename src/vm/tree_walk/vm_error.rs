/// Error types for VM evaluation.

use thiserror::Error;

use crate::{ast::{ Identifier}, code_error::CodeError, prelude::Mappable};
#[derive(Error, Debug, PartialEq)]
pub enum VmErrorType {
    /// Division by zero error
    #[error("Division by zero")]
    DivisionByZero,
    /// Type mismatch error
    #[error("Type is mismatched,{0:?}")]
    TypeMismatch(&'static str),
    /// Undefined identifier error
    #[error("Undefined identifier: {0}")]
    UndefinedIdentifier(Identifier),
    /// Invalid operation error
    #[error("Invalid operation: {0}")]
    InvalidOperation(String),
    #[error("Unsupproted Operation {0:?}")]
    Unsupported(String),
    #[error("Error for that are not supported yet.")]
    InvalidTypeDefination,
    #[error("Having same name in scope")]
    SameVariableName,
    #[error("Calling a non funciton with function")]
    CallingNonFunc,
    #[error("Function input parameter is not struct.")]
    FuncInvalidInput,
}
pub type VmError=CodeError<VmErrorType>;

impl VmErrorType {
    /// Returns `true` if the vm error is [`TypeMismatch`].
    ///
    /// [`TypeMismatch`]: VmErrorType::TypeMismatch
    pub fn is_type_mismatch(&self) -> bool {
        matches!(self, Self::TypeMismatch(..))
    }
    //pub fn 
}

