use macros::generate_unchecked;

use crate::{
    ast::StatementNode,
    prelude::IndexPtr,
    types::{TypeContainer, TypeId}, vm::tree_walk::VmErrorType,
};

#[derive(Debug, Clone, PartialEq)]
pub struct VmFunc {
    param: TypeId,
    output: Option<TypeId>,
    body: StatementNode,
}

impl VmFunc {
    #[generate_unchecked]
    pub fn new_checked(
        param: TypeId,
        output: Option<TypeId>,
        body: StatementNode,
        type_container: &TypeContainer,
    ) -> Option<Self> {
        type_container.get_type(&param)?.as_product()?;
        Some(Self {
            param,
            output,
            body,
        })
    }
    
    pub fn get_param(&self) -> TypeId {
        self.param
    }
    pub fn update_output_type(&mut self, output: TypeId) -> Result<(), VmErrorType> {
        match self.output {
            None => {
                self.output = Some(output);
                Ok(())
            }
            Some(x) if x == output => {
                self.output = Some(output);
                Ok(())
            }
            _ => Err(VmErrorType::TypeMismatch("")),
        }
    }

    pub(crate) fn get_statement(&self) -> &StatementNode {
        &self.body
    }
}
pub type FuncId = IndexPtr<VmFunc>;
