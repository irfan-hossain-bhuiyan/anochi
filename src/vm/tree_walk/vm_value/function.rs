use crate::{
    ast::{StatNode, StatementNode},
    prelude::{IndexPtr, Mappable},
    token::{Position, tokenizer::HasPosition},
    types::{TypeContainer, TypeId}, vm::tree_walk::VmErrorType,
};

#[derive(Debug, Clone, PartialEq)]
pub struct VmFunc {
    param: TypeId,
    output: Option<TypeId>,
    body: StatNode<Position>,
}

impl VmFunc {
    pub fn new_checked<T: HasPosition>(
        param: TypeId,
        output: Option<TypeId>,
        body: StatNode<T>,
        type_container: &TypeContainer,
    ) -> Option<Self> {
        if !type_container.get_type(&param)?.is_product() {
            return None;
        }
        let body: StatNode<Position> = body.inner_map(&mut |x: T| x.get_position().clone());
        Some(Self {
            param,
            output,
            body,
        })
    }
    pub fn new<T:HasPosition>(
        param: TypeId,
        output: Option<TypeId>,
        body: StatNode<T>,
        type_container: &TypeContainer,
    ) -> Self {
        Self::new_checked(param, output, body, type_container).unwrap()
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
            _ => return Err(VmErrorType::TypeMismatch("")),
        }
    }

    pub(crate) fn get_statement(&self) -> &StatementNode {
        &self.body
    }
}
pub type FuncId = IndexPtr<VmFunc>;
