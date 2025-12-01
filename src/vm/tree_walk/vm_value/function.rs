use crate::{ast::StatementNode, types::TypeId};

struct Function{
    func_sig:FunctionSignature,
    statement: StatementNode
}

impl Function {
    fn new(func_sig: FunctionSignature, statement: StatementNode) -> Self {
        Self { func_sig, statement }
    }
}
struct FunctionSignature{
    input:TypeId,
    output:TypeId,
}

impl FunctionSignature {
    fn new(input: TypeId, output: TypeId) -> Self {
        Self { input, output }
    }
}


