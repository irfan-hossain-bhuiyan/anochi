use std::fmt::Display;

use derive_more::Deref;

use crate::{prelude::Mappable, token::Position};

pub trait CodeErrorType {}
#[derive(Debug,Default,Deref)]
pub struct CodeError<T>{
    #[deref]
    error_type:T,
    error_pos:Position,
}
impl<T> CodeError<T> {
    pub fn new(error_type: T, error_pos: Position) -> Self {
        Self { error_type, error_pos }
    }
    pub fn new_no_pos(error_type: T)->Self{
        Self { error_type, error_pos: Position::default() }
    }

    pub fn error_type(&self) -> &T {
        &self.error_type
    }
    pub fn into_error_type(self) -> T {
        self.error_type
    }
}

impl<T:Display> CodeError<T>{
    pub fn err_str(&self, code_str: &str) -> String {
        let position = &self.error_pos;
        let error_message = format!(
            "Error at line {}, column {}: {}\n{}",
            position.line, position.column, self.error_type, code_str
        );
        error_message
    }
}
impl<T,U> Mappable<T,U> for CodeError<T>{
    fn inner_map<F>(self,f:&mut F) -> Self::Mapped
        where
            F: FnMut(T) -> U {
        Self::Mapped{
            error_type:f(self.error_type),
            error_pos:self.error_pos,
        }
    }

    type Mapped=CodeError<U>;
}

