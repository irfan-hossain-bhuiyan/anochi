use std::fmt::Display;

use crate::{ token::Position};
#[derive(Debug,Default,)]
pub struct CodeError<T>{
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
