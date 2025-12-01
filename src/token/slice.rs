
use crate::token::Position;

use super::tokenizer::Token;
use std::fmt;
use std::ops::Deref;

#[derive(Debug)]
#[repr(transparent)]
pub struct TokenSlice([Token]);

impl TokenSlice {
    pub(crate) fn from_slice(tokens: &[Token]) -> &Self {
        // SAFETY: TokenSlice is repr(transparent) over [Token]
        // This is private - only TokenContainer can create TokenSlice from raw arrays
        unsafe { &*(tokens as *const [Token] as *const Self) }
    }

    pub fn slice(&self, start: usize, end: usize) -> &Self {
        Self::from_slice(&self.0[start..end])
    }
    pub fn pos_range(&self) -> Position{
        if self.0.is_empty() {
            return Position::default(); // Return an empty range if no tokens
        }
        let start = self.0.first().unwrap().position.clone();
        let end = self.0.last().unwrap().position.clone();
        start.extend(end)
    }

    pub fn slice_from(&self, start: usize) -> &Self {
        Self::from_slice(&self.0[start..])
    }

    pub fn slice_to(&self, end: usize) -> &Self {
        Self::from_slice(&self.0[..end])
    }

    pub fn first(&self) -> Option<&Token> {
        self.0.first()
    }

    pub fn last(&self) -> Option<&Token> {
        self.0.last()
    }
}

// Deref implementation for convenient slice access
impl Deref for TokenSlice {
    type Target = [Token];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// AsRef implementations for convenient conversion
impl AsRef<[Token]> for TokenSlice {
    fn as_ref(&self) -> &[Token] {
        &self.0
    }
}

// Display implementation for TokenSlice
impl fmt::Display for TokenSlice {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TokenSlice[{}]: ", self.len())?;
        for (i, token) in self.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{:?}", token.token_type)?;
        }
        Ok(())
    }
}
