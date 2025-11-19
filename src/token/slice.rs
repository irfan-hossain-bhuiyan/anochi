use std::fmt;
use std::ops::Deref;
use super::tokenizer::Token;

#[derive(Debug)]
#[repr(transparent)]
pub struct TokenSlice<'a>([Token<'a>]);

impl<'a> TokenSlice<'a> {
    pub(crate) fn from_slice<'b>(tokens: &'b [Token<'a>]) -> &'b Self {
        // SAFETY: TokenSlice is repr(transparent) over [Token]
        // This is private - only TokenContainer can create TokenSlice from raw arrays
        unsafe { &*(tokens as *const [Token<'a>] as *const Self) }
    }

    pub fn slice(&self, start: usize, end: usize) -> &Self {
        Self::from_slice(&self.0[start..end])
    }

    pub fn slice_from(&self, start: usize) -> &Self {
        Self::from_slice(&self.0[start..])
    }

    pub fn slice_to(&self, end: usize) -> &Self {
        Self::from_slice(&self.0[..end])
    }

    pub fn get_str_slice(&self) -> &'a str {
        let Some(first_token) = self.0.first() else {
            return "";
        };
        let last_token = self.0.last().unwrap();

        let start_ptr = first_token.position.slice.as_ptr();
        let last_slice = last_token.position.slice;
        let end_ptr = unsafe { last_slice.as_ptr().add(last_slice.len()) };

        let start_offset = start_ptr as usize;
        let end_offset = end_ptr as usize;
        let slice_len = end_offset - start_offset;
        // Tokens are guaranteed sequential from TokenContainer
        unsafe {
            let slice_ptr = start_ptr;
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(slice_ptr, slice_len))
        }
    }
}

// Deref implementation for convenient slice access
impl<'a> Deref for TokenSlice<'a> {
    type Target = [Token<'a>];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// AsRef implementations for convenient conversion
impl<'a> AsRef<[Token<'a>]> for TokenSlice<'a> {
    fn as_ref(&self) -> &[Token<'a>] {
        &self.0
    }
}

// Display implementation for TokenSlice
impl<'a> fmt::Display for TokenSlice<'a> {
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
