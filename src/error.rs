//! Error type for disasm6502 crate.
use std::fmt;
use std::io;
use std::result;

/// Result type used throughout the crate.
pub type Result<T> = result::Result<T, Disasm6502Error>;

///Error type for disasm6502 crate.
#[derive(Debug)]
pub enum Disasm6502Error {
    /// I/O error
    Io(io::Error)
}

impl fmt::Display for Disasm6502Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Disasm6502Error::Io(ref err) => err.fmt(f)
        }
    }
}

impl From<io::Error> for Disasm6502Error {
    fn from(err: io::Error) -> Disasm6502Error {
        Disasm6502Error::Io(err)
    }
}
