//! [Disasm6502](https://github.com/kondrak/disasm6502) - a 6502 disassembler
pub mod error;
pub mod instruction;

use error::Result;
use instruction::Instruction;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

/// Disassembles data from file.
pub fn from_file(filename: &str) -> Result<Vec<Instruction>> {
    let path = Path::new(&filename);
    let mut file = try!(File::open(&path));
    let mut bytes = Vec::new();
    try!(file.read_to_end(&mut bytes));

    from_array(&bytes)
}

/// Disassembles data from array of bytes with start address at $0000.
pub fn from_array(bytes: &[u8]) -> Result<Vec<Instruction>> {
    from_addr_array(bytes, 0x0000)
}

/// Disassembles data from array of bytes using a start address.
pub fn from_addr_array(bytes: &[u8], start_address: u16) -> Result<Vec<Instruction>> {
    let mut ret = Vec::<Instruction>::new();
    let mut index: usize = 0;
    let mut next_addr = start_address;

    while index < bytes.len() {
        let instruction = instruction::decode(next_addr, &mut index, &bytes);
        ret.push(instruction);
        next_addr = start_address + index as u16;
    }

    Ok(ret)
}
