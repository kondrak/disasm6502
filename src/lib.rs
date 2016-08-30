//! [Disasm6502](https://github.com/kondrak/disasm6502) - a 6502 disassembler crate.
//!
//! A crate providing functionality to disassemble 6502 binary code. Supports decoding of forbidden instructions, provides information about cycle count and which registers the instruction accesses. Acceptable data input can be either an array of bytes, a vector of bytes or a binary file.
//!
//!# Quick Start
//!
//!```
//!extern crate disasm6502;
//!
//!fn main()
//!{
//!    let bytes = vec![0x05, 0x0B, 0x6C, 0x01, 0x02,
//!                     0x0A, 0xA2, 0xFF, 0x20, 0x02,
//!                     0xFD, 0x78, 0xD0, 0xFC, 0x1D,
//!                     0x05, 0x1E, 0x04, 0x15, 0x02,
//!                     0x96, 0xAB, 0x58, 0x61, 0x01,
//!                     0x91, 0xFB];
//!
//!    // disassemble...
//!    let instructions = disasm6502::from_array(&bytes).unwrap();
//!
//!    // ...and print!
//!    for i in instructions.iter() {
//!        println!("{}", i);
//!    }
//!}
//!```
pub mod error;
pub mod instruction;

use error::Result;
use instruction::Instruction;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

/// Disassembles data from binary file using $0000 as start address.
///
/// # Examples
///
/// ```
/// extern crate disasm6502;
///
/// // first instruction will be located at $0000
/// let instructions = disasm6502::from_file("examples/data.bin").unwrap();
/// ```
pub fn from_file(filename: &str) -> Result<Vec<Instruction>> {
    from_addr_file(filename, 0x0000)
}

/// Disassembles data from binary file using a custom start address.
///
/// # Examples
///
/// ```
/// extern crate disasm6502;
///
/// // first instruction will be located at $0800
/// let instructions = disasm6502::from_addr_file("examples/data.bin", 0x0800).unwrap();
/// ```
pub fn from_addr_file(filename: &str, start_address: u16) -> Result<Vec<Instruction>> {
    let path = Path::new(&filename);
    let mut file = try!(File::open(&path));
    let mut bytes = Vec::new();
    try!(file.read_to_end(&mut bytes));

    from_addr_array(&bytes, start_address)
}

/// Disassembles data from array of bytes using $0000 as start address.
///
/// # Examples
///
/// ```
/// extern crate disasm6502;
///
/// let bytes = vec![0x05, 0x0B, 0x6C, 0x01, 0x02];
///
/// // first instruction will be located at $0000
/// let instructions = disasm6502::from_array(&bytes).unwrap();
/// ```
pub fn from_array(bytes: &[u8]) -> Result<Vec<Instruction>> {
    from_addr_array(bytes, 0x0000)
}

/// Disassembles data from array of bytes using a custom start address.
///
/// # Examples
///
/// ```
/// extern crate disasm6502;
///
/// let bytes = vec![0x05, 0x0B, 0x6C, 0x01, 0x02];
///
/// // first instruction will be located at $0800
/// let instructions = disasm6502::from_addr_array(&bytes, 0x0800).unwrap();
/// ```
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
