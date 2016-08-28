pub mod error;
pub mod instruction;

use error::Result;
use instruction::Instruction;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

pub fn from_file(filename: &str) -> Result<Vec<Instruction>> {
    let path = Path::new(&filename);
    let mut file = try!(File::open(&path));
    let mut bytes = Vec::new();
    try!(file.read_to_end(&mut bytes));

    from_array(&bytes)
}

pub fn from_array(bytes: &[u8]) -> Result<Vec<Instruction>> {
    let mut ret = Vec::<Instruction>::new();

    let mut index: usize = 0;
    while index < bytes.len() {
        let instruction = instruction::decode(&mut index, &bytes);
        ret.push(instruction);
    }

    Ok(ret)
}
