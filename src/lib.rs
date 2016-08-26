pub mod error;

use error::Result;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

#[derive(Debug)]
pub struct Instruction {
    foo: u8
}

impl Instruction {
    fn new() -> Instruction {
        Instruction {
            foo: 0
        }
    }
}

pub fn from_file(filename: &str) -> Result<Vec<Instruction>> {
    let path = Path::new(&filename);
    let mut file = try!(File::open(&path));
    let mut bytes = Vec::new();
    try!(file.read_to_end(&mut bytes));

    from_array(&bytes)
}

pub fn from_vec(bytes: &Vec<u8>) -> Result<Vec<Instruction>> {
    from_array(&bytes)
}

pub fn from_array(bytes: &[u8]) -> Result<Vec<Instruction>> {
    let mut ret = Vec::<Instruction>::new();
    ret.push(Instruction::new());
    // TODO
    Ok(ret)
}
