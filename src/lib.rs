pub mod error;

use error::Result;

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

pub fn from_file(filename: &str) {
    // TODO
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
