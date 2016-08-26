extern crate disasm6502;

fn main()
{
    let bytes: [u8; 2] = [0xEA, 0xEA];

    let instructions = disasm6502::from_array(&bytes).unwrap_or_else(|e| {
        panic!("{}", e);
    });

    for i in instructions.iter() {
        // print instruction
        println!("{:?}", i);
    }
    // TODO
}
