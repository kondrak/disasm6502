extern crate disasm6502;

fn main()
{
    let bytes: [u8; 7] = [0xA2, 0xFF, 0x20, 0x02, 0xFD, 0x78, 0xEA];

    let instructions = disasm6502::from_array(&bytes).unwrap_or_else(|e| {
        panic!("{}", e);
    });

    for i in instructions.iter() {
        // print instruction
        println!("{}", i);
    }
    // TODO
}
