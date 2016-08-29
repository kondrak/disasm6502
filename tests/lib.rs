extern crate disasm6502;

#[test]
fn check_disasm() {
    let bytes = vec![0x05, 0x0B, 0x6C, 0x01, 0x02, 0x0A, 0xA2, 0xFF, 0x20, 0x02, 0xFD, 0x78, 0xD0, 0xFC, 0x1D, 0x05, 0x1E, 0x04, 0x15, 0x02, 0x96, 0xAB, 0x58, 0x61, 0x01, 0x91, 0xFB];

    let instructions = disasm6502::from_array(&bytes).unwrap();

    for i in instructions.iter() {
        println!("{}", i);
    }
}

#[test]
fn check_disasm_file() {
    let instructions = disasm6502::from_file("tests/file_test.prg").unwrap();

    for i in instructions.iter() {
        println!("{}", i);
    }
}
