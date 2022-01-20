extern crate disasm6502;

#[test]
fn check_disasm() {
    let bytes = vec![0x05, 0x0B, 0x6C, 0x01, 0x02, 0x0A, 0xA2, 0xFF, 0x20, 0x02, 0xFD, 0x78, 0xD0, 0xFC, 0x1D, 0x05, 0x1E, 0x04, 0x15, 0x02, 0x96, 0xAB, 0x58, 0x61, 0x01, 0x91, 0xFB, 0x90];

    let instructions = disasm6502::from_array(&bytes).unwrap();

    for i in instructions.iter() {
        println!("{}", i);
    }
}

#[test]
fn check_disasm_file() {
    let instructions = disasm6502::from_file("tests/test_pattern.bin").unwrap();

    for i in instructions.iter() {
        let cc = if i.extra_cycle {
            format!("(*{}) ", i.cycles)
        } else {
            format!("({})  ", i.cycles)
        };

        let rr = if let Some(ref reg_read) = i.registers_read {
            let mut r_str = String::from(" Reads:[");
            for r in reg_read.iter() {
                r_str.push_str(format!("{}", r).as_str());
            }
            r_str.push_str("]");
            r_str.to_owned()
        } else {
            String::from("          ")
        };

        let rw = if let Some(ref reg_written) = i.registers_written {
            let mut r_str = String::from(" Writes:[");
            for r in reg_written.iter() {
                r_str.push_str(format!("{}", r).as_str());
            }
            r_str.push_str("]");
            r_str.to_owned()
        } else {
            String::from("")
        };

        let af = if let Some(ref aff_flags) = i.affected_flags {
            let mut f_str = String::from(" Affects:[");
            for f in aff_flags.iter() {
                f_str.push_str(format!("{}", f).as_str());
            }
            f_str.push_str("]");
            f_str.to_owned()
        } else {
            String::from("")
        };

        println!("{}{}{}{}{}", i, cc, rr, rw, af);
    }
}

#[test]
#[should_panic]
fn check_disasm_file_fail() {
    let _ = disasm6502::from_file("tests/foo").unwrap_or_else(|e| { panic!("{}", e); }); }

#[test]
fn check_relative_addressing() {
    const BEQ: u8 = 0xF0;

    // Test various relative addressing edge conditions
    struct RelativeTest {
        start: u16,
        bytes: &'static [u8],
        expect: u16
    }
    static TESTS: [RelativeTest; 60] = [
        RelativeTest { start: 0x0000, bytes: &[BEQ, 0xFD], expect: 0xFFFF },
        RelativeTest { start: 0x0000, bytes: &[BEQ, 0xFE], expect: 0x0000 },
        RelativeTest { start: 0x0000, bytes: &[BEQ, 0xFF], expect: 0x0001 },
        RelativeTest { start: 0x0000, bytes: &[BEQ, 0x00], expect: 0x0002 },
        RelativeTest { start: 0x0000, bytes: &[BEQ, 0x01], expect: 0x0003 },
        RelativeTest { start: 0x0000, bytes: &[BEQ, 0x02], expect: 0x0004 },
        RelativeTest { start: 0x0000, bytes: &[BEQ, 0x7D], expect: 0x007F },
        RelativeTest { start: 0x0000, bytes: &[BEQ, 0x7E], expect: 0x0080 },
        RelativeTest { start: 0x0000, bytes: &[BEQ, 0x7F], expect: 0x0081 },
        RelativeTest { start: 0x0000, bytes: &[BEQ, 0x80], expect: 0xFF82 },
        RelativeTest { start: 0x0000, bytes: &[BEQ, 0x81], expect: 0xFF83 },
        RelativeTest { start: 0x0000, bytes: &[BEQ, 0x82], expect: 0xFF84 },
        RelativeTest { start: 0x4000, bytes: &[BEQ, 0xFD], expect: 0x3FFF },
        RelativeTest { start: 0x4000, bytes: &[BEQ, 0xFE], expect: 0x4000 },
        RelativeTest { start: 0x4000, bytes: &[BEQ, 0xFF], expect: 0x4001 },
        RelativeTest { start: 0x4000, bytes: &[BEQ, 0x00], expect: 0x4002 },
        RelativeTest { start: 0x4000, bytes: &[BEQ, 0x01], expect: 0x4003 },
        RelativeTest { start: 0x4000, bytes: &[BEQ, 0x02], expect: 0x4004 },
        RelativeTest { start: 0x4000, bytes: &[BEQ, 0x7D], expect: 0x407F },
        RelativeTest { start: 0x4000, bytes: &[BEQ, 0x7E], expect: 0x4080 },
        RelativeTest { start: 0x4000, bytes: &[BEQ, 0x7F], expect: 0x4081 },
        RelativeTest { start: 0x4000, bytes: &[BEQ, 0x80], expect: 0x3F82 },
        RelativeTest { start: 0x4000, bytes: &[BEQ, 0x81], expect: 0x3F83 },
        RelativeTest { start: 0x4000, bytes: &[BEQ, 0x82], expect: 0x3F84 },
        RelativeTest { start: 0x7FFF, bytes: &[BEQ, 0xFD], expect: 0x7FFE },
        RelativeTest { start: 0x7FFF, bytes: &[BEQ, 0xFE], expect: 0x7FFF },
        RelativeTest { start: 0x7FFF, bytes: &[BEQ, 0xFF], expect: 0x8000 },
        RelativeTest { start: 0x7FFF, bytes: &[BEQ, 0x00], expect: 0x8001 },
        RelativeTest { start: 0x7FFF, bytes: &[BEQ, 0x01], expect: 0x8002 },
        RelativeTest { start: 0x7FFF, bytes: &[BEQ, 0x02], expect: 0x8003 },
        RelativeTest { start: 0x7FFF, bytes: &[BEQ, 0x7D], expect: 0x807E },
        RelativeTest { start: 0x7FFF, bytes: &[BEQ, 0x7E], expect: 0x807F },
        RelativeTest { start: 0x7FFF, bytes: &[BEQ, 0x7F], expect: 0x8080 },
        RelativeTest { start: 0x7FFF, bytes: &[BEQ, 0x80], expect: 0x7F81 },
        RelativeTest { start: 0x7FFF, bytes: &[BEQ, 0x81], expect: 0x7F82 },
        RelativeTest { start: 0x7FFF, bytes: &[BEQ, 0x82], expect: 0x7f83 },
        RelativeTest { start: 0x8000, bytes: &[BEQ, 0xFD], expect: 0x7FFF },
        RelativeTest { start: 0x8000, bytes: &[BEQ, 0xFE], expect: 0x8000 },
        RelativeTest { start: 0x8000, bytes: &[BEQ, 0xFF], expect: 0x8001 },
        RelativeTest { start: 0x8000, bytes: &[BEQ, 0x00], expect: 0x8002 },
        RelativeTest { start: 0x8000, bytes: &[BEQ, 0x01], expect: 0x8003 },
        RelativeTest { start: 0x8000, bytes: &[BEQ, 0x02], expect: 0x8004 },
        RelativeTest { start: 0x8000, bytes: &[BEQ, 0x7D], expect: 0x807F },
        RelativeTest { start: 0x8000, bytes: &[BEQ, 0x7E], expect: 0x8080 },
        RelativeTest { start: 0x8000, bytes: &[BEQ, 0x7F], expect: 0x8081 },
        RelativeTest { start: 0x8000, bytes: &[BEQ, 0x80], expect: 0x7F82 },
        RelativeTest { start: 0x8000, bytes: &[BEQ, 0x81], expect: 0x7F83 },
        RelativeTest { start: 0x8000, bytes: &[BEQ, 0x82], expect: 0x7F84 },
        RelativeTest { start: 0xFFFF, bytes: &[BEQ, 0xFD], expect: 0xFFFE },
        RelativeTest { start: 0xFFFF, bytes: &[BEQ, 0xFE], expect: 0xFFFF },
        RelativeTest { start: 0xFFFF, bytes: &[BEQ, 0xFF], expect: 0x0000 },
        RelativeTest { start: 0xFFFF, bytes: &[BEQ, 0x00], expect: 0x0001 },
        RelativeTest { start: 0xFFFF, bytes: &[BEQ, 0x01], expect: 0x0002 },
        RelativeTest { start: 0xFFFF, bytes: &[BEQ, 0x02], expect: 0x0003 },
        RelativeTest { start: 0xFFFF, bytes: &[BEQ, 0x7D], expect: 0x007E },
        RelativeTest { start: 0xFFFF, bytes: &[BEQ, 0x7E], expect: 0x007F },
        RelativeTest { start: 0xFFFF, bytes: &[BEQ, 0x7F], expect: 0x0080 },
        RelativeTest { start: 0xFFFF, bytes: &[BEQ, 0x80], expect: 0xFF81 },
        RelativeTest { start: 0xFFFF, bytes: &[BEQ, 0x81], expect: 0xFF82 },
        RelativeTest { start: 0xFFFF, bytes: &[BEQ, 0x82], expect: 0xFF83 },
    ];

    for test in TESTS.iter() {
        // Disassemble the instruction
        let instructions = disasm6502::from_addr_array(&test.bytes, test.start).unwrap();
        assert_eq!(instructions.len(), 1);
        let disassembly = format!("{}", instructions.get(0).unwrap());
        // Extract the address of the branch target
        let address = u16::from_str_radix(
            disassembly
                .split_ascii_whitespace()
                .last()
                .unwrap()
                .trim_start_matches('$'),
            16,
        ).unwrap();
        // Confirm the relative address was applied correctly.
        assert_eq!(address, test.expect);
    }
}
