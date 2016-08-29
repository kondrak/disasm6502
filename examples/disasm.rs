extern crate disasm6502;

fn main()
{
    let bytes = vec![0x05, 0x0B, 0x6C, 0x01, 0x02, 0x0A, 0xA2, 0xFF, 0x20, 0x02, 0xFD, 0x78, 0xD0, 0xFC, 0x1D, 0x05, 0x1E, 0x04, 0x15, 0x02, 0x96, 0xAB, 0x58, 0x61, 0x01, 0x91, 0xFB];

    let instructions = disasm6502::from_array(&bytes).unwrap_or_else(|e| {
        panic!("{}", e);
    });

    // print all instructions!
    for i in instructions.iter() {
        // format cycle count
        let cc = if i.extra_cycle {
            format!("(*{}) ", i.cycles)
        } else {
            format!("({})  ", i.cycles)
        };
        
        // format read registers
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

        // format written registers
        let rw = if let Some(ref reg_written) = i.registers_written {
            let mut r_str = String::from("   Writes:[");
            for r in reg_written.iter() {
                r_str.push_str(format!("{}", r).as_str());
            }
            r_str.push_str("]");
            r_str.to_owned()
        } else {
            String::from("")
        };

        // format instruction mnemonic
        let instr_str = format!("{}", i);
        let mut spacing = String::new();

        // format extra spacing for better looks!
        for _ in 0..(32 - instr_str.len()) {
            spacing.push_str(" ");
        }

        println!("{}{}{}{}{}", instr_str, spacing, cc, rr, rw);
    }

    println!("${:04X}: .END", bytes.len());
}
