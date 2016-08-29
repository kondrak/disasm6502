extern crate disasm6502;

fn main()
{
    let bytes = vec![0x05, 0x0B, 0x6C, 0x01, 0x02, 0x0A, 0xA2, 0xFF, 0x20, 0x02, 0xFD, 0x78, 0xD0, 0xFC, 0x1D, 0x05, 0x1E, 0x04, 0x15, 0x02, 0x96, 0xAB, 0x58, 0x61, 0x01, 0x91, 0xFB];

    let instructions = disasm6502::from_array(&bytes).unwrap_or_else(|e| {
        panic!("{}", e);
    });

    for i in instructions.iter() {
        // print instruction
        let rr = if let Some(ref reg_read) = i.registers_read {
            let mut r_str = String::from(" R:[ ");
            for r in reg_read.iter() {
                r_str.push_str(format!("{} ", r).as_str());
            }
            r_str.push_str("]");
            r_str.to_owned()
        } else {
            String::from("")
        };

        let rw = if let Some(ref reg_written) = i.registers_written {
            let mut r_str = String::from(" W:[ ");
            for r in reg_written.iter() {
                r_str.push_str(format!("{} ", r).as_str());
            }
            r_str.push_str("]");
            r_str.to_owned()
        } else {
            String::from("")
        };
        
        println!("{}{}{}", i, rr, rw);
    }

    println!("${:04X}: .END", bytes.len());
}
