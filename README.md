# disasm6502

[![Crates.io](https://img.shields.io/crates/v/disasm6502.svg)](https://crates.io/crates/disasm6502)
[![Documentation](https://docs.rs/disasm6502/badge.svg)](https://docs.rs/disasm6502)
[![build](https://github.com/kondrak/disasm6502/actions/workflows/rust.yml/badge.svg)](https://github.com/kondrak/disasm6502/actions/workflows/rust.yml)
[![Coverage Status](https://coveralls.io/repos/github/kondrak/disasm6502/badge.svg?branch=master)](https://coveralls.io/github/kondrak/disasm6502?branch=master)
![](https://img.shields.io/crates/l/json.svg)

A crate providing functionality to disassemble 6502 binary code. Supports decoding of forbidden instructions, provides information about cycle count, which registers the instruction accesses and which status flags are affected. Acceptable data input can be either an array of bytes, a vector of bytes or a binary file.

[Documentation](https://docs.rs/disasm6502)

Usage
-----
```toml
# Cargo.toml
[dependencies]
disasm6502 = "0.2"
```

Example
-------
```rust
extern crate disasm6502;

fn main()
{
    let bytes = vec![0x05, 0x0B, 0x6C, 0x01, 0x02,
                     0x0A, 0xA2, 0xFF, 0x20, 0x02,
                     0xFD, 0x78, 0xD0, 0xFC, 0x1D,
                     0x05, 0x1E, 0x04, 0x15, 0x02,
                     0x96, 0xAB, 0x58, 0x61, 0x01,
                     0x91, 0xFB];

    // disassemble...
    let instructions = disasm6502::from_array(&bytes).unwrap();

    // ...and print!
    for i in instructions.iter() {
        println!("{}", i);
    }
}
```

Build instructions
------------------

```
cargo build
cargo run --example disasm6502
```

This will run the [example](https://github.com/kondrak/disasm6502/blob/master/examples/disasm6502.rs) which produces the following output:

```asm
$0000: 05 0B    ORA $0B           (3)  Reads:[A]    Writes:[A]    Affects:[NZ]
$0002: 6C 01 02 JMP ($0201)       (5)                             
$0005: 0A       ASL A             (2)  Reads:[A]    Writes:[A]    Affects:[NZC]
$0006: A2 FF    LDX #$FF          (2)               Writes:[X]    Affects:[NZ]
$0008: 20 02 FD JSR $FD02         (6)                             
$000B: 78       SEI               (2)                             Affects:[I]
$000C: D0 FC    BNE $000A         (*4)                            
$000E: 1D 05 1E ORA $1E05,X       (*5) Reads:[AX]   Writes:[A]    Affects:[NZ]
$0011: 04 15    NOP $15       ??? (3)                             
$0013: 02       HLT           ??? (1)                             
$0014: 96 AB    STX $AB,Y         (4)  Reads:[XY]                 
$0016: 58       CLI               (2)                             Affects:[I]
$0017: 61 01    ADC ($01,X)       (6)  Reads:[AX]   Writes:[A]    Affects:[NVZC]
$0019: 91 FB    STA ($FB),Y       (6)  Reads:[AY]                 
$001B: .END
```

## License

Licensed under either of

 * Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)
