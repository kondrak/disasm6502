// opcode enumeration suffix: // addressing mode:
// imm = #$00                 // immediate 
// zp = $00                   // zero page
// zpx = $00,X                // zero page with X
// zpy = $00,Y                // zero page with Y
// izx = ($00,X)              // indexed indirect (X)
// izy = ($00),Y              // indirect indexed (Y)
// abs = $0000                // absolute
// abx = $0000,X              // absolute indexed with X
// aby = $0000,Y              // absolute indexed with Y
// ind = ($0000)              // indirect
// rel = $0000                // relative to PC/IP

use std::fmt;

pub enum AddrMode {
    Implied,
    Accumulator,
    Immediate,
    Absolute,
    AbsoluteIndexedX(bool), // extra cycle?
    AbsoluteIndexedY(bool), // extra cycle?
    Zeropage,
    ZeropageIndexedX,
    ZeropageIndexedY,
    Relative,
    Indirect,
    IndexedIndirectX,
    IndirectIndexedY(bool) // extra cycle?
}

pub enum CPURegister {
    A, X, Y
}

impl fmt::Display for CPURegister {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let reg_name = match *self {
            CPURegister::A => "A",
            CPURegister::X => "X",
            CPURegister::Y => "Y"
        };

        write!(f, "{}", reg_name)
    }
}

pub enum OpCode {
    // Load/store
    LDA(u8), LDX(u8), LDY(u8), STA(u8), STX(u8), STY(u8),
    // Register transfers
    TAX(u8), TAY(u8), TXA(u8), TYA(u8),
    // Stack operations
    TSX(u8), TXS(u8), PHA(u8), PHP(u8), PLA(u8), PLP(u8),
    // Logical
    AND(u8), EOR(u8), ORA(u8), BIT(u8),
    // Arithmetic
    ADC(u8), SBC(u8), CMP(u8), CPX(u8), CPY(u8),
    // Inc/Dec
    INC(u8), INX(u8), INY(u8), DEC(u8), DEX(u8), DEY(u8),
    // Shifts
    ASL(u8), LSR(u8), ROL(u8), ROR(u8),
    // Jump calls
    JMP(u8), JSR(u8), RTS(u8),
    // Branches
    BCC(u8), BCS(u8), BEQ(u8), BMI(u8), BNE(u8), BPL(u8), BVC(u8), BVS(u8),
    // Status flag changes
    CLC(u8), CLD(u8), CLI(u8), CLV(u8), SEC(u8), SED(u8), SEI(u8),
    // System functions
    BRK(u8), NOP(u8), RTI(u8),
    // forbidden/undocumented
    HLT(u8), SLO(u8), ANC(u8), RLA(u8), SRE(u8), RRA(u8), ALR(u8),
    SAX(u8), XAA(u8), AHX(u8), TAS(u8), SHY(u8), SHX(u8), ARR(u8),
    LAX(u8), LAS(u8), DCP(u8), AXS(u8), ISC(u8)
}

impl OpCode {
    pub fn to_hex(&self) -> u8 {
        match *self {
            OpCode::LDA(o) => o, OpCode::LDX(o) => o, OpCode::LDY(o) => o, OpCode::STA(o) => o,
            OpCode::STX(o) => o, OpCode::STY(o) => o, OpCode::TAX(o) => o, OpCode::TAY(o) => o,
            OpCode::TXA(o) => o, OpCode::TYA(o) => o, OpCode::TSX(o) => o, OpCode::TXS(o) => o,
            OpCode::PHA(o) => o, OpCode::PHP(o) => o, OpCode::PLA(o) => o, OpCode::PLP(o) => o,
            OpCode::AND(o) => o, OpCode::EOR(o) => o, OpCode::ORA(o) => o, OpCode::BIT(o) => o,
            OpCode::ADC(o) => o, OpCode::SBC(o) => o, OpCode::CMP(o) => o, OpCode::CPX(o) => o,
            OpCode::CPY(o) => o, OpCode::INC(o) => o, OpCode::INX(o) => o, OpCode::INY(o) => o,
            OpCode::DEC(o) => o, OpCode::DEX(o) => o, OpCode::DEY(o) => o, OpCode::ASL(o) => o,
            OpCode::LSR(o) => o, OpCode::ROL(o) => o, OpCode::ROR(o) => o, OpCode::JMP(o) => o,
            OpCode::JSR(o) => o, OpCode::RTS(o) => o, OpCode::BCC(o) => o, OpCode::BCS(o) => o,
            OpCode::BEQ(o) => o, OpCode::BMI(o) => o, OpCode::BNE(o) => o, OpCode::BPL(o) => o,
            OpCode::BVC(o) => o, OpCode::BVS(o) => o, OpCode::CLC(o) => o, OpCode::CLD(o) => o,
            OpCode::CLI(o) => o, OpCode::CLV(o) => o, OpCode::SEC(o) => o, OpCode::SED(o) => o,
            OpCode::SEI(o) => o, OpCode::BRK(o) => o, OpCode::NOP(o) => o, OpCode::RTI(o) => o,
            OpCode::HLT(o) => o, OpCode::SLO(o) => o, OpCode::ANC(o) => o, OpCode::RLA(o) => o,
            OpCode::SRE(o) => o, OpCode::RRA(o) => o, OpCode::ALR(o) => o, OpCode::SAX(o) => o,
            OpCode::XAA(o) => o, OpCode::AHX(o) => o, OpCode::TAS(o) => o, OpCode::SHY(o) => o,
            OpCode::SHX(o) => o, OpCode::ARR(o) => o, OpCode::LAX(o) => o, OpCode::LAS(o) => o,
            OpCode::DCP(o) => o, OpCode::AXS(o) => o, OpCode::ISC(o) => o
        }
    }
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op_name = match *self {
            OpCode::LDA(_) => "LDA", OpCode::LDX(_) => "LDX", OpCode::LDY(_) => "LDY", OpCode::STA(_) => "STA",
            OpCode::STX(_) => "STX", OpCode::STY(_) => "STY", OpCode::TAX(_) => "TAX", OpCode::TAY(_) => "TAY",
            OpCode::TXA(_) => "TXA", OpCode::TYA(_) => "TYA", OpCode::TSX(_) => "TSX", OpCode::TXS(_) => "TXS",
            OpCode::PHA(_) => "PHA", OpCode::PHP(_) => "PHP", OpCode::PLA(_) => "PLA", OpCode::PLP(_) => "PLP",
            OpCode::AND(_) => "AND", OpCode::EOR(_) => "EOR", OpCode::ORA(_) => "ORA", OpCode::BIT(_) => "BIT",
            OpCode::ADC(_) => "ADC", OpCode::SBC(_) => "SBC", OpCode::CMP(_) => "CMP", OpCode::CPX(_) => "CPX",
            OpCode::CPY(_) => "CPY", OpCode::INC(_) => "INC", OpCode::INX(_) => "INX", OpCode::INY(_) => "INY",
            OpCode::DEC(_) => "DEC", OpCode::DEX(_) => "DEX", OpCode::DEY(_) => "DEY", OpCode::ASL(_) => "ASL",
            OpCode::LSR(_) => "LSR", OpCode::ROL(_) => "ROL", OpCode::ROR(_) => "ROR", OpCode::JMP(_) => "JMP",
            OpCode::JSR(_) => "JSR", OpCode::RTS(_) => "RTS", OpCode::BCC(_) => "BCC", OpCode::BCS(_) => "BCS",
            OpCode::BEQ(_) => "BEQ", OpCode::BMI(_) => "BMI", OpCode::BNE(_) => "BNE", OpCode::BPL(_) => "BPL",
            OpCode::BVC(_) => "BVC", OpCode::BVS(_) => "BVS", OpCode::CLC(_) => "CLC", OpCode::CLD(_) => "CLD",
            OpCode::CLI(_) => "CLI", OpCode::CLV(_) => "CLV", OpCode::SEC(_) => "SEC", OpCode::SED(_) => "SED",
            OpCode::SEI(_) => "SEI", OpCode::BRK(_) => "BRK", OpCode::NOP(_) => "NOP", OpCode::RTI(_) => "RTI",
            OpCode::HLT(_) => "HLT", OpCode::SLO(_) => "SLO", OpCode::ANC(_) => "ANC", OpCode::RLA(_) => "RLA",
            OpCode::SRE(_) => "SRE", OpCode::RRA(_) => "RRA", OpCode::ALR(_) => "ALR", OpCode::SAX(_) => "SAX",
            OpCode::XAA(_) => "XAA", OpCode::AHX(_) => "AHX", OpCode::TAS(_) => "TAS", OpCode::SHY(_) => "SHY",
            OpCode::SHX(_) => "SHX", OpCode::ARR(_) => "ARR", OpCode::LAX(_) => "LAX", OpCode::LAS(_) => "LAS",
            OpCode::DCP(_) => "DCP", OpCode::AXS(_) => "AXS", OpCode::ISC(_) => "ISC",
        };
        
        write!(f, "{}", op_name)
    }
}

pub struct Instruction {
    pub opcode: OpCode,
    pub cycles: u8, // how many cycles to execute the operation?
    pub addr_mode: AddrMode,
    pub address: u16,
    pub operand: Option<u16>,
    pub registers_read: Option<Vec<CPURegister>>, // registers read by this instruction
    pub registers_written: Option<Vec<CPURegister>> // registers written by this instruction
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "${:04X}: {} {}", self.address, self.as_hex_str(), self.as_str())
    }
}

impl Instruction {
    fn new(opcode: OpCode, address: u16, cycles: u8, addr_mode: AddrMode) -> Instruction {
        Instruction {
            opcode: opcode,
            cycles: cycles,
            addr_mode: addr_mode,
            address: address,
            operand: None,
            registers_read: None,
            registers_written: None
        }
    }

    pub fn as_hex_str(&self) -> String {
        let (oper_hi, oper_lo) = if let Some(v) = self.operand {
            ((v >> 8) & 0xFF, v & 0xFF)
        } else {
            (0, 0)
        };
        
        let operand_hex = match self.addr_mode {
            AddrMode::Implied     => format!("      "),
            AddrMode::Accumulator => format!("      "),
            AddrMode::Immediate   => format!(" {:02X}   ", oper_lo),
            AddrMode::Absolute    => format!(" {:02X} {:02X}", oper_lo, oper_hi),
            AddrMode::AbsoluteIndexedX(_) => format!(" {:02X} {:02X}", oper_lo, oper_hi),
            AddrMode::AbsoluteIndexedY(_) => format!(" {:02X} {:02X}", oper_lo, oper_hi),
            AddrMode::Zeropage => format!(" {:02X}   ", oper_lo),
            AddrMode::ZeropageIndexedX => format!(" {:02X}   ", oper_lo),
            AddrMode::ZeropageIndexedY => format!(" {:02X}   ", oper_lo),
            AddrMode::Relative => format!(" {:02X}   ", oper_lo),
            AddrMode::Indirect => format!(" {:02X} {:02X}", oper_lo, oper_hi),
            AddrMode::IndexedIndirectX    => format!(" {:02X}   ", oper_lo),
            AddrMode::IndirectIndexedY(_) => format!(" {:02X}   ", oper_lo)
        };

        format!("{:02X}{}", self.opcode.to_hex(), operand_hex)
    }

    pub fn as_str(&self) -> String {
        let operand = if let Some(v) = self.operand { v } else { 0 };
        
        let operand_str = match self.addr_mode {
            AddrMode::Implied     => format!(""),
            AddrMode::Accumulator => format!("A"),
            AddrMode::Immediate   => format!("#${:02X}", operand),
            AddrMode::Absolute    => format!("${:04X}", operand),
            AddrMode::AbsoluteIndexedX(_) => format!("${:04X},X", operand),
            AddrMode::AbsoluteIndexedY(_) => format!("${:04X},Y", operand),
            AddrMode::Zeropage => format!("${:02X}", operand),
            AddrMode::ZeropageIndexedX => format!("${:02X},X", operand),
            AddrMode::ZeropageIndexedY => format!("${:02X},Y", operand),
            // TODO: check wrapping?
            AddrMode::Relative => format!("${:04X}", (self.address as i16 + (2 + operand as i8) as i16) as u16),
            AddrMode::Indirect => format!("(${:04X})", operand),
            AddrMode::IndexedIndirectX    => format!("(${:02X},X)", operand),
            AddrMode::IndirectIndexedY(_) => format!("(${:02X}),Y", operand)
        };

        format!("{} {}", self.opcode, operand_str)
    }
}

// read word: Little Endian
fn read_word_le(index: &mut usize, buffer: &[u8]) -> u16 {
    let value_be = ((buffer[*index] as u16) << 8 & 0xFF00) | ((buffer[*index + 0x0001] as u16) & 0x00FF);
    *index += 1;

    ((value_be << 8) & 0xFF00) | ((value_be >> 8) & 0x00FF)
}

fn fetch_operand(addr_mode: &AddrMode, index: &mut usize, buffer: &[u8]) -> Option<u16> {
    *index += 1;

    let operand = match *addr_mode {
        AddrMode::Absolute => Some(read_word_le(index, buffer)),
        AddrMode::AbsoluteIndexedX(_) => Some(read_word_le(index, buffer)),
        AddrMode::AbsoluteIndexedY(_) => Some(read_word_le(index, buffer)),
        AddrMode::Zeropage => Some(buffer[*index] as u16),
        AddrMode::ZeropageIndexedX => Some(buffer[*index] as u16),
        AddrMode::ZeropageIndexedY => Some(buffer[*index] as u16),
        AddrMode::Relative  => Some(buffer[*index] as u16),
        AddrMode::Immediate => Some(buffer[*index] as u16),
        AddrMode::Indirect  => Some(read_word_le(index, buffer)),
        AddrMode::IndexedIndirectX    => Some(buffer[*index] as u16),
        AddrMode::IndirectIndexedY(_) => Some(buffer[*index] as u16),
        _ => None
    };

    // move the buffer index past fetched operand (if it exists!)
    if let Some(_) = operand {
        *index += 1;
    }
    
    operand
}

fn fetch(opcode: OpCode, num_cycles: u8, addr_mode: AddrMode, data: (u16, &mut usize, &[u8])) -> Instruction {
    // TODO: cycle count should be adjusted here for certain instructions
    let operand = fetch_operand(&addr_mode, data.1, data.2);
    let mut instruction = Instruction::new(opcode, data.0, num_cycles, addr_mode);
    instruction.operand = operand;
    instruction
}

pub fn decode(address: u16, index: &mut usize, memory: &[u8]) -> Instruction {
    let op = memory[*index];

    // use a tuple for less obfuscated code
    let data = (address, index, memory);
    match op {
        0x05 => fetch(OpCode::ORA(op), 3, AddrMode::Zeropage, data),
        0x0A => fetch(OpCode::ASL(op), 2, AddrMode::Accumulator, data),
        0x15 => fetch(OpCode::ORA(op), 4, AddrMode::ZeropageIndexedX, data),
        0x20 => fetch(OpCode::JSR(op), 6, AddrMode::Absolute, data),
        0x78 => fetch(OpCode::SEI(op), 2, AddrMode::Implied, data),
        0xA2 => fetch(OpCode::LDX(op), 2, AddrMode::Immediate, data),
        0x1D => fetch(OpCode::ORA(op), 5, AddrMode::AbsoluteIndexedX(true), data),
        0x1E => fetch(OpCode::ASL(op), 7, AddrMode::AbsoluteIndexedX(false), data),
        0xD0 => fetch(OpCode::BNE(op), 4, AddrMode::Relative, data),
        0x61 => fetch(OpCode::ADC(op), 6, AddrMode::IndexedIndirectX, data),
        0x6C => fetch(OpCode::JMP(op), 5, AddrMode::Indirect, data),
        0x91 => fetch(OpCode::STA(op), 6, AddrMode::IndirectIndexedY(false), data),
        0x96 => fetch(OpCode::STX(op), 4, AddrMode::ZeropageIndexedY, data),
        _ =>  fetch(OpCode::NOP(op), 7, AddrMode::Implied, data),
    }
}
