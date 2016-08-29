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
    LDA, LDX, LDY, STA, STX, STY,
    // Register transfers
    TAX, TAY, TXA, TYA,
    // Stack operations
    TSX, TXS, PHA, PHP, PLA, PLP,
    // Logical
    AND, EOR, ORA, BIT,
    // Arithmetic
    ADC, SBC, CMP, CPX, CPY,
    // Inc/Dec
    INC, INX, INY, DEC, DEX, DEY,
    // Shifts
    ASL, LSR, ROL, ROR,
    // Jump calls
    JMP, JSR, RTS,
    // Branches
    BCC, BCS, BEQ, BMI, BNE, BPL, BVC, BVS,
    // Status flag changes
    CLC, CLD, CLI, CLV, SEC, SED, SEI,
    // System functions
    BRK, NOP, RTI,
    // forbidden/undocumented
    HLT, SLO, ANC, RLA, SRE, RRA, ALR,
    SAX, XAA, AHX, TAS, SHY, SHX, ARR,
    LAX, LAS, DCP, AXS, ISC
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op_name = match *self {
            OpCode::LDA => "LDA", OpCode::LDX => "LDX", OpCode::LDY => "LDY", OpCode::STA => "STA",
            OpCode::STX => "STX", OpCode::STY => "STY", OpCode::TAX => "TAX", OpCode::TAY => "TAY",
            OpCode::TXA => "TXA", OpCode::TYA => "TYA", OpCode::TSX => "TSX", OpCode::TXS => "TXS",
            OpCode::PHA => "PHA", OpCode::PHP => "PHP", OpCode::PLA => "PLA", OpCode::PLP => "PLP",
            OpCode::AND => "AND", OpCode::EOR => "EOR", OpCode::ORA => "ORA", OpCode::BIT => "BIT",
            OpCode::ADC => "ADC", OpCode::SBC => "SBC", OpCode::CMP => "CMP", OpCode::CPX => "CPX",
            OpCode::CPY => "CPY", OpCode::INC => "INC", OpCode::INX => "INX", OpCode::INY => "INY",
            OpCode::DEC => "DEC", OpCode::DEX => "DEX", OpCode::DEY => "DEY", OpCode::ASL => "ASL",
            OpCode::LSR => "LSR", OpCode::ROL => "ROL", OpCode::ROR => "ROR", OpCode::JMP => "JMP",
            OpCode::JSR => "JSR", OpCode::RTS => "RTS", OpCode::BCC => "BCC", OpCode::BCS => "BCS",
            OpCode::BEQ => "BEQ", OpCode::BMI => "BMI", OpCode::BNE => "BNE", OpCode::BPL => "BPL",
            OpCode::BVC => "BVC", OpCode::BVS => "BVS", OpCode::CLC => "CLC", OpCode::CLD => "CLD",
            OpCode::CLI => "CLI", OpCode::CLV => "CLV", OpCode::SEC => "SEC", OpCode::SED => "SED",
            OpCode::SEI => "SEI", OpCode::BRK => "BRK", OpCode::NOP => "NOP", OpCode::RTI => "RTI",
            OpCode::HLT => "HLT", OpCode::SLO => "SLO", OpCode::ANC => "ANC", OpCode::RLA => "RLA",
            OpCode::SRE => "SRE", OpCode::RRA => "RRA", OpCode::ALR => "ALR", OpCode::SAX => "SAX",
            OpCode::XAA => "XAA", OpCode::AHX => "AHX", OpCode::TAS => "TAS", OpCode::SHY => "SHY",
            OpCode::SHX => "SHX", OpCode::ARR => "ARR", OpCode::LAX => "LAX", OpCode::LAS => "LAS",
            OpCode::DCP => "DCP", OpCode::AXS => "AXS", OpCode::ISC => "ISC",
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
        let op_value = if let Some(v) = self.operand {
            v
        } else {
            0
        };
        
        let operand = match self.addr_mode {
            AddrMode::Implied => format!(""),
            AddrMode::Accumulator => format!("A"),
            AddrMode::Immediate => format!("#${:02X}", op_value),
            AddrMode::Absolute => format!("${:04X}  ", op_value),
            AddrMode::AbsoluteIndexedX(_) => format!("${:04X},X", op_value),
            AddrMode::AbsoluteIndexedY(_) => format!("${:04X},Y", op_value),
            AddrMode::Zeropage => format!("${:02X}", op_value),
            AddrMode::ZeropageIndexedX => format!("${:02X},X", op_value),
            AddrMode::ZeropageIndexedY => format!("${:02X},Y", op_value),
            // TODO: check wrapping?
            AddrMode::Relative => format!("${:04X}", (self.address as i16 + (2 + op_value as i8) as i16) as u16),
            AddrMode::Indirect => format!("(${:04X})", op_value),
            AddrMode::IndexedIndirectX => format!("(${:02X},X)", op_value),
            AddrMode::IndirectIndexedY(_) => format!("(${:02X}),Y", op_value)
        };

        write!(f, "${:04X}: {} {}", self.address, self.opcode, operand)
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
}

fn read_word_le(index: &mut usize, buffer: &[u8]) -> u16 {
    let value_be: u16 = ((buffer[*index] as u16) << 8 & 0xFF00) |    
    ((buffer[*index + 0x0001] as u16) & 0x00FF);
    let value_le: u16 = ((value_be << 8) & 0xFF00) | ((value_be >> 8) & 0x00FF);

    *index += 1;
    value_le
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
        AddrMode::Relative => Some(buffer[*index] as u16),
        AddrMode::Immediate => Some(buffer[*index] as u16),
        AddrMode::Indirect => Some(read_word_le(index, buffer)),
        AddrMode::IndexedIndirectX => Some(buffer[*index] as u16),
        AddrMode::IndirectIndexedY(_) => Some(buffer[*index] as u16),
        _ => None
    };

    // move the buffer index past fetched operand (if it exists!)
    if let Some(_) = operand {
        *index += 1;
    }
    
    operand
}

fn fetch(opcode: OpCode, num_cycles: u8, addr_mode: AddrMode, address: u16, index: &mut usize, buffer: &[u8]) -> Instruction {
    // TODO: cycle count should be adjusted here for certain instructions
    let operand = fetch_operand(&addr_mode, index, buffer);
    let mut instruction = Instruction::new(opcode, address, num_cycles, addr_mode);
    instruction.operand = operand;
    instruction
}

pub fn decode(address: u16, index: &mut usize, memory: &[u8]) -> Instruction {
    match memory[*index] {
        0x05 => fetch(OpCode::ORA, 3, AddrMode::Zeropage, address, index, memory),
        0x0A => fetch(OpCode::ASL, 2, AddrMode::Accumulator, address, index, memory),
        0x15 => fetch(OpCode::ORA, 4, AddrMode::ZeropageIndexedX, address, index, memory),
        0x20 => fetch(OpCode::JSR, 6, AddrMode::Absolute, address, index, memory),
        0x78 => fetch(OpCode::SEI, 2, AddrMode::Implied, address, index, memory),
        0xA2 => fetch(OpCode::LDX, 2, AddrMode::Immediate, address, index, memory),
        0x1D => fetch(OpCode::ORA, 5, AddrMode::AbsoluteIndexedX(true), address, index, memory),
        0x1E => fetch(OpCode::ASL, 7, AddrMode::AbsoluteIndexedX(false), address, index, memory),
        0xD0 => fetch(OpCode::BNE, 4, AddrMode::Relative, address, index, memory),
        0x61 => fetch(OpCode::ADC, 6, AddrMode::IndexedIndirectX, address, index, memory),
        0x6C => fetch(OpCode::JMP, 5, AddrMode::Indirect, address, index, memory),
        0x91 => fetch(OpCode::STA, 6, AddrMode::IndirectIndexedY(false), address, index, memory),
        0x96 => fetch(OpCode::STX, 4, AddrMode::ZeropageIndexedY, address, index, memory),
        _ =>  fetch(OpCode::NOP, 7, AddrMode::Implied, address, index, memory),
    }
}
