//! Decoded 6502 instruction.
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
use self::CPURegister::*;
use self::OpCode::*;
use self::AddrMode::*;

pub type RegVec = Option<Vec<CPURegister>>;

// Some() vector
macro_rules! sv {
    ( $( $x:expr ),* ) => {{
        let mut temp_vec = Vec::new();
        $(temp_vec.push($x);)*
            Some(temp_vec)
    }};
}

/// 6502 addressing modes.
pub enum AddrMode {
    Implied,
    Accumulator,
    Immediate,
    Absolute,
    /// extra cycle on page boundary cross?
    AbsoluteIndexedX(bool),
    /// extra cycle on page boundary cross?
    AbsoluteIndexedY(bool),
    Zeropage,
    ZeropageIndexedX,
    ZeropageIndexedY,
    Relative,
    Indirect,
    IndexedIndirectX,
    /// extra cycleon page boundary cross?
    IndirectIndexedY(bool)
}

/// 6502 CPU registers.
pub enum CPURegister {
    A, X, Y
}

impl fmt::Display for CPURegister {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let reg_name = match *self {
            A => "A", X => "X", Y => "Y"
        };

        write!(f, "{}", reg_name)
    }
}

/// 6502 opcodes (with associated hex value).
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
    /// Fetch opcode's hex value.
    pub fn to_hex(&self) -> u8 {
        match *self {
            LDA(o) => o, LDX(o) => o, LDY(o) => o, STA(o) => o,
            STX(o) => o, STY(o) => o, TAX(o) => o, TAY(o) => o,
            TXA(o) => o, TYA(o) => o, TSX(o) => o, TXS(o) => o,
            PHA(o) => o, PHP(o) => o, PLA(o) => o, PLP(o) => o,
            AND(o) => o, EOR(o) => o, ORA(o) => o, BIT(o) => o,
            ADC(o) => o, SBC(o) => o, CMP(o) => o, CPX(o) => o,
            CPY(o) => o, INC(o) => o, INX(o) => o, INY(o) => o,
            DEC(o) => o, DEX(o) => o, DEY(o) => o, ASL(o) => o,
            LSR(o) => o, ROL(o) => o, ROR(o) => o, JMP(o) => o,
            JSR(o) => o, RTS(o) => o, BCC(o) => o, BCS(o) => o,
            BEQ(o) => o, BMI(o) => o, BNE(o) => o, BPL(o) => o,
            BVC(o) => o, BVS(o) => o, CLC(o) => o, CLD(o) => o,
            CLI(o) => o, CLV(o) => o, SEC(o) => o, SED(o) => o,
            SEI(o) => o, BRK(o) => o, NOP(o) => o, RTI(o) => o,
            HLT(o) => o, SLO(o) => o, ANC(o) => o, RLA(o) => o,
            SRE(o) => o, RRA(o) => o, ALR(o) => o, SAX(o) => o,
            XAA(o) => o, AHX(o) => o, TAS(o) => o, SHY(o) => o,
            SHX(o) => o, ARR(o) => o, LAX(o) => o, LAS(o) => o,
            DCP(o) => o, AXS(o) => o, ISC(o) => o
        }
    }
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op_name = match *self {
            LDA(_) => "LDA", LDX(_) => "LDX", LDY(_) => "LDY", STA(_) => "STA",
            STX(_) => "STX", STY(_) => "STY", TAX(_) => "TAX", TAY(_) => "TAY",
            TXA(_) => "TXA", TYA(_) => "TYA", TSX(_) => "TSX", TXS(_) => "TXS",
            PHA(_) => "PHA", PHP(_) => "PHP", PLA(_) => "PLA", PLP(_) => "PLP",
            AND(_) => "AND", EOR(_) => "EOR", ORA(_) => "ORA", BIT(_) => "BIT",
            ADC(_) => "ADC", SBC(_) => "SBC", CMP(_) => "CMP", CPX(_) => "CPX",
            CPY(_) => "CPY", INC(_) => "INC", INX(_) => "INX", INY(_) => "INY",
            DEC(_) => "DEC", DEX(_) => "DEX", DEY(_) => "DEY", ASL(_) => "ASL",
            LSR(_) => "LSR", ROL(_) => "ROL", ROR(_) => "ROR", JMP(_) => "JMP",
            JSR(_) => "JSR", RTS(_) => "RTS", BCC(_) => "BCC", BCS(_) => "BCS",
            BEQ(_) => "BEQ", BMI(_) => "BMI", BNE(_) => "BNE", BPL(_) => "BPL",
            BVC(_) => "BVC", BVS(_) => "BVS", CLC(_) => "CLC", CLD(_) => "CLD",
            CLI(_) => "CLI", CLV(_) => "CLV", SEC(_) => "SEC", SED(_) => "SED",
            SEI(_) => "SEI", BRK(_) => "BRK", NOP(_) => "NOP", RTI(_) => "RTI",
            HLT(_) => "HLT", SLO(_) => "SLO", ANC(_) => "ANC", RLA(_) => "RLA",
            SRE(_) => "SRE", RRA(_) => "RRA", ALR(_) => "ALR", SAX(_) => "SAX",
            XAA(_) => "XAA", AHX(_) => "AHX", TAS(_) => "TAS", SHY(_) => "SHY",
            SHX(_) => "SHX", ARR(_) => "ARR", LAX(_) => "LAX", LAS(_) => "LAS",
            DCP(_) => "DCP", AXS(_) => "AXS", ISC(_) => "ISC",
        };
        
        write!(f, "{}", op_name)
    }
}

/// Decoded 6502 instruction.
pub struct Instruction {
    /// Instruction opcode.
    pub opcode: OpCode,
    /// Cycle count for the instruction.
    pub cycles: u8,
    /// Instruction addressing mode.
    pub addr_mode: AddrMode,
    /// Address of the instruction in memory buffer.
    pub address: u16,
    /// Optional instruction operand.
    pub operand: Option<u16>,
    /// Instruction may take an extra cycle if zero page boundary is crossed.
    pub extra_cycle: bool,
    /// Registers read by this instruction (optional).
    pub registers_read: RegVec,
    /// Registers written by this instruction (optional).
    pub registers_written: RegVec
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
            extra_cycle: false,
            operand: None,
            registers_read: None,
            registers_written: None
        }
    }

    /// Convert instruction to fixed length string of hex values (opcode + operand, if applicable).
    pub fn as_hex_str(&self) -> String {
        let (oper_hi, oper_lo) = if let Some(v) = self.operand {
            ((v >> 8) & 0xFF, v & 0xFF)
        } else {
            (0, 0)
        };
        
        let operand_hex = match self.addr_mode {
            Implied     => format!("      "),
            Accumulator => format!("      "),
            Immediate   => format!(" {:02X}   ", oper_lo),
            Absolute    => format!(" {:02X} {:02X}", oper_lo, oper_hi),
            AbsoluteIndexedX(_) => format!(" {:02X} {:02X}", oper_lo, oper_hi),
            AbsoluteIndexedY(_) => format!(" {:02X} {:02X}", oper_lo, oper_hi),
            Zeropage => format!(" {:02X}   ", oper_lo),
            ZeropageIndexedX => format!(" {:02X}   ", oper_lo),
            ZeropageIndexedY => format!(" {:02X}   ", oper_lo),
            Relative => format!(" {:02X}   ", oper_lo),
            Indirect => format!(" {:02X} {:02X}", oper_lo, oper_hi),
            IndexedIndirectX    => format!(" {:02X}   ", oper_lo),
            IndirectIndexedY(_) => format!(" {:02X}   ", oper_lo)
        };

        format!("{:02X}{}", self.opcode.to_hex(), operand_hex)
    }

    /// Convert instruction to assembler mnemonic
    pub fn as_str(&self) -> String {
        let operand = if let Some(v) = self.operand { v } else { 0 };
        
        let operand_str = match self.addr_mode {
            Implied     => format!(""),
            Accumulator => format!("A"),
            Immediate   => format!("#${:02X}", operand),
            Absolute    => format!("${:04X}", operand),
            AbsoluteIndexedX(_) => format!("${:04X},X", operand),
            AbsoluteIndexedY(_) => format!("${:04X},Y", operand),
            Zeropage => format!("${:02X}", operand),
            ZeropageIndexedX => format!("${:02X},X", operand),
            ZeropageIndexedY => format!("${:02X},Y", operand),
            // TODO: check wrapping?
            Relative => format!("${:04X}", (self.address as i16 + (2 + operand as i8) as i16) as u16),
            Indirect => format!("(${:04X})", operand),
            IndexedIndirectX    => format!("(${:02X},X)", operand),
            IndirectIndexedY(_) => format!("(${:02X}),Y", operand)
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

fn fetch_operand(addr_mode: &AddrMode, index: &mut usize, buffer: &[u8]) -> (Option<u16>, bool) {
    *index += 1;

    let mut extra_cycle = false;
    let operand = match *addr_mode {
        Absolute => Some(read_word_le(index, buffer)),
        AbsoluteIndexedX(ec) => { extra_cycle = ec; Some(read_word_le(index, buffer)) },
        AbsoluteIndexedY(ec) => { extra_cycle = ec; Some(read_word_le(index, buffer)) },
        Zeropage => Some(buffer[*index] as u16),
        ZeropageIndexedX => Some(buffer[*index] as u16),
        ZeropageIndexedY => Some(buffer[*index] as u16),
        Relative  => { extra_cycle = true; Some(buffer[*index] as u16) },
        Immediate => Some(buffer[*index] as u16),
        Indirect  => Some(read_word_le(index, buffer)),
        IndexedIndirectX     => Some(buffer[*index] as u16),
        IndirectIndexedY(ec) => {extra_cycle = ec; Some(buffer[*index] as u16) },
        _ => None
    };

    // move the buffer index past fetched operand (if it exists!)
    if let Some(_) = operand {
        *index += 1;
    }

    (operand, extra_cycle)
}

fn fetch(opcode: OpCode, num_cycles: u8, addr_mode: AddrMode, data: (u16, &mut usize, &[u8]), reg_read: RegVec, reg_written: RegVec) -> Instruction {
    let (operand, extra_cycle) = fetch_operand(&addr_mode, data.1, data.2);

    let mut instruction = Instruction::new(opcode, data.0, num_cycles, addr_mode);
    instruction.operand = operand;
    instruction.extra_cycle = extra_cycle;
    instruction.registers_read = reg_read;
    instruction.registers_written = reg_written;
    instruction
}

/// Create instruction for given index in memory buffer using a start address
pub fn decode(address: u16, index: &mut usize, memory: &[u8]) -> Instruction {
    let op = memory[*index];

    // use a tuple for less obfuscated code
    let data = (address, index, memory);
    match op {
        0x05 => fetch(ORA(op), 3, Zeropage, data, sv![A], sv![A]),
        0x0A => fetch(ASL(op), 2, Accumulator, data, sv![A], sv![A]),
        0x15 => fetch(ORA(op), 4, ZeropageIndexedX, data, sv![X], sv![A]),
        0x20 => fetch(JSR(op), 6, Absolute, data, None, None),
        0x78 => fetch(SEI(op), 2, Implied, data, None, None),
        0xA2 => fetch(LDX(op), 2, Immediate, data, None, sv![X]),
        0x1D => fetch(ORA(op), 5, AbsoluteIndexedX(true), data, sv![X], sv![A]),
        0x1E => fetch(ASL(op), 7, AbsoluteIndexedX(false), data, sv![X], sv![A]),
        0xD0 => fetch(BNE(op), 4, Relative, data, None, None),
        0x61 => fetch(ADC(op), 6, IndexedIndirectX, data, sv![X], sv![A]),
        0x6C => fetch(JMP(op), 5, Indirect, data, None, None),
        0x91 => fetch(STA(op), 6, IndirectIndexedY(false), data, sv![A], None),
        0x96 => fetch(STX(op), 4, ZeropageIndexedY, data, sv![X, Y], None),
        _ =>  fetch(NOP(op), 7, Implied, data, None, sv![X]),
    }
}
