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
    /// bool - extra cycle on page boundary cross?
    AbsoluteIndexedX(bool),
    /// bool - extra cycle on page boundary cross?
    AbsoluteIndexedY(bool),
    Zeropage,
    ZeropageIndexedX,
    ZeropageIndexedY,
    Relative,
    Indirect,
    IndexedIndirectX,
    /// bool - extra cycleon page boundary cross?
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
    /// instruction opcode
    pub opcode: OpCode,
    /// cycle count for the instruction
    pub cycles: u8,
    /// instruction addressing mode
    pub addr_mode: AddrMode,
    /// address of the instruction in memory buffer
    pub address: u16,
    /// optional instruction operand
    pub operand: Option<u16>,
    /// instruction may take an extra cycle if zero page boundary is crossed
    pub extra_cycle: bool,
    /// registers read by this instruction (optional)
    pub registers_read: RegVec,
    /// registers written by this instruction (optional)
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
    ///
    /// # Examples
    ///
    /// ```
    /// extern crate disasm6502;
    ///
    /// let memory = vec![0x05, 0x0B, 0x6C, 0x01, 0x02];
    ///
    /// // set program counter to 0 - will decode first instruction
    /// let mut pc: usize = 0;
    ///
    /// // interprets 0x05 as an instruction, places it at $0800
    /// let instruction = disasm6502::instruction::decode(0x0800, &mut pc, &memory);
    ///
    /// // prints: "0x05 0x0B   " (instruction + operand value)
    /// println!("{}", instruction.as_hex_str());
    /// ```
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

    /// Convert instruction to assembler mnemonic.
    ///
    /// # Examples
    ///
    /// ```
    /// extern crate disasm6502;
    ///
    /// let memory = vec![0x05, 0x0B, 0x6C, 0x01, 0x02];
    ///
    /// // set program counter to 0 - will decode first instruction
    /// let mut pc: usize = 0;
    ///
    /// // interprets 0x05 as an instruction, places it at $0800
    /// let instruction = disasm6502::instruction::decode(0x0800, &mut pc, &memory);
    ///
    /// // prints: "ORA $0B"
    /// println!("{}", instruction.as_str());
    /// ```
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

/// Create instruction for given index/program counter in memory buffer and place it at specified address.
///
/// # Examples
///
/// ```
/// extern crate disasm6502;
///
/// let memory = vec![0x05, 0x0B, 0x6C, 0x01, 0x02];
///
/// // set program counter to 0 - will decode first instruction
/// let mut pc: usize = 0;
///
/// // interprets 0x05 as an instruction, places it at $0800
/// let instruction = disasm6502::instruction::decode(0x0800, &mut pc, &memory);
/// ```
pub fn decode(address: u16, index: &mut usize, memory: &[u8]) -> Instruction {
    let op = memory[*index];

    // use a tuple for less obfuscated code
    let data = (address, index, memory);
    match op {
        // ** documented instructions **
        /* BRK     */ 0x00 => fetch(BRK(op), 7, Implied, data, None, None),
        /* ORA_izx */ 0x01 => fetch(ORA(op), 6, IndexedIndirectX, data, None, None),
        /* ORA_zp  */ 0x05 => fetch(ORA(op), 3, Zeropage, data, None, None),
        /* ASL_zp  */ 0x06 => fetch(ASL(op), 5, Zeropage, data, None, None), 
        /* PHP     */ 0x08 => fetch(PHP(op), 3, Implied, data, None, None),
        /* ORA_imm */ 0x09 => fetch(ORA(op), 2, Immediate, data, None, None),
        /* ASL     */ 0x0A => fetch(ASL(op), 2, Accumulator, data, None, None),
        /* ORA_abs */ 0x0D => fetch(ORA(op), 4, Absolute, data, None, None),
        /* ASL_abs */ 0x0E => fetch(ASL(op), 6, Absolute, data, None, None),
        /* BPL_rel */ 0x10 => fetch(BPL(op), 4, Relative, data, None, None), // add 1 cycle if page boundary is crossed
        /* ORA_izy */ 0x11 => fetch(ORA(op), 6, IndirectIndexedY(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* ORA_zpx */ 0x15 => fetch(ORA(op), 4, ZeropageIndexedX, data, None, None),
        /* ASL_zpx */ 0x16 => fetch(ASL(op), 6, ZeropageIndexedX, data, None, None),
        /* CLC     */ 0x18 => fetch(CLC(op), 2, Implied, data, None, None),
        /* ORA_aby */ 0x19 => fetch(ORA(op), 5, AbsoluteIndexedY(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* ORA_abx */ 0x1D => fetch(ORA(op), 5, AbsoluteIndexedX(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* ASL_abx */ 0x1E => fetch(ASL(op), 7, AbsoluteIndexedX(false), data, None, None),
        /* JSR_abs */ 0x20 => fetch(JSR(op), 6, Absolute, data, None, None),
        /* AND_izx */ 0x21 => fetch(AND(op), 6, IndexedIndirectX, data, None, None),
        /* BIT_zp  */ 0x24 => fetch(BIT(op), 3, Zeropage, data, None, None),
        /* AND_zp  */ 0x25 => fetch(AND(op), 3, Zeropage, data, None, None),
        /* ROL_zp  */ 0x26 => fetch(ROL(op), 5, Zeropage, data, None, None),
        /* PLP     */ 0x28 => fetch(PLP(op), 4, Implied, data, None, None),
        /* AND_imm */ 0x29 => fetch(AND(op), 2, Immediate, data, None, None),
        /* ROL     */ 0x2A => fetch(ROL(op), 2, Accumulator, data, None, None),
        /* BIT_abs */ 0x2C => fetch(BIT(op), 4, Absolute, data, None, None),
        /* AND_abs */ 0x2D => fetch(AND(op), 4, Absolute, data, None, None),
        /* ROL_abs */ 0x2E => fetch(ROL(op), 6, Absolute, data, None, None),
        /* BMI_rel */ 0x30 => fetch(BMI(op), 4, Relative, data, None, None), // add 1 cycle if page boundary is crossed
        /* AND_izy */ 0x31 => fetch(AND(op), 6, IndirectIndexedY(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* AND_zpx */ 0x35 => fetch(AND(op), 4, ZeropageIndexedX, data, None, None),
        /* ROL_zpx */ 0x36 => fetch(ROL(op), 6, ZeropageIndexedX, data, None, None),
        /* SEC     */ 0x38 => fetch(SEC(op), 2, Implied, data, None, None),
        /* AND_aby */ 0x39 => fetch(AND(op), 5, AbsoluteIndexedY(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* AND_abx */ 0x3D => fetch(AND(op), 5, AbsoluteIndexedX(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* ROL_abx */ 0x3E => fetch(ROL(op), 7, AbsoluteIndexedX(false), data, None, None),
        /* RTI     */ 0x40 => fetch(RTI(op), 6, Implied, data, None, None),
        /* EOR_izx */ 0x41 => fetch(EOR(op), 6, IndexedIndirectX, data, None, None),
        /* EOR_zp  */ 0x45 => fetch(EOR(op), 3, Zeropage, data, None, None),
        /* LSR_zp  */ 0x46 => fetch(LSR(op), 5, Zeropage, data, None, None),
        /* PHA     */ 0x48 => fetch(PHA(op), 3, Implied, data, None, None),
        /* EOR_imm */ 0x49 => fetch(EOR(op), 2, Immediate, data, None, None),
        /* LSR     */ 0x4A => fetch(LSR(op), 2, Accumulator, data, None, None),
        /* JMP_abs */ 0x4C => fetch(JMP(op), 3, Absolute, data, None, None),
        /* EOR_abs */ 0x4D => fetch(EOR(op), 4, Absolute, data, None, None),
        /* LSR_abs */ 0x4E => fetch(LSR(op), 6, Absolute, data, None, None),
        /* BVC_rel */ 0x50 => fetch(BVC(op), 4, Relative, data, None, None), // add 1 cycle if page boundary is crossed
        /* EOR_izy */ 0x51 => fetch(EOR(op), 6, IndirectIndexedY(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* EOR_zpx */ 0x55 => fetch(EOR(op), 4, ZeropageIndexedX, data, None, None),
        /* LSR_zpx */ 0x56 => fetch(LSR(op), 6, ZeropageIndexedX, data, None, None),
        /* CLI     */ 0x58 => fetch(CLI(op), 2, Implied, data, None, None),
        /* EOR_aby */ 0x59 => fetch(EOR(op), 5, AbsoluteIndexedY(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* EOR_abx */ 0x5D => fetch(EOR(op), 5, AbsoluteIndexedX(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* LSR_abx */ 0x5E => fetch(LSR(op), 7, AbsoluteIndexedX(false), data, None, None),
        /* RTS     */ 0x60 => fetch(RTS(op), 6, Implied, data, None, None),
        /* ADC_izx */ 0x61 => fetch(ADC(op), 6, IndexedIndirectX, data, None, None),
        /* ADC_zp  */ 0x65 => fetch(ADC(op), 3, Zeropage, data, None, None),
        /* ROR_zp  */ 0x66 => fetch(ROR(op), 5, Zeropage, data, None, None),
        /* PLA     */ 0x68 => fetch(PLA(op), 4, Implied, data, None, None),
        /* ADC_imm */ 0x69 => fetch(ADC(op), 2, Immediate, data, None, None),
        /* ROR     */ 0x6A => fetch(ROR(op), 2, Accumulator, data, None, None),
        /* JMP_ind */ 0x6C => fetch(JMP(op), 5, Indirect, data, None, None),
        /* ADC_abs */ 0x6D => fetch(ADC(op), 4, Absolute, data, None, None),
        /* ROR_abs */ 0x6E => fetch(ROR(op), 6, Absolute, data, None, None),
        /* BVS_rel */ 0x70 => fetch(BVS(op), 4, Relative, data, None, None), // add 1 cycle if page boundary is crossed
        /* ADC_izy */ 0x71 => fetch(ADC(op), 6, IndirectIndexedY(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* ADC_zpx */ 0x75 => fetch(ADC(op), 4, ZeropageIndexedX, data, None, None),
        /* ROR_zpx */ 0x76 => fetch(ROR(op), 6, ZeropageIndexedX, data, None, None),
        /* SEI     */ 0x78 => fetch(SEI(op), 2, Implied, data, None, None),
        /* ADC_aby */ 0x79 => fetch(ADC(op), 5, AbsoluteIndexedY(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* ADC_abx */ 0x7D => fetch(ADC(op), 5, AbsoluteIndexedX(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* ROR_abx */ 0x7E => fetch(ROR(op), 7, AbsoluteIndexedX(false), data, None, None),
        /* STA_izx */ 0x81 => fetch(STA(op), 6, IndexedIndirectX, data, None, None),
        /* STY_zp  */ 0x84 => fetch(STY(op), 3, Zeropage, data, None, None),
        /* STA_zp  */ 0x85 => fetch(STA(op), 3, Zeropage, data, None, None),
        /* STX_zp  */ 0x86 => fetch(STX(op), 3, Zeropage, data, None, None),
        /* DEY     */ 0x88 => fetch(DEY(op), 2, Implied, data, None, None),
        /* TXA     */ 0x8A => fetch(TXA(op), 2, Implied, data, None, None),
        /* STY_abs */ 0x8C => fetch(STY(op), 4, Absolute, data, None, None),
        /* STA_abs */ 0x8D => fetch(STA(op), 4, Absolute, data, None, None),
        /* STX_abs */ 0x8E => fetch(STX(op), 4, Absolute, data, None, None),
        /* BCC_rel */ 0x90 => fetch(BCC(op), 4, Relative, data, None, None), // add 1 cycle if page boundary is crossed
        /* STA_izy */ 0x91 => fetch(STA(op), 6, IndirectIndexedY(false), data, None, None),
        /* STY_zpx */ 0x94 => fetch(STY(op), 4, ZeropageIndexedX, data, None, None),
        /* STA_zpx */ 0x95 => fetch(STA(op), 4, ZeropageIndexedX, data, None, None),
        /* STX_zpy */ 0x96 => fetch(STX(op), 4, ZeropageIndexedY, data, None, None),
        /* TYA     */ 0x98 => fetch(TYA(op), 2, Implied, data, None, None),
        /* STA_aby */ 0x99 => fetch(STA(op), 5, AbsoluteIndexedY(false), data, None, None),
        /* TXS     */ 0x9A => fetch(TXS(op), 2, Implied, data, None, None),
        /* STA_abx */ 0x9D => fetch(STA(op), 5, AbsoluteIndexedX(false), data, None, None),
        /* LDY_imm */ 0xA0 => fetch(LDY(op), 2, Immediate, data, None, None),
        /* LDA_izx */ 0xA1 => fetch(LDA(op), 6, IndexedIndirectX, data, None, None),
        /* LDX_imm */ 0xA2 => fetch(LDX(op), 2, Immediate, data, None, None),
        /* LDY_zp  */ 0xA4 => fetch(LDY(op), 3, Zeropage, data, None, None),
        /* LDA_zp  */ 0xA5 => fetch(LDA(op), 3, Zeropage, data, None, None),
        /* LDX_zp  */ 0xA6 => fetch(LDX(op), 3, Zeropage, data, None, None),
        /* TAY     */ 0xA8 => fetch(TAY(op), 2, Implied, data, None, None),
        /* LDA_imm */ 0xA9 => fetch(LDA(op), 2, Immediate, data, None, None),
        /* TAX     */ 0xAA => fetch(TAX(op), 2, Implied, data, None, None),
        /* LDY_abs */ 0xAC => fetch(LDY(op), 4, Absolute, data, None, None),
        /* LDA_abs */ 0xAD => fetch(LDA(op), 4, Absolute, data, None, None),
        /* LDX_abs */ 0xAE => fetch(LDX(op), 4, Absolute, data, None, None),
        /* BCS_rel */ 0xB0 => fetch(BCS(op), 4, Relative, data, None, None), // add 1 cycle if page boundary is crossed
        /* LDA_izy */ 0xB1 => fetch(LDA(op), 6, IndirectIndexedY(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* LDY_zpx */ 0xB4 => fetch(LDY(op), 4, ZeropageIndexedX, data, None, None),
        /* LDA_zpx */ 0xB5 => fetch(LDA(op), 4, ZeropageIndexedX, data, None, None),
        /* LDX_zpy */ 0xB6 => fetch(LDX(op), 4, ZeropageIndexedY, data, None, None),
        /* CLV     */ 0xB8 => fetch(CLV(op), 2, Implied, data, None, None),
        /* LDA_aby */ 0xB9 => fetch(LDA(op), 5, AbsoluteIndexedY(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* TSX     */ 0xBA => fetch(TSX(op), 2, Implied, data, None, None),
        /* LDY_abx */ 0xBC => fetch(LDY(op), 5, AbsoluteIndexedX(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* LDA_abx */ 0xBD => fetch(LDA(op), 5, AbsoluteIndexedX(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* LDX_aby */ 0xBE => fetch(LDX(op), 5, AbsoluteIndexedY(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* CPY_imm */ 0xC0 => fetch(CPY(op), 2, Immediate, data, None, None),
        /* CMP_izx */ 0xC1 => fetch(CMP(op), 6, IndexedIndirectX, data, None, None),
        /* CPY_zp  */ 0xC4 => fetch(CPY(op), 3, Zeropage, data, None, None),
        /* CMP_zp  */ 0xC5 => fetch(CMP(op), 3, Zeropage, data, None, None),
        /* DEC_zp  */ 0xC6 => fetch(DEC(op), 5, Zeropage, data, None, None),
        /* INY     */ 0xC8 => fetch(INY(op), 2, Implied, data, None, None),
        /* CMP_imm */ 0xC9 => fetch(CMP(op), 2, Immediate, data, None, None),
        /* DEX     */ 0xCA => fetch(DEX(op), 2, Implied, data, None, None),
        /* CPY_abs */ 0xCC => fetch(CPY(op), 4, Absolute, data, None, None),
        /* CMP_abs */ 0xCD => fetch(CMP(op), 4, Absolute, data, None, None),
        /* DEC_abs */ 0xCE => fetch(DEC(op), 6, Absolute, data, None, None),
        /* BNE_rel */ 0xD0 => fetch(BNE(op), 4, Relative, data, None, None), // add 1 cycle if page boundary is crossed
        /* CMP_izy */ 0xD1 => fetch(CMP(op), 6, IndirectIndexedY(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* CMP_zpx */ 0xD5 => fetch(CMP(op), 4, ZeropageIndexedX, data, None, None),
        /* DEC_zpx */ 0xD6 => fetch(DEC(op), 6, ZeropageIndexedX, data, None, None),
        /* CLD     */ 0xD8 => fetch(CLD(op), 2, Implied, data, None, None),
        /* CMP_aby */ 0xD9 => fetch(CMP(op), 5, AbsoluteIndexedY(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* CMP_abx */ 0xDD => fetch(CMP(op), 5, AbsoluteIndexedX(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* DEC_abx */ 0xDE => fetch(DEC(op), 7, AbsoluteIndexedX(false), data, None, None),
        /* CPX_imm */ 0xE0 => fetch(CPX(op), 2, Immediate, data, None, None),
        /* SBC_izx */ 0xE1 => fetch(SBC(op), 6, IndexedIndirectX, data, None, None),
        /* CPX_zp  */ 0xE4 => fetch(CPX(op), 3, Zeropage, data, None, None),
        /* SBC_zp  */ 0xE5 => fetch(SBC(op), 3, Zeropage, data, None, None),
        /* INC_zp  */ 0xE6 => fetch(INC(op), 5, Zeropage, data, None, None),
        /* INX     */ 0xE8 => fetch(INX(op), 2, Implied, data, None, None),
        /* SBC_imm */ 0xE9 => fetch(SBC(op), 2, Immediate, data, None, None),
        /* NOP     */ 0xEA => fetch(NOP(op), 2, Implied, data, None, None),
        /* CPX     */ 0xEC => fetch(CPX(op), 4, Absolute, data, None, None),
        /* SBC_abs */ 0xED => fetch(SBC(op), 4, Absolute, data, None, None),
        /* INC_abs */ 0xEE => fetch(INC(op), 6, Absolute, data, None, None),
        /* BEQ_rel */ 0xF0 => fetch(BEQ(op), 4, Relative, data, None, None), // add 1 cycle if page boundary is crossed
        /* SBC_izy */ 0xF1 => fetch(SBC(op), 6, IndirectIndexedY(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* SBC_zpx */ 0xF5 => fetch(SBC(op), 4, ZeropageIndexedX, data, None, None),
        /* INC_zpx */ 0xF6 => fetch(INC(op), 6, ZeropageIndexedX, data, None, None),
        /* SED     */ 0xF8 => fetch(SED(op), 2, Implied, data, None, None),
        /* SBC_aby */ 0xF9 => fetch(SBC(op), 5, AbsoluteIndexedY(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* SBC_abx */ 0xFD => fetch(SBC(op), 5, AbsoluteIndexedX(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* INC_abx */ 0xFE => fetch(INC(op), 7, AbsoluteIndexedX(false), data, None, None),
        // ** undocumented/forbidden instructions **
        /* HLT     */ 0x02 => fetch(HLT(op), 1, Implied, data, None, None),
        /* SLO_izx */ 0x03 => fetch(SLO(op), 8, IndexedIndirectX, data, None, None),
        /* NOP_zp  */ 0x04 => fetch(NOP(op), 3, Zeropage, data, None, None),
        /* SLO_zp  */ 0x07 => fetch(SLO(op), 5, Zeropage, data, None, None),
        /* ANC_imm */ 0x0B => fetch(ANC(op), 2, Immediate, data, None, None),
        /* NOP_abs */ 0x0C => fetch(NOP(op), 4, Absolute, data, None, None),
        /* SLO_abs */ 0x0F => fetch(SLO(op), 6, Absolute, data, None, None),
        /* HLT     */ 0x12 => fetch(HLT(op), 1, Implied, data, None, None),
        /* SLO_izy */ 0x13 => fetch(SLO(op), 8, IndirectIndexedY(false), data, None, None),
        /* NOP_zpx */ 0x14 => fetch(NOP(op), 4, ZeropageIndexedX, data, None, None),
        /* SLO_zpx */ 0x17 => fetch(SLO(op), 6, ZeropageIndexedX, data, None, None),
        /* NOP     */ 0x1A => fetch(NOP(op), 2, Implied, data, None, None),
        /* SLO_aby */ 0x1B => fetch(SLO(op), 7, AbsoluteIndexedY(false), data, None, None),
        /* NOP_abx */ 0x1C => fetch(NOP(op), 5, AbsoluteIndexedX(true), data, None, None), // add 1 cycle if page boudary is crossed
        /* SLO_abx */ 0x1F => fetch(SLO(op), 7, AbsoluteIndexedX(false), data, None, None),
        /* HLT     */ 0x22 => fetch(HLT(op), 1, Implied, data, None, None),
        /* RLA_izx */ 0x23 => fetch(RLA(op), 8, IndexedIndirectX, data, None, None),
        /* RLA_zp  */ 0x27 => fetch(RLA(op), 5, Zeropage, data, None, None),
        /* ANC_imm */ 0x2B => fetch(ANC(op), 2, Immediate, data, None, None),
        /* RLA_abs */ 0x2F => fetch(RLA(op), 6, Absolute, data, None, None),
        /* HLT     */ 0x32 => fetch(HLT(op), 1, Implied, data, None, None),
        /* RLA_izy */ 0x33 => fetch(RLA(op), 8, IndirectIndexedY(false), data, None, None),
        /* NOP_zpx */ 0x34 => fetch(NOP(op), 4, ZeropageIndexedX, data, None, None),
        /* RLA_zpx */ 0x37 => fetch(RLA(op), 6, ZeropageIndexedX, data, None, None),
        /* NOP     */ 0x3A => fetch(NOP(op), 2, Implied, data, None, None),
        /* RLA_aby */ 0x3B => fetch(RLA(op), 7, AbsoluteIndexedY(false), data, None, None),
        /* NOP_abx */ 0x3C => fetch(NOP(op), 5, AbsoluteIndexedX(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* RLA_abx */ 0x3F => fetch(RLA(op), 7, AbsoluteIndexedX(false), data, None, None),
        /* HLT     */ 0x42 => fetch(HLT(op), 1, Implied, data, None, None),
        /* SRE_izx */ 0x43 => fetch(SRE(op), 8, IndexedIndirectX, data, None, None),
        /* NOP     */ 0x44 => fetch(NOP(op), 3, Implied, data, None, None),
        /* SRE_zp  */ 0x47 => fetch(SRE(op), 5, Zeropage, data, None, None),
        /* ALR_imm */ 0x4B => fetch(ALR(op), 2, Immediate, data, None, None),
        /* SRE_abs */ 0x4F => fetch(SRE(op), 6, Absolute, data, None, None),
        /* HLT     */ 0x52 => fetch(HLT(op), 1, Implied, data, None, None),
        /* SRE_izy */ 0x53 => fetch(SRE(op), 8, IndirectIndexedY(false), data, None, None),
        /* NOP_zpx */ 0x54 => fetch(NOP(op), 4, ZeropageIndexedX, data, None, None),
        /* SRE_zpx */ 0x57 => fetch(SRE(op), 6, ZeropageIndexedX, data, None, None),
        /* NOP     */ 0x5A => fetch(NOP(op), 2, Implied, data, None, None),
        /* SRE_aby */ 0x5B => fetch(SRE(op), 7, AbsoluteIndexedY(false), data, None, None),
        /* NOP_abx */ 0x5C => fetch(NOP(op), 5, AbsoluteIndexedX(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* SRE_abx */ 0x5F => fetch(SRE(op), 7, AbsoluteIndexedX(false), data, None, None),
        /* HLT     */ 0x62 => fetch(HLT(op), 1, Implied, data, None, None),
        /* RRA_izx */ 0x63 => fetch(RRA(op), 8, IndexedIndirectX, data, None, None),
        /* NOP_zp  */ 0x64 => fetch(NOP(op), 3, Zeropage, data, None, None),
        /* RRA_zp  */ 0x67 => fetch(RRA(op), 5, Zeropage, data, None, None),
        /* ARR     */ 0x6B => fetch(ARR(op), 2, Implied, data, None, None),
        /* RRA_abs */ 0x6F => fetch(RRA(op), 6, Absolute, data, None, None),
        /* HLT     */ 0x72 => fetch(HLT(op), 1, Implied, data, None, None),
        /* RRA_izy */ 0x73 => fetch(RRA(op), 8, IndirectIndexedY(false), data, None, None),
        /* NOP_zpx */ 0x74 => fetch(NOP(op), 4, ZeropageIndexedX, data, None, None),
        /* RRA_zpx */ 0x77 => fetch(RRA(op), 6, ZeropageIndexedX, data, None, None),
        /* NOP     */ 0x7A => fetch(NOP(op), 2, Implied, data, None, None),
        /* RRA_aby */ 0x7B => fetch(RRA(op), 7, AbsoluteIndexedY(false), data, None, None),
        /* NOP_abx */ 0x7C => fetch(NOP(op), 5, AbsoluteIndexedX(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* RRA_abx */ 0x7F => fetch(RRA(op), 7, AbsoluteIndexedX(false), data, None, None),
        /* NOP_imm */ 0x80 => fetch(NOP(op), 2, Immediate, data, None, None),
        /* NOP_imm */ 0x82 => fetch(NOP(op), 2, Immediate, data, None, None),
        /* SAX_izx */ 0x83 => fetch(SAX(op), 6, IndexedIndirectX, data, None, None),
        /* SAX_zp  */ 0x87 => fetch(SAX(op), 3, Zeropage, data, None, None),
        /* NOP_imm */ 0x89 => fetch(NOP(op), 2, Immediate, data, None, None),
        /* XAA_imm */ 0x8B => fetch(XAA(op), 2, Immediate, data, None, None),
        /* SAX_abs */ 0x8F => fetch(SAX(op), 4, Absolute, data, None, None),
        /* HLT     */ 0x92 => fetch(HLT(op), 1, Implied, data, None, None),
        /* AHX_izy */ 0x93 => fetch(AHX(op), 6, IndirectIndexedY(false), data, None, None),
        /* SAX_zpy */ 0x97 => fetch(SAX(op), 4, ZeropageIndexedY, data, None, None),
        /* TAS_aby */ 0x9B => fetch(TAS(op), 5, AbsoluteIndexedY(false), data, None, None),
        /* SHY_abx */ 0x9C => fetch(SHY(op), 5, AbsoluteIndexedX(false), data, None, None),
        /* SHX_aby */ 0x9E => fetch(SHX(op), 5, AbsoluteIndexedY(false), data, None, None),
        /* AHX_aby */ 0x9F => fetch(AHX(op), 5, AbsoluteIndexedY(false), data, None, None),
        /* LAX_izx */ 0xA3 => fetch(LAX(op), 6, IndexedIndirectX, data, None, None),
        /* LAX_zp  */ 0xA7 => fetch(LAX(op), 3, Zeropage, data, None, None),
        /* LAX_imm */ 0xAB => fetch(LAX(op), 2, Immediate, data, None, None),
        /* LAX_abs */ 0xAF => fetch(LAX(op), 4, Absolute, data, None, None),
        /* HLT     */ 0xB2 => fetch(HLT(op), 1, Implied, data, None, None),
        /* LAX_izy */ 0xB3 => fetch(LAX(op), 6, IndirectIndexedY(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* LAX_zpy */ 0xB7 => fetch(LAX(op), 4, ZeropageIndexedY, data, None, None),
        /* LAS_aby */ 0xBB => fetch(LAS(op), 5, AbsoluteIndexedY(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* LAX_aby */ 0xBF => fetch(LAX(op), 5, AbsoluteIndexedY(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* NOP_imm */ 0xC2 => fetch(NOP(op), 2, Immediate, data, None, None),
        /* DCP_izx */ 0xC3 => fetch(DCP(op), 8, IndexedIndirectX, data, None, None),
        /* DCP_zp  */ 0xC7 => fetch(DCP(op), 5, Zeropage, data, None, None),
        /* AXS_imm */ 0xCB => fetch(AXS(op), 2, Immediate, data, None, None),
        /* DCP_abs */ 0xCF => fetch(DCP(op), 6, Absolute, data, None, None),
        /* HLT     */ 0xD2 => fetch(HLT(op), 1, Implied, data, None, None),
        /* DCP_izy */ 0xD3 => fetch(DCP(op), 8, IndirectIndexedY(false), data, None, None),
        /* NOP_zpx */ 0xD4 => fetch(NOP(op), 4, ZeropageIndexedX, data, None, None),
        /* DCP_zpx */ 0xD7 => fetch(DCP(op), 6, ZeropageIndexedX, data, None, None),
        /* NOP     */ 0xDA => fetch(NOP(op), 2, Implied, data, None, None),
        /* DCP_aby */ 0xDB => fetch(DCP(op), 7, AbsoluteIndexedY(false), data, None, None),
        /* NOP_abx */ 0xDC => fetch(NOP(op), 5, AbsoluteIndexedX(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* DCP_abx */ 0xDF => fetch(DCP(op), 7, AbsoluteIndexedX(false), data, None, None),
        /* NOP_imm */ 0xE2 => fetch(NOP(op), 2, Immediate, data, None, None),
        /* ISC_izx */ 0xE3 => fetch(ISC(op), 8, IndexedIndirectX, data, None, None),
        /* ISC_zp  */ 0xE7 => fetch(ISC(op), 5, Zeropage, data, None, None),
        /* SBC_imm */ 0xEB => fetch(SBC(op), 2, Immediate, data, None, None),
        /* ISC_abs */ 0xEF => fetch(ISC(op), 6, Absolute, data, None, None),
        /* HLT     */ 0xF2 => fetch(HLT(op), 1, Implied, data, None, None),
        /* ISC_izy */ 0xF3 => fetch(ISC(op), 8, IndirectIndexedY(false), data, None, None),
        /* NOP_zpx */ 0xF4 => fetch(NOP(op), 4, ZeropageIndexedX, data, None, None),
        /* ISC_zpx */ 0xF7 => fetch(ISC(op), 6, ZeropageIndexedX, data, None, None),
        /* NOP     */ 0xFA => fetch(NOP(op), 2, Implied, data, None, None),
        /* ISC_aby */ 0xFB => fetch(ISC(op), 7, AbsoluteIndexedY(false), data, None, None),
        /* NOP_abx */ 0xFC => fetch(NOP(op), 5, AbsoluteIndexedX(true), data, None, None), // add 1 cycle if page boundary is crossed
        /* ISC_abx */ 0xFF => fetch(ISC(op), 7, AbsoluteIndexedX(false), data, None, None),
                         _ => fetch(NOP(op), 0, Implied, data, None, None)
    }
}
