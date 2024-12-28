mod binary_parsing;

#[derive(Debug, PartialEq, Eq)]
pub struct InstructionSet {
    pub instructions: Vec<Instruction>,
    pub bits: u8,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Instruction {
    Mov {
        dest: MovOperand,
        source: MovOperand,
    },
    ImmToReg8 {
        dest: Reg8,
        source: u8,
    },
    ImmToReg16 {
        dest: Reg16,
        source: (u8, u8),
    },
    ImmToMemory {
        dest: MovOperand,
        source: (u8, Option<u8>),
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Reg16 {
    AX,
    CX,
    DX,
    BX,
    SP,
    BP,
    SI,
    DI,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Reg8 {
    AL,
    CL,
    DL,
    BL,
    AH,
    CH,
    DH,
    BH,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Mod00EffectiveAddr {
    BxPlusSi,
    BxPlusDi,
    BPPlusSi,
    BPPlusDi,
    Si,
    Di,
    DirectAddr((u8, u8)),
    Bx,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum EffectiveAddr<T> {
    BxPlusSi(T),
    BxPlusDi(T),
    BPPlusSi(T),
    BPPlusDi(T),
    Si(T),
    Di(T),
    Bp(T),
    Bx(T),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum MovOperand {
    Reg8(Reg8),
    Reg16(Reg16),
    Mod00(Mod00EffectiveAddr),
    Mod01(EffectiveAddr<u8>),
    Mod10(EffectiveAddr<(u8, u8)>),
}

use std::fmt;

impl fmt::Display for InstructionSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for instr in &self.instructions {
            writeln!(f, "{}", instr)?;
        }
        Ok(())
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Mov { dest, source } => {
                write!(f, "mov {}, {}", dest, source)
            }
            Instruction::ImmToReg8 { dest, source } => {
                write!(f, "mov {}, {:}", dest, source)
            }
            Instruction::ImmToReg16 {
                dest,
                source: (low, high),
            } => {
                let val = ((*high as u16) << 8) | (*low as u16);
                write!(f, "mov {}, {:}", dest, val)
            }
            Instruction::ImmToMemory { dest, source } => {
                let byte_1 = source.0;
                let source = match source.1 {
                    Some(byte_2) => {
                        let val = ((byte_2 as u16) << 8) | (byte_1 as u16);
                        format!("word {val:}")
                    }
                    None => {
                        format!("byte {byte_1:}")
                    }
                };
                write!(f, "mov {}, {:}", dest, source)
            }
        }
    }
}

impl fmt::Display for Reg16 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Reg16::AX => "ax",
            Reg16::CX => "cx",
            Reg16::DX => "dx",
            Reg16::BX => "bx",
            Reg16::SP => "sp",
            Reg16::BP => "bp",
            Reg16::SI => "si",
            Reg16::DI => "di",
        };
        write!(f, "{}", s)
    }
}

impl fmt::Display for Reg8 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Reg8::AL => "al",
            Reg8::CL => "cl",
            Reg8::DL => "dl",
            Reg8::BL => "bl",
            Reg8::AH => "ah",
            Reg8::CH => "ch",
            Reg8::DH => "dh",
            Reg8::BH => "bh",
        };
        write!(f, "{}", s)
    }
}

impl fmt::Display for Mod00EffectiveAddr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // For 16-bit addressing, assembly often displays memory references like [bx+si], [bp+di], etc.
        match self {
            Mod00EffectiveAddr::BxPlusSi => write!(f, "[bx + si]"),
            Mod00EffectiveAddr::BxPlusDi => write!(f, "[bx + di]"),
            Mod00EffectiveAddr::BPPlusSi => write!(f, "[bp + si]"),
            Mod00EffectiveAddr::BPPlusDi => write!(f, "[bp + di]"),
            Mod00EffectiveAddr::Si => write!(f, "[si]"),
            Mod00EffectiveAddr::Di => write!(f, "[di]"),
            Mod00EffectiveAddr::Bx => write!(f, "[bx]"),
            Mod00EffectiveAddr::DirectAddr((low, high)) => {
                let addr = ((*high as u16) << 8) | (*low as u16);
                write!(f, "[{:}]", addr)
            }
        }
    }
}

impl fmt::Display for MovOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MovOperand::Reg8(r) => write!(f, "{}", r),
            MovOperand::Reg16(r) => write!(f, "{}", r),
            MovOperand::Mod00(m) => write!(f, "{}", m),
            MovOperand::Mod01(m) => write!(f, "{}", m),
            MovOperand::Mod10(m) => write!(f, "{}", m),
        }
    }
}

impl fmt::Display for EffectiveAddr<u8> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let disp = *match self {
            EffectiveAddr::BxPlusSi(d) => d,
            EffectiveAddr::BxPlusDi(d) => d,
            EffectiveAddr::BPPlusSi(d) => d,
            EffectiveAddr::BPPlusDi(d) => d,
            EffectiveAddr::Si(d) => d,
            EffectiveAddr::Di(d) => d,
            EffectiveAddr::Bp(d) => d,
            EffectiveAddr::Bx(d) => d,
        };

        // Display as a signed byte displacement in hex (e.g., +0x12, -0x0A)
        let disp_str = if disp & 0x80 != 0 {
            // negative number in signed 8-bit
            let neg = (!disp).wrapping_add(1);
            format!("- {:}", neg)
        } else {
            format!("+ {:}", disp)
        };

        match self {
            EffectiveAddr::BxPlusSi(_) => write!(f, "[bx + si {}]", disp_str),
            EffectiveAddr::BxPlusDi(_) => write!(f, "[bx + di {}]", disp_str),
            EffectiveAddr::BPPlusSi(_) => write!(f, "[bp + si {}]", disp_str),
            EffectiveAddr::BPPlusDi(_) => write!(f, "[bp + di {}]", disp_str),
            EffectiveAddr::Si(_) => write!(f, "[si {}]", disp_str),
            EffectiveAddr::Di(_) => write!(f, "[di {}]", disp_str),
            EffectiveAddr::Bp(_) => write!(f, "[bp {}]", disp_str),
            EffectiveAddr::Bx(_) => write!(f, "[bx {}]", disp_str),
        }
    }
}

impl fmt::Display for EffectiveAddr<(u8, u8)> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (low, high) = *match self {
            EffectiveAddr::BxPlusSi(d) => d,
            EffectiveAddr::BxPlusDi(d) => d,
            EffectiveAddr::BPPlusSi(d) => d,
            EffectiveAddr::BPPlusDi(d) => d,
            EffectiveAddr::Si(d) => d,
            EffectiveAddr::Di(d) => d,
            EffectiveAddr::Bp(d) => d,
            EffectiveAddr::Bx(d) => d,
        };

        let disp = ((high as u16) << 8) | (low as u16);
        // Display two-byte displacement in hex (e.g. +0x1234 or -0x1234)
        let disp_str = if disp & 0x8000 != 0 {
            // Negative in 16-bit signed
            let neg = (!disp).wrapping_add(1);
            format!("- {:}", neg)
        } else {
            format!("+ {:}", disp)
        };

        match self {
            EffectiveAddr::BxPlusSi(_) => write!(f, "[bx + si {}]", disp_str),
            EffectiveAddr::BxPlusDi(_) => write!(f, "[bx + di {}]", disp_str),
            EffectiveAddr::BPPlusSi(_) => write!(f, "[bp + si {}]", disp_str),
            EffectiveAddr::BPPlusDi(_) => write!(f, "[bp + di {}]", disp_str),
            EffectiveAddr::Si(_) => write!(f, "[si {}]", disp_str),
            EffectiveAddr::Di(_) => write!(f, "[di {}]", disp_str),
            EffectiveAddr::Bp(_) => write!(f, "[bp {}]", disp_str),
            EffectiveAddr::Bx(_) => write!(f, "[bx {}]", disp_str),
        }
    }
}
