use std::fmt;

use num_derive::FromPrimitive;

mod binary_parsing;

#[derive(Debug, PartialEq, Eq)]
pub struct InstructionSet {
    pub instructions: Vec<(usize, Instruction)>,
    pub bits: u8,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Instruction {
    MovRegMemWithRegToEither {
        dest: MovOperand,
        source: MovOperand,
    },
    MovImmToReg8 {
        dest: Reg8,
        source: u8,
    },
    MovImmToReg16 {
        dest: Reg16,
        source: (u8, u8),
    },
    MovImmToMemory {
        dest: MovOperand,
        source: (u8, Option<u8>),
    },
    MovMemoryToAccumulator {
        dest: AccumulatorReg,
        source: (u8, u8),
    },
    MovAccumulatorToMemory {
        source: AccumulatorReg,
        dest: (u8, u8),
    },
    ArithmRegMemWithReg {
        operation: ArithmeticOp,
        dest: MovOperand,
        source: MovOperand,
    },
    ArithmImmToMemory {
        operation: ArithmeticOp,
        dest: MovOperand,
        source: (u8, Option<u8>),
    },
    ArithmImmToAcc {
        operation: ArithmeticOp,
        dest: AccumulatorReg,
        source: (u8, Option<u8>),
    },
    Unsupported,
}

#[derive(Debug, FromPrimitive, Clone, PartialEq, Eq, Copy)]
#[repr(u8)]
pub enum ArithmeticOp {
    Add = 0b00000000,
    Sub = 0b00000101,
    Cmp = 0b00000111,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum AccumulatorReg {
    Ax,
    Al,
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
pub enum Reg8 {
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
pub enum Mod00EffectiveAddr {
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
pub enum EffectiveAddr<T> {
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
pub enum MovOperand {
    Reg8(Reg8),
    Reg16(Reg16),
    Mod00(Mod00EffectiveAddr),
    Mod01(EffectiveAddr<u8>),
    Mod10(EffectiveAddr<(u8, u8)>),
}

impl fmt::Display for ArithmeticOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArithmeticOp::Add => write!(f, "add"),
            ArithmeticOp::Sub => write!(f, "sub"),
            ArithmeticOp::Cmp => write!(f, "cmp"),
        }
    }
}

impl fmt::Display for InstructionSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "bits 16")?;
        for (idx, instr) in &self.instructions {
            writeln!(f, "{instr} ; {idx:?}")?;
            // writeln!(f, "{instr}")?;
        }
        Ok(())
    }
}

impl fmt::Display for AccumulatorReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Ax => "ax",
            Self::Al => "al",
        };
        write!(f, "{s}")
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MovRegMemWithRegToEither { dest, source } => {
                write!(f, "mov {dest}, {source}")
            }
            Self::MovImmToReg8 { dest, source } => {
                write!(f, "mov {dest}, {source:}")
            }
            Self::MovImmToReg16 {
                dest,
                source: (low, high),
            } => {
                let val = (u16::from(*high) << 8) | u16::from(*low);
                write!(f, "mov {dest}, {val:}")
            }
            Self::MovImmToMemory { dest, source } => {
                let byte_1 = source.0;
                let source = match source.1 {
                    Some(byte_2) => {
                        let val = (u16::from(byte_2) << 8) | u16::from(byte_1);
                        format!("word {val:}")
                    }
                    None => {
                        format!("byte {byte_1:}")
                    }
                };
                write!(f, "mov {dest}, {source:}")
            }
            Self::MovMemoryToAccumulator { dest, source } => {
                let source = (u16::from(source.1) << 8) | u16::from(source.0);
                write!(f, "mov {dest}, [{source:}]")
            }
            Self::MovAccumulatorToMemory { dest, source } => {
                let dest = (u16::from(dest.1) << 8) | u16::from(dest.0);
                write!(f, "mov [{dest}], {source:}")
            }
            Instruction::ArithmRegMemWithReg {
                dest,
                source,
                operation,
            } => {
                write!(f, "{operation} {dest}, {source}")
            }
            Instruction::ArithmImmToMemory {
                dest,
                source,
                operation,
            } => {
                let byte_1 = source.0;
                let source = match source.1 {
                    Some(byte_2) => {
                        let val = (u16::from(byte_2) << 8) | u16::from(byte_1);
                        format!("word {val:}")
                    }
                    None => {
                        format!("byte {byte_1:}")
                    }
                };
                write!(f, "{operation} {dest}, {source:}")
            }
            Instruction::ArithmImmToAcc {
                dest,
                source,
                operation,
            } => {
                let byte_1 = source.0;
                let source = match source.1 {
                    Some(byte_2) => {
                        let val = (u16::from(byte_2) << 8) | u16::from(byte_1);
                        let is_signed = val & 0x8000_u16 != 0;
                        let disp_str = if is_signed {
                            // negative number in signed 8-bit
                            let neg = (!val).wrapping_add(1);
                            format!("-{neg:}")
                        } else {
                            format!("{val:}")
                        };
                        format!("{disp_str:}")
                    }
                    None => {
                        let is_signed = byte_1 & 0x80 != 0;

                        let disp_str = if is_signed {
                            // negative number in signed 8-bit
                            let neg = (!byte_1).wrapping_add(1);
                            format!("-{neg:}")
                        } else {
                            format!("{byte_1:}")
                        };
                        format!("{disp_str:}")
                    }
                };
                write!(f, "{operation} {dest}, {source:}")
            }
            Instruction::Unsupported => {
                write!(f, "; unsupported")
            }
        }
    }
}

impl fmt::Display for Reg16 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::AX => "ax",
            Self::CX => "cx",
            Self::DX => "dx",
            Self::BX => "bx",
            Self::SP => "sp",
            Self::BP => "bp",
            Self::SI => "si",
            Self::DI => "di",
        };
        write!(f, "{s}")
    }
}

impl fmt::Display for Reg8 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::AL => "al",
            Self::CL => "cl",
            Self::DL => "dl",
            Self::BL => "bl",
            Self::AH => "ah",
            Self::CH => "ch",
            Self::DH => "dh",
            Self::BH => "bh",
        };
        write!(f, "{s}")
    }
}

impl fmt::Display for Mod00EffectiveAddr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // For 16-bit addressing, assembly often displays memory references like [bx+si], [bp+di], etc.
        match self {
            Self::BxPlusSi => write!(f, "[bx + si]"),
            Self::BxPlusDi => write!(f, "[bx + di]"),
            Self::BPPlusSi => write!(f, "[bp + si]"),
            Self::BPPlusDi => write!(f, "[bp + di]"),
            Self::Si => write!(f, "[si]"),
            Self::Di => write!(f, "[di]"),
            Self::Bx => write!(f, "[bx]"),
            Self::DirectAddr((low, high)) => {
                let addr = (u16::from(*high) << 8) | u16::from(*low);
                write!(f, "[{addr:}]")
            }
        }
    }
}

impl fmt::Display for MovOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Reg8(r) => write!(f, "{r}"),
            Self::Reg16(r) => write!(f, "{r}"),
            Self::Mod00(m) => write!(f, "{m}"),
            Self::Mod01(m) => write!(f, "{m}"),
            Self::Mod10(m) => write!(f, "{m}"),
        }
    }
}

impl fmt::Display for EffectiveAddr<u8> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let disp = *match self {
            Self::BxPlusSi(d) => d,
            Self::BxPlusDi(d) => d,
            Self::BPPlusSi(d) => d,
            Self::BPPlusDi(d) => d,
            Self::Si(d) => d,
            Self::Di(d) => d,
            Self::Bp(d) => d,
            Self::Bx(d) => d,
        };

        // Display as a signed byte displacement in hex (e.g., +0x12, -0x0A)
        let is_signed = disp & 0x80 != 0;
        let disp_str = signed_display(is_signed, disp);

        match self {
            Self::BxPlusSi(_) => write!(f, "[bx + si{disp_str}]"),
            Self::BxPlusDi(_) => write!(f, "[bx + di{disp_str}]"),
            Self::BPPlusSi(_) => write!(f, "[bp + si{disp_str}]"),
            Self::BPPlusDi(_) => write!(f, "[bp + di{disp_str}]"),
            Self::Si(_) => write!(f, "[si{disp_str}]"),
            Self::Di(_) => write!(f, "[di{disp_str}]"),
            Self::Bp(_) => write!(f, "[bp{disp_str}]"),
            Self::Bx(_) => write!(f, "[bx{disp_str}]"),
        }
    }
}

fn signed_display(is_signed: bool, disp: u8) -> String {
    if disp == 0 {
        return format!("");
    }
    let disp_str = if is_signed {
        // negative number in signed 8-bit
        let neg = (!disp).wrapping_add(1);
        format!(" - {neg:}")
    } else {
        format!(" + {disp:}")
    };
    disp_str
}

fn signed_display_u16(is_signed: bool, disp: u16) -> String {
    let disp_str = if is_signed {
        // negative number in signed 8-bit
        let neg = (!disp).wrapping_add(1);
        format!("- {neg:}")
    } else {
        format!("+ {disp:}")
    };
    disp_str
}

impl fmt::Display for EffectiveAddr<(u8, u8)> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (low, high) = *match self {
            Self::BxPlusSi(d) => d,
            Self::BxPlusDi(d) => d,
            Self::BPPlusSi(d) => d,
            Self::BPPlusDi(d) => d,
            Self::Si(d) => d,
            Self::Di(d) => d,
            Self::Bp(d) => d,
            Self::Bx(d) => d,
        };

        let disp = (u16::from(high) << 8) | u16::from(low);
        // Display two-byte displacement in hex (e.g. +0x1234 or -0x1234)
        let disp_str = if disp & 0x8000 != 0 {
            if disp != 0 {
                // Negative in 16-bit signed
                let neg = (!disp).wrapping_add(1);
                format!("- {neg:}")
            } else {
                format!("{disp}")
            }
        } else {
            format!("+ {disp:}")
        };

        match self {
            Self::BxPlusSi(_) => write!(f, "[bx + si {disp_str}]"),
            Self::BxPlusDi(_) => write!(f, "[bx + di {disp_str}]"),
            Self::BPPlusSi(_) => write!(f, "[bp + si {disp_str}]"),
            Self::BPPlusDi(_) => write!(f, "[bp + di {disp_str}]"),
            Self::Si(_) => write!(f, "[si {disp_str}]"),
            Self::Di(_) => write!(f, "[di {disp_str}]"),
            Self::Bp(_) => write!(f, "[bp {disp_str}]"),
            Self::Bx(_) => write!(f, "[bx {disp_str}]"),
        }
    }
}
