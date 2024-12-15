mod binary_parsing;
mod file_parsing;

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
