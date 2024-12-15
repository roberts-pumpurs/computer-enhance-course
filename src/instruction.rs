mod binary_parsing;
mod file_parsing;

#[derive(Debug, PartialEq, Eq)]
pub struct InstructionSet {
    pub instructions: Vec<Instruction>,
    pub bits: u8,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Instruction {
    Mov { dest: Operand, source: Operand },
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
enum Operand {
    Reg8(Reg8),
    Reg16(Reg16),
}
