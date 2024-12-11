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
        dest: (Register, Wide),
        source: (Register, Wide),
    },
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Wide(bool);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Register {
    AX,
    CX,
    DX,
    BX,
    SP,
    BP,
    SI,
    DI,
}
