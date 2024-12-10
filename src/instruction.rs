mod binary_parsing;
mod file_parsing;

#[derive(Debug, PartialEq)]
pub struct InstructionSet {
    pub instructions: Vec<Instruction>,
    pub bits: u8,
}

#[derive(Debug, PartialEq)]
pub enum Instruction {
    Mov { dest: Register, source: Register },
}

#[derive(Debug, PartialEq)]
pub enum Register {
    R000,
    R001,
    R010,
    R100,
    R011,
    R101,
    R110,
    R111,
}
