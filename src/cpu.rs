use crate::instruction::Instruction;

pub struct Cpu;

impl Default for Cpu {
    fn default() -> Self {
        Self::new()
    }
}

impl Cpu {
    #[must_use]
    pub const fn new() -> Self {
        Self
    }

    #[must_use]
    pub fn decompile(&self, asm: &[u8]) -> Instruction {
        todo!()
    }
}
