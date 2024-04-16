use crate::instruction::Instruction;

pub struct Cpu;

impl Cpu {
    pub fn new() -> Self {
        Cpu
    }

    pub fn decompile(&self, asm: &[u8]) -> Instruction {
        todo!()
    }
}
