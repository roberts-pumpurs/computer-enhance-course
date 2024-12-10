use crate::instruction::file_parsing;

use super::{Instruction, InstructionSet};

impl InstructionSet {
    pub fn from_bytes(bytes: &[u8]) -> Self {
        for byte in bytes.windows(2) {
            let first_byte = byte[0];
            let second_byte = byte[1];
            Instruction::from_byte(first_byte, second_byte);
        }
        Self {
            instructions: vec![],
            bits: 16,
        }
    }
}

impl Instruction {
    pub fn from_byte(first_byte: u8, second_byte: u8) -> Self {
        mod masks {
            pub const OPC: u8 = 0b11111100_u8;
            pub const DIR: u8 = 0b00000010_u8;
            pub const WOR: u8 = 0b00000001_u8;
            pub const MOD: u8 = 0b11000000_u8;
            pub const RG1: u8 = 0b00111000_u8;
            pub const RG2: u8 = 0b00000111_u8;
        }

        mod opcodes {
            pub const REG_MEM_TO_FROM_REG: u8 = 0b10001000_u8;
        }

        mod registers {}

        let opcode = first_byte & masks::OPC;
        let direction = first_byte & masks::DIR;
        let word = first_byte & masks::WOR;
        let memory_mode = second_byte & masks::MOD;
        let register_operand1 = second_byte & masks::RG1;
        let register_operand2 = second_byte & masks::RG2;

        dbg!(&format!("f byte {:b}", first_byte));
        dbg!(&format!("s byte {:b}", second_byte));
        dbg!(&format!("{:b}", opcode));
        match opcode {
            opcodes::REG_MEM_TO_FROM_REG => {}
            _ => unimplemented!(),
        }
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_move_parsing_works() {
        let content = std::fs::read_to_string("fixtures/single_reg_mov.asm").unwrap();
        let content_binary = std::fs::read("fixtures/single_reg_mov").unwrap();
        let derived_set = InstructionSet::from_decoded_asm_file(&content);
        let derived_set_binary = InstructionSet::from_bytes(&content_binary);

        assert_eq!(derived_set, derived_set_binary);
    }
}
