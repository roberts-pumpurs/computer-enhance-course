use crate::instruction::{Register, Wide};
use itertools::Itertools as _;

use super::{Instruction, InstructionSet};

impl InstructionSet {
    #[must_use]
    pub fn from_bytes(bytes: &[u8]) -> Self {
        let instructions = bytes
            .iter()
            .tuples()
            .map(|(first, second)| Instruction::from_byte(*first, *second))
            .collect_vec();
        Self {
            instructions,
            bits: 16,
        }
    }
}

impl Instruction {
    #[must_use]
    pub fn from_byte(first_byte: u8, second_byte: u8) -> Self {
        mod masks {
            pub(crate) const OPC: u8 = 0b11111100_u8;
            pub(crate) const DIR: u8 = 0b00000010_u8;
            pub(crate) const WOR: u8 = 0b00000001_u8;
            pub(crate) const MOD: u8 = 0b________11000000_u8;
            pub(crate) const RG1: u8 = 0b________00111000_u8;
            pub(crate) const RG2: u8 = 0b________00000111_u8;
        }

        mod opcodes {
            pub(crate) const REG_MEM_TO_FROM_REG: u8 = 0b10001000_u8;
        }

        mod registers {}

        let opcode = first_byte & masks::OPC;
        let direction = first_byte & masks::DIR;
        let word = first_byte & masks::WOR;
        let memory_mode = second_byte & masks::MOD;

        match opcode {
            opcodes::REG_MEM_TO_FROM_REG => {
                let register_operand1 = second_byte & masks::RG1;
                let register_operand2 = second_byte & masks::RG2;

                let wide = Wide(word > 0);

                let source = get_register(register_operand1 >> 3);
                let dest = get_register(register_operand2);
                Self::Mov {
                    dest: (dest, wide),
                    source: (source, wide),
                }
            }
            _ => unimplemented!(),
        }
    }
}

#[inline]
fn get_register(register: u8) -> Register {
    match register {
        0b000 => Register::AX,
        0b001 => Register::CX,
        0b010 => Register::DX,
        0b011 => Register::BX,
        0b100 => Register::SP,
        0b101 => Register::BP,
        0b110 => Register::SI,
        0b111 => Register::DI,
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_move_parsing_works() {
        let content =
            std::fs::read_to_string("fixtures/listing_0037_single_register_mov.asm").unwrap();
        let content_binary = std::fs::read("fixtures/listing_0037_single_register_mov").unwrap();
        let derived_set = InstructionSet::from_decoded_asm_file(&content);
        let derived_set_binary = InstructionSet::from_bytes(&content_binary);

        assert_eq!(derived_set, derived_set_binary);
    }

    #[test]
    fn test_many_register_move_parsing() {
        let content =
            std::fs::read_to_string("fixtures/listing_0038_many_register_mov.asm").unwrap();
        let content_binary = std::fs::read("fixtures/listing_0038_many_register_mov").unwrap();
        let derived_set = InstructionSet::from_decoded_asm_file(&content);
        let derived_set_binary = InstructionSet::from_bytes(&content_binary);

        assert_eq!(derived_set, derived_set_binary);
    }

    #[test]
    fn test_get_expected_register() {
        let base_data = 0b00000111_u8;
        let register = get_register(base_data);
        assert_eq!(register, Register::DI);

        let base_data = 0b00111000_u8;
        let register = get_register(base_data >> 3);
        assert_eq!(register, Register::DI);
    }
}
