use itertools::Itertools as _;

use crate::instruction::Operand;

use super::{Instruction, InstructionSet, Reg16, Reg8};

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

        mod word_modes {
            pub(crate) const W_M_16B_WORD: u8 = 0b00000001_u8;
            pub(crate) const W_M_8B_WORD: u8 = 0b_00000000_u8;
        }

        mod dir_mode {
            pub(crate) const D_M_REG_HAS_IX_SOURCE: u8 = 0b00000000_u8;
            pub(crate) const W_M_REG_HAS_IX_DEST: u8 = 0b__00000010_u8;
        }

        mod memory_mode {
            pub(crate) const MEM_M_NO_DISPLACEMENT: u8 = 0b____00000000_u8;
            pub(crate) const MEM_M_8_BIT_DISPLACEMENT: u8 = 0b_01000000_u8;
            pub(crate) const MEM_M_16_BIT_DISPLACEMENT: u8 = 0b10000000_u8;
            pub(crate) const MEM_M_REGISTER_MODE: u8 = 0b______11000000_u8;
        }

        let opcode = first_byte & masks::OPC;
        let direction = first_byte & masks::DIR;
        let word = first_byte & masks::WOR;
        let memory_mode = second_byte & masks::MOD;

        if memory_mode != memory_mode::MEM_M_REGISTER_MODE {
            unimplemented!("we only support reg to reg movement");
        }

        if direction != dir_mode::D_M_REG_HAS_IX_SOURCE {
            unimplemented!("we can't read extra bytes yet");
        }

        match opcode {
            opcodes::REG_MEM_TO_FROM_REG => {
                let register_operand1 = second_byte & masks::RG1;
                let register_operand2 = second_byte & masks::RG2;

                match word {
                    word_modes::W_M_16B_WORD => {
                        let source = decode_reg16(register_operand1 >> 3);
                        let dest = decode_reg16(register_operand2);
                        Self::Mov {
                            dest: Operand::Reg16(dest),
                            source: Operand::Reg16(source),
                        }
                    }
                    word_modes::W_M_8B_WORD => {
                        let source = decode_reg8(register_operand1 >> 3);
                        let dest = decode_reg8(register_operand2);
                        Self::Mov {
                            dest: Operand::Reg8(dest),
                            source: Operand::Reg8(source),
                        }
                    }
                    _ => unreachable!(),
                }
            }
            _ => unimplemented!(),
        }
    }
}

fn decode_reg8(reg: u8) -> Reg8 {
    match reg {
        0 => Reg8::AL,
        1 => Reg8::CL,
        2 => Reg8::DL,
        3 => Reg8::BL,
        4 => Reg8::AH,
        5 => Reg8::CH,
        6 => Reg8::DH,
        7 => Reg8::BH,
        _ => unreachable!(),
    }
}

fn decode_reg16(reg: u8) -> Reg16 {
    match reg {
        0 => Reg16::AX,
        1 => Reg16::CX,
        2 => Reg16::DX,
        3 => Reg16::BX,
        4 => Reg16::SP,
        5 => Reg16::BP,
        6 => Reg16::SI,
        7 => Reg16::DI,
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
        let register = decode_reg8(base_data);
        assert_eq!(register, Reg8::BH);

        let base_data = 0b00111000_u8;
        let register = decode_reg8(base_data >> 3);
        assert_eq!(register, Reg8::BH);
    }
}
