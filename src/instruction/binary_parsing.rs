use itertools::Itertools as _;

use crate::instruction::Operand;

use super::{EffectiveAddr, Instruction, InstructionSet, Mod00EffectiveAddr, Reg16, Reg8};

impl InstructionSet {
    #[must_use]
    pub fn from_bytes(bytes: &[u8]) -> Self {
        let mut byte_iter = bytes.iter().copied().peekable();

        let mut ixs = vec![];
        while byte_iter.peek().is_some() {
            let ix = Instruction::from_byte(&mut byte_iter);
            ixs.push(ix);
        }

        Self {
            instructions: ixs,
            bits: 16,
        }
    }
}

impl Instruction {
    #[must_use]
    pub fn from_byte<I: Iterator<Item = u8>>(mut bytes: I) -> Self {
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
            // reg is the source
            pub(crate) const D_M_REG_HAS_IX_SOURCE: u8 = 0b00000000_u8;
            // reg is the destination
            pub(crate) const W_M_REG_HAS_IX_DEST: u8 = 0b__00000010_u8;
        }

        mod memory_mode {
            pub(crate) const MEM_M_NO_DISPLACEMENT: u8 = 0b____00000000_u8;
            pub(crate) const MEM_M_8_BIT_DISPLACEMENT: u8 = 0b_01000000_u8;
            pub(crate) const MEM_M_16_BIT_DISPLACEMENT: u8 = 0b10000000_u8;
            pub(crate) const MEM_M_REGISTER_MODE: u8 = 0b______11000000_u8;
        }

        let first_byte = bytes.next().unwrap();
        let opcode = first_byte & masks::OPC;
        match opcode {
            opcodes::REG_MEM_TO_FROM_REG => {
                let second_byte = bytes.next().unwrap();

                let direction = first_byte & masks::DIR;
                let word = first_byte & masks::WOR;
                let memory_mode = second_byte & masks::MOD;
                let register_operand1 = second_byte & masks::RG1;
                let register_operand2 = second_byte & masks::RG2;

                match memory_mode {
                    memory_mode::MEM_M_REGISTER_MODE => {
                        assert_eq!(
                            direction,
                            dir_mode::D_M_REG_HAS_IX_SOURCE,
                            "reg mode always has all the bytes it needs"
                        );

                        let (source, dest) = match word {
                            word_modes::W_M_16B_WORD => {
                                decode_reg16_pair(register_operand1, register_operand2)
                            }
                            word_modes::W_M_8B_WORD => {
                                decode_reg8_pair(register_operand1, register_operand2)
                            }
                            _ => unreachable!(),
                        };

                        Self::Mov { dest, source }
                    }
                    memory_mode::MEM_M_NO_DISPLACEMENT => {
                        let reg = match word {
                            word_modes::W_M_16B_WORD => decode_reg16(register_operand1),
                            word_modes::W_M_8B_WORD => decode_reg8(register_operand1),
                            _ => unreachable!(),
                        };

                        let rm = decode_mod00_rm(register_operand2, &mut bytes);

                        let (source, dest) = match direction {
                            dir_mode::D_M_REG_HAS_IX_SOURCE => (reg, rm),
                            dir_mode::W_M_REG_HAS_IX_DEST => (rm, reg),
                            _ => unreachable!(),
                        };
                        Self::Mov { dest, source }
                    }
                    memory_mode::MEM_M_8_BIT_DISPLACEMENT => {
                        let low_disp_byte = bytes.next().unwrap();

                        let reg = match word {
                            word_modes::W_M_16B_WORD => decode_reg16(register_operand1),
                            word_modes::W_M_8B_WORD => decode_reg8(register_operand1),
                            _ => unreachable!(),
                        };

                        let rm = decode_mod01_mod02_rm(register_operand2, low_disp_byte);

                        let (source, dest) = match direction {
                            dir_mode::D_M_REG_HAS_IX_SOURCE => (reg, Operand::Mod01(rm)),
                            dir_mode::W_M_REG_HAS_IX_DEST => (Operand::Mod01(rm), reg),
                            _ => unreachable!(),
                        };
                        Self::Mov { dest, source }
                    }
                    memory_mode::MEM_M_16_BIT_DISPLACEMENT => {
                        let low_disp_byte = bytes.next().unwrap();
                        let high_disp_byte = bytes.next().unwrap();

                        let reg = match word {
                            word_modes::W_M_16B_WORD => decode_reg16(register_operand1),
                            word_modes::W_M_8B_WORD => decode_reg8(register_operand1),
                            _ => unreachable!(),
                        };

                        let rm = decode_mod01_mod02_rm(
                            register_operand2,
                            (low_disp_byte, high_disp_byte),
                        );

                        let (source, dest) = match direction {
                            dir_mode::D_M_REG_HAS_IX_SOURCE => (reg, Operand::Mod10(rm)),
                            dir_mode::W_M_REG_HAS_IX_DEST => (Operand::Mod10(rm), reg),
                            _ => unreachable!(),
                        };
                        Self::Mov { dest, source }
                    }
                    _ => unimplemented!("we only support reg to reg movement"),
                }
            }
            _ => unimplemented!(),
        }
    }
}

fn decode_reg8_pair(register_operand1: u8, register_operand2: u8) -> (Operand, Operand) {
    let source = decode_reg8(register_operand1 >> 3);
    let dest = decode_reg8(register_operand2);
    (source, dest)
}

fn decode_reg16_pair(register_operand1: u8, register_operand2: u8) -> (Operand, Operand) {
    let source = decode_reg16(register_operand1 >> 3);
    let dest = decode_reg16(register_operand2);
    (source, dest)
}

fn decode_reg8(reg: u8) -> Operand {
    Operand::Reg8(match reg {
        0b000 => Reg8::AL,
        0b001 => Reg8::CL,
        0b010 => Reg8::DL,
        0b011 => Reg8::BL,
        0b100 => Reg8::AH,
        0b101 => Reg8::CH,
        0b110 => Reg8::DH,
        0b111 => Reg8::BH,
        _ => unreachable!(),
    })
}

fn decode_reg16(reg: u8) -> Operand {
    Operand::Reg16(match reg {
        0b000 => Reg16::AX,
        0b001 => Reg16::CX,
        0b010 => Reg16::DX,
        0b011 => Reg16::BX,
        0b100 => Reg16::SP,
        0b101 => Reg16::BP,
        0b110 => Reg16::SI,
        0b111 => Reg16::DI,
        _ => unreachable!(),
    })
}

fn decode_mod00_rm<I: Iterator<Item = u8>>(rm: u8, mut bytes: I) -> Operand {
    Operand::Mod00(match rm {
        0b000 => Mod00EffectiveAddr::BxPlusSi,
        0b001 => Mod00EffectiveAddr::BxPlusDi,
        0b010 => Mod00EffectiveAddr::BPPlusSi,
        0b011 => Mod00EffectiveAddr::BPPlusDi,
        0b100 => Mod00EffectiveAddr::Si,
        0b101 => Mod00EffectiveAddr::Di,
        0b110 => Mod00EffectiveAddr::DirectAddr((bytes.next().unwrap(), bytes.next().unwrap())),
        0b111 => Mod00EffectiveAddr::Bx,
        _ => unreachable!(),
    })
}

fn decode_mod01_mod02_rm<T>(rm: u8, addr: T) -> EffectiveAddr<T> {
    match rm {
        0b000 => EffectiveAddr::BxPlusSi(addr),
        0b001 => EffectiveAddr::BxPlusDi(addr),
        0b010 => EffectiveAddr::BPPlusSi(addr),
        0b011 => EffectiveAddr::BPPlusDi(addr),
        0b100 => EffectiveAddr::Si(addr),
        0b101 => EffectiveAddr::Di(addr),
        0b110 => EffectiveAddr::Bp(addr),
        0b111 => EffectiveAddr::Bx(addr),
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_listing_37() {
        let content =
            std::fs::read_to_string("fixtures/listing_0037_single_register_mov.asm").unwrap();
        let content_binary = std::fs::read("fixtures/listing_0037_single_register_mov").unwrap();
        let derived_set = InstructionSet::from_decoded_asm_file(&content);
        let derived_set_binary = InstructionSet::from_bytes(&content_binary);

        assert_eq!(derived_set, derived_set_binary);
    }

    #[test]
    fn test_listing_38() {
        let content =
            std::fs::read_to_string("fixtures/listing_0038_many_register_mov.asm").unwrap();
        let content_binary = std::fs::read("fixtures/listing_0038_many_register_mov").unwrap();
        let derived_set = InstructionSet::from_decoded_asm_file(&content);
        let derived_set_binary = InstructionSet::from_bytes(&content_binary);

        assert_eq!(derived_set, derived_set_binary);
    }

    #[test]
    fn test_listing_39() {
        // let content = std::fs::read_to_string("fixtures/listing_0039_more_movs.asm").unwrap();
        let content_binary = std::fs::read("fixtures/listing_0039_more_movs").unwrap();
        let derived_set_binary = InstructionSet::from_bytes(&content_binary);

        // assert_eq!(derived_set, derived_set_binary);
        panic!("{:?}", derived_set_binary);
    }

    #[test]
    fn test_get_expected_register() {
        let base_data = 0b00000111_u8;
        let register = decode_reg8(base_data);
        assert_eq!(register, Operand::Reg8(Reg8::BH));

        let base_data = 0b00111000_u8;
        let register = decode_reg8(base_data >> 3);
        assert_eq!(register, Operand::Reg8(Reg8::BH));
    }
}
