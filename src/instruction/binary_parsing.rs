use crate::instruction::MovOperand;

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

mod word_modes {
    pub(crate) const W_M_16B_WORD: u8 = 0b00000001_u8;
    pub(crate) const W_M_8B_WORD: u8 = 0b_00000000_u8;
}

impl Instruction {
    #[must_use]
    pub fn from_byte<I: Iterator<Item = u8>>(mut bytes: I) -> Self {
        mod opcodes {
            pub(crate) const REG_MEM_TO_FROM_REG: u8 = 0b____10001000_u8;
            pub(crate) const REG_MEM_TO_FROM_REG_MAX: u8 = 0b10001011_u8;

            pub(crate) const IMMEDIATE_TO_REG: u8 = 0b_______10110000_u8;
            pub(crate) const IMMEDIATE_TO_REG_MAX: u8 = 0b___10111111_u8;
        }

        let first_byte = bytes.next().unwrap();
        match first_byte {
            opcodes::REG_MEM_TO_FROM_REG..=opcodes::REG_MEM_TO_FROM_REG_MAX => {
                mod masks {
                    pub(crate) const DIR: u8 = 0b00000010_u8;
                    pub(crate) const WOR: u8 = 0b00000001_u8;
                    pub(crate) const MOD: u8 = 0b________11000000_u8;
                    pub(crate) const RG1: u8 = 0b________00111000_u8;
                    pub(crate) const RG2: u8 = 0b________00000111_u8;
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

                        let source = reg_to_mov_operand(&word, register_operand1 >> 3);
                        let dest = reg_to_mov_operand(&word, register_operand2);

                        Self::Mov { dest, source }
                    }
                    memory_mode::MEM_M_NO_DISPLACEMENT => {
                        let reg = reg_to_mov_operand(&word, register_operand1 >> 3);
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
                        let reg = reg_to_mov_operand(&word, register_operand1 >> 3);
                        let rm = decode_mod01_mod02_rm(register_operand2, low_disp_byte);

                        let (source, dest) = match direction {
                            dir_mode::D_M_REG_HAS_IX_SOURCE => (reg, MovOperand::Mod01(rm)),
                            dir_mode::W_M_REG_HAS_IX_DEST => (MovOperand::Mod01(rm), reg),
                            _ => unreachable!(),
                        };
                        Self::Mov { dest, source }
                    }
                    memory_mode::MEM_M_16_BIT_DISPLACEMENT => {
                        let low_disp_byte = bytes.next().unwrap();
                        let high_disp_byte = bytes.next().unwrap();

                        let reg = reg_to_mov_operand(&word, register_operand1 >> 3);
                        let rm = decode_mod01_mod02_rm(
                            register_operand2,
                            (low_disp_byte, high_disp_byte),
                        );
                        let (source, dest) = match direction {
                            dir_mode::D_M_REG_HAS_IX_SOURCE => (reg, MovOperand::Mod10(rm)),
                            dir_mode::W_M_REG_HAS_IX_DEST => (MovOperand::Mod10(rm), reg),
                            _ => unreachable!(),
                        };
                        Self::Mov { dest, source }
                    }
                    _ => unimplemented!("we only support reg to reg movement"),
                }
            }
            opcodes::IMMEDIATE_TO_REG..=opcodes::IMMEDIATE_TO_REG_MAX => {
                mod masks {
                    pub(crate) const REG: u8 = 0b00000111_u8;
                    pub(crate) const WOR: u8 = 0b00001000_u8;
                }

                let wide = (first_byte & masks::WOR) >> 3;
                let reg = first_byte & masks::REG;
                match wide {
                    word_modes::W_M_16B_WORD => {
                        let reg = decode_reg16(reg);
                        let data_byte_1 = bytes.next().unwrap();
                        let data_byte_2 = bytes.next().unwrap();
                        Self::ImmToReg16 {
                            dest: reg,
                            source: (data_byte_1, data_byte_2),
                        }
                    }
                    word_modes::W_M_8B_WORD => {
                        let reg = decode_reg8(reg);
                        let data_byte_1 = bytes.next().unwrap();
                        Self::ImmToReg8 {
                            dest: reg,
                            source: (data_byte_1),
                        }
                    }
                    _ => unreachable!(),
                }
            }
            _ => unimplemented!(),
        }
    }
}

fn reg_to_mov_operand(word: &u8, register_operand1: u8) -> MovOperand {
    let reg = match *word {
        word_modes::W_M_16B_WORD => MovOperand::Reg16(decode_reg16(register_operand1)),
        word_modes::W_M_8B_WORD => MovOperand::Reg8(decode_reg8(register_operand1)),
        _ => unreachable!(),
    };
    reg
}

fn decode_reg8(reg: u8) -> Reg8 {
    match reg {
        0b000 => Reg8::AL,
        0b001 => Reg8::CL,
        0b010 => Reg8::DL,
        0b011 => Reg8::BL,
        0b100 => Reg8::AH,
        0b101 => Reg8::CH,
        0b110 => Reg8::DH,
        0b111 => Reg8::BH,
        _ => unreachable!(),
    }
}

fn decode_reg16(reg: u8) -> Reg16 {
    match reg {
        0b000 => Reg16::AX,
        0b001 => Reg16::CX,
        0b010 => Reg16::DX,
        0b011 => Reg16::BX,
        0b100 => Reg16::SP,
        0b101 => Reg16::BP,
        0b110 => Reg16::SI,
        0b111 => Reg16::DI,
        _ => unreachable!(),
    }
}

fn decode_mod00_rm<I: Iterator<Item = u8>>(rm: u8, mut bytes: I) -> MovOperand {
    MovOperand::Mod00(match rm {
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
    use std::{
        fs::{self, File},
        io::{Read, Write},
    };

    use itertools::{assert_equal, Itertools};
    use pretty_assertions::assert_eq;
    use tempdir::TempDir;
    use xshell::cmd;

    use super::*;

    fn read_and_test(test: &str) {
        let content_binary = std::fs::read(format!("fixtures/{test:}")).unwrap();
        let derived_set_binary = InstructionSet::from_bytes(&content_binary);

        generate_and_compare_machine_code(test, &derived_set_binary);
    }

    fn generate_and_compare_machine_code(test: &str, ix_set: &InstructionSet) {
        let output = format!("{ix_set:}");
        let sh = xshell::Shell::new().unwrap();
        let fixture_machine_code_file = sh.current_dir().join("fixtures").join(format!("{test:}"));
        let fixture_asm_code_file = sh
            .current_dir()
            .join("fixtures")
            .join(format!("{test:}.asm"));
        let expected_asm_content = sh
            .read_file(fixture_asm_code_file)
            .unwrap()
            .lines()
            .filter(|x| !x.starts_with(";"))
            .filter(|x| !x.is_empty())
            .join("\n");
        let expected_content = sh.read_binary_file(fixture_machine_code_file).unwrap();

        // write the temp asm to file
        let temp_dir = TempDir::new(test).unwrap();
        let asm_output_file = temp_dir.path().join(format!("{test:}.asm"));
        let machine_code_output_file = temp_dir.path().join(format!("{test:}"));
        let mut f = File::create(&asm_output_file).unwrap();
        f.write_all(output.as_bytes()).unwrap();
        f.sync_all().unwrap();

        // run the nasm command
        let _g = sh.push_dir(temp_dir.path());
        cmd!(sh, "nasm {asm_output_file} -o {machine_code_output_file}")
            .run()
            .unwrap();

        // read the generated binary
        let content_binary = sh.read_binary_file(&machine_code_output_file).unwrap();
        dbg!(ix_set);
        if content_binary != expected_content {
            assert_eq!(expected_asm_content, output);
        }
        assert_eq!(content_binary, expected_content);
    }

    #[test]
    fn test_listing_37() {
        let test = "listing_0037_single_register_mov";
        read_and_test(test);
    }

    #[test]
    fn test_listing_38() {
        let test = "listing_0038_many_register_mov";
        read_and_test(test);
    }

    #[test]
    fn test_listing_39() {
        let test = "listing_0039_more_movs";
        read_and_test(test);
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
