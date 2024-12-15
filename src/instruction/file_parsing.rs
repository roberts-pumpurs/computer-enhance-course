use crate::instruction::Instruction;

use super::{InstructionSet, Operand, Reg16, Reg8};

impl InstructionSet {
    #[must_use]
    pub fn from_decoded_asm_file(file_content: &str) -> Self {
        let mut instructions = Vec::new();
        let (lines, bits) = get_metadata(file_content);

        for line in lines {
            let instr = Instruction::from_line(line);
            instructions.push(instr);
        }

        Self { instructions, bits }
    }
}

pub(crate) fn get_metadata(file_content: &str) -> (impl Iterator<Item = &str>, u8) {
    const BITS_PREFIX: usize = "bits ".len();

    let mut lines = file_content
        .lines()
        .filter(|x| !x.starts_with(';'))
        .filter(|x| !x.is_empty());
    let bit_line = lines.next().unwrap();
    let bits = bit_line.split_at(BITS_PREFIX).1.parse().unwrap();
    (lines, bits)
}

impl Instruction {
    #[must_use]
    pub fn from_line(line: &str) -> Self {
        let Some((ix, regs)) = line.split_once(' ') else {
            panic!("Invalid instruction: {line}");
        };

        let Some((dest, source)) = regs.split_once(", ") else {
            panic!("Invalid instruction: {line}");
        };

        let dest = Operand::from_str(dest);
        let source = Operand::from_str(source);

        match ix {
            "mov" => Self::Mov { dest, source },
            _ => panic!("Invalid instruction: {line}"),
        }
    }
}

impl Operand {
    #[must_use]
    pub(crate) fn from_chars(left: char, right: char) -> Self {
        match (left, right) {
            ('a', 'l') => Operand::Reg8(Reg8::AL),
            ('c', 'l') => Operand::Reg8(Reg8::CL),
            ('d', 'l') => Operand::Reg8(Reg8::BL),
            ('b', 'l') => Operand::Reg8(Reg8::BL),
            ('a', 'h') => Operand::Reg8(Reg8::AH),
            ('c', 'h') => Operand::Reg8(Reg8::CH),
            ('d', 'h') => Operand::Reg8(Reg8::DH),
            ('b', 'h') => Operand::Reg8(Reg8::BH),
            ('a', 'x') => Operand::Reg16(Reg16::AX),
            ('c', 'x') => Operand::Reg16(Reg16::CX),
            ('d', 'x') => Operand::Reg16(Reg16::DX),
            ('b', 'x') => Operand::Reg16(Reg16::BX),
            ('s', 'p') => Operand::Reg16(Reg16::SP),
            ('b', 'p') => Operand::Reg16(Reg16::BP),
            ('s', 'i') => Operand::Reg16(Reg16::SI),
            ('d', 'i') => Operand::Reg16(Reg16::DI),
            _ => {
                unreachable!()
            }
        }
    }

    #[must_use]
    pub(crate) const fn to_chars(&self) -> (char, char) {
        match self {
            // Reg8 variants
            Operand::Reg8(Reg8::AL) => ('a', 'l'),
            Operand::Reg8(Reg8::CL) => ('c', 'l'),
            Operand::Reg8(Reg8::DL) => ('d', 'l'),
            Operand::Reg8(Reg8::BL) => ('b', 'l'),
            Operand::Reg8(Reg8::AH) => ('a', 'h'),
            Operand::Reg8(Reg8::CH) => ('c', 'h'),
            Operand::Reg8(Reg8::DH) => ('d', 'h'),
            Operand::Reg8(Reg8::BH) => ('b', 'h'),

            // Reg16 variants
            Operand::Reg16(Reg16::AX) => ('a', 'x'),
            Operand::Reg16(Reg16::CX) => ('c', 'x'),
            Operand::Reg16(Reg16::DX) => ('d', 'x'),
            Operand::Reg16(Reg16::BX) => ('b', 'x'),
            Operand::Reg16(Reg16::SP) => ('s', 'p'),
            Operand::Reg16(Reg16::BP) => ('b', 'p'),
            Operand::Reg16(Reg16::SI) => ('s', 'i'),
            Operand::Reg16(Reg16::DI) => ('d', 'i'),
            _ => unimplemented!(),
        }
    }

    #[must_use]
    pub fn from_str(reg: &str) -> Self {
        let mut chars = reg.chars();
        let first = chars.next().expect("needs to have 2 chars");
        let second = chars.next().expect("needs to have 2 chars");
        Self::from_chars(first, second)
    }
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use super::*;

    #[test]
    fn test_parse_ixs_single_mov() {
        let content =
            std::fs::read_to_string("fixtures/listing_0037_single_register_mov.asm").unwrap();
        let derived_set = InstructionSet::from_decoded_asm_file(&content);

        let expected = InstructionSet {
            instructions: vec![Instruction::Mov {
                dest: Operand::Reg16(Reg16::CX),
                source: Operand::Reg16(Reg16::BX),
            }],
            bits: 16,
        };

        assert_eq!(derived_set, expected);
    }

    #[test]
    fn test_parse_ixs_large_mov() {
        let content =
            std::fs::read_to_string("fixtures/listing_0038_many_register_mov.asm").unwrap();
        let derived_set = InstructionSet::from_decoded_asm_file(&content);
        // mov cx, bx
        // mov ch, ah
        // mov dx, bx
        // mov si, bx
        // mov bx, di
        // mov al, cl
        // mov ch, ch
        // mov bx, ax
        // mov bx, si
        // mov sp, di
        // mov bp, ax
        let expected = InstructionSet {
            instructions: [
                Instruction::Mov {
                    dest: Operand::Reg16(Reg16::CX),
                    source: Operand::Reg16(Reg16::BX),
                },
                Instruction::Mov {
                    dest: Operand::Reg8(Reg8::CH),
                    source: Operand::Reg8(Reg8::AH),
                },
                Instruction::Mov {
                    dest: Operand::Reg16(Reg16::DX),
                    source: Operand::Reg16(Reg16::BX),
                },
                Instruction::Mov {
                    dest: Operand::Reg16(Reg16::SI),
                    source: Operand::Reg16(Reg16::BX),
                },
                Instruction::Mov {
                    dest: Operand::Reg16(Reg16::BX),
                    source: Operand::Reg16(Reg16::DI),
                },
                Instruction::Mov {
                    dest: Operand::Reg8(Reg8::AL),
                    source: Operand::Reg8(Reg8::CL),
                },
                Instruction::Mov {
                    dest: Operand::Reg8(Reg8::CH),
                    source: Operand::Reg8(Reg8::CH),
                },
                Instruction::Mov {
                    dest: Operand::Reg16(Reg16::BX),
                    source: Operand::Reg16(Reg16::AX),
                },
                Instruction::Mov {
                    dest: Operand::Reg16(Reg16::BX),
                    source: Operand::Reg16(Reg16::SI),
                },
                Instruction::Mov {
                    dest: Operand::Reg16(Reg16::SP),
                    source: Operand::Reg16(Reg16::DI),
                },
                Instruction::Mov {
                    dest: Operand::Reg16(Reg16::BP),
                    source: Operand::Reg16(Reg16::AX),
                },
            ]
            .to_vec(),
            bits: 16,
        };

        assert_eq!(derived_set, expected);
    }
}
