use crate::instruction::Instruction;

use super::{InstructionSet, MovOperand, Reg16, Reg8};

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

        let dest = MovOperand::from_str(dest);
        let source = MovOperand::from_str(source);

        match ix {
            "mov" => Self::Mov { dest, source },
            _ => panic!("Invalid instruction: {line}"),
        }
    }
}

impl MovOperand {
    #[must_use]
    pub(crate) fn from_chars(left: char, right: char) -> Self {
        match (left, right) {
            ('a', 'l') => MovOperand::Reg8(Reg8::AL),
            ('c', 'l') => MovOperand::Reg8(Reg8::CL),
            ('d', 'l') => MovOperand::Reg8(Reg8::BL),
            ('b', 'l') => MovOperand::Reg8(Reg8::BL),
            ('a', 'h') => MovOperand::Reg8(Reg8::AH),
            ('c', 'h') => MovOperand::Reg8(Reg8::CH),
            ('d', 'h') => MovOperand::Reg8(Reg8::DH),
            ('b', 'h') => MovOperand::Reg8(Reg8::BH),
            ('a', 'x') => MovOperand::Reg16(Reg16::AX),
            ('c', 'x') => MovOperand::Reg16(Reg16::CX),
            ('d', 'x') => MovOperand::Reg16(Reg16::DX),
            ('b', 'x') => MovOperand::Reg16(Reg16::BX),
            ('s', 'p') => MovOperand::Reg16(Reg16::SP),
            ('b', 'p') => MovOperand::Reg16(Reg16::BP),
            ('s', 'i') => MovOperand::Reg16(Reg16::SI),
            ('d', 'i') => MovOperand::Reg16(Reg16::DI),
            _ => {
                unreachable!()
            }
        }
    }

    #[must_use]
    pub(crate) const fn to_chars(&self) -> (char, char) {
        match self {
            // Reg8 variants
            MovOperand::Reg8(Reg8::AL) => ('a', 'l'),
            MovOperand::Reg8(Reg8::CL) => ('c', 'l'),
            MovOperand::Reg8(Reg8::DL) => ('d', 'l'),
            MovOperand::Reg8(Reg8::BL) => ('b', 'l'),
            MovOperand::Reg8(Reg8::AH) => ('a', 'h'),
            MovOperand::Reg8(Reg8::CH) => ('c', 'h'),
            MovOperand::Reg8(Reg8::DH) => ('d', 'h'),
            MovOperand::Reg8(Reg8::BH) => ('b', 'h'),

            // Reg16 variants
            MovOperand::Reg16(Reg16::AX) => ('a', 'x'),
            MovOperand::Reg16(Reg16::CX) => ('c', 'x'),
            MovOperand::Reg16(Reg16::DX) => ('d', 'x'),
            MovOperand::Reg16(Reg16::BX) => ('b', 'x'),
            MovOperand::Reg16(Reg16::SP) => ('s', 'p'),
            MovOperand::Reg16(Reg16::BP) => ('b', 'p'),
            MovOperand::Reg16(Reg16::SI) => ('s', 'i'),
            MovOperand::Reg16(Reg16::DI) => ('d', 'i'),
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
                dest: MovOperand::Reg16(Reg16::CX),
                source: MovOperand::Reg16(Reg16::BX),
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
                    dest: MovOperand::Reg16(Reg16::CX),
                    source: MovOperand::Reg16(Reg16::BX),
                },
                Instruction::Mov {
                    dest: MovOperand::Reg8(Reg8::CH),
                    source: MovOperand::Reg8(Reg8::AH),
                },
                Instruction::Mov {
                    dest: MovOperand::Reg16(Reg16::DX),
                    source: MovOperand::Reg16(Reg16::BX),
                },
                Instruction::Mov {
                    dest: MovOperand::Reg16(Reg16::SI),
                    source: MovOperand::Reg16(Reg16::BX),
                },
                Instruction::Mov {
                    dest: MovOperand::Reg16(Reg16::BX),
                    source: MovOperand::Reg16(Reg16::DI),
                },
                Instruction::Mov {
                    dest: MovOperand::Reg8(Reg8::AL),
                    source: MovOperand::Reg8(Reg8::CL),
                },
                Instruction::Mov {
                    dest: MovOperand::Reg8(Reg8::CH),
                    source: MovOperand::Reg8(Reg8::CH),
                },
                Instruction::Mov {
                    dest: MovOperand::Reg16(Reg16::BX),
                    source: MovOperand::Reg16(Reg16::AX),
                },
                Instruction::Mov {
                    dest: MovOperand::Reg16(Reg16::BX),
                    source: MovOperand::Reg16(Reg16::SI),
                },
                Instruction::Mov {
                    dest: MovOperand::Reg16(Reg16::SP),
                    source: MovOperand::Reg16(Reg16::DI),
                },
                Instruction::Mov {
                    dest: MovOperand::Reg16(Reg16::BP),
                    source: MovOperand::Reg16(Reg16::AX),
                },
            ]
            .to_vec(),
            bits: 16,
        };

        assert_eq!(derived_set, expected);
    }
}
