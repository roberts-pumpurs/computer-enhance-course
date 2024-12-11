use crate::instruction::Instruction;

use super::{InstructionSet, Register, Wide};

impl InstructionSet {
    #[must_use]
    pub fn from_decoded_asm_file(file_content: &str) -> Self {
        const BITS_PREFIX: usize = "bits ".len();
        let mut instructions = Vec::new();

        let mut lines = file_content
            .lines()
            .filter(|x| !x.starts_with(';'))
            .filter(|x| !x.is_empty());
        let bit_line = lines.next().unwrap();
        let bits = bit_line.split_at(BITS_PREFIX).1.parse().unwrap();

        for line in lines {
            let instr = Instruction::from_line(line);
            instructions.push(instr);
        }

        Self { instructions, bits }
    }
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

        let dest = Register::from_str(dest);
        let source = Register::from_str(source);

        match ix {
            "mov" => Self::Mov { dest, source },
            _ => panic!("Invalid instruction: {line}"),
        }
    }
}

impl Register {
    #[must_use]
    pub fn from_chars(left: char, right: char) -> (Self, Wide) {
        let is_wide = matches!(right, 'x' | 'p' | 'i');
        match (left, right) {
            ('a', 'l' | 'x') => (Self::AX, Wide(is_wide)),
            ('c', 'l' | 'x') => (Self::CX, Wide(is_wide)),
            ('d', 'l' | 'x') => (Self::DX, Wide(is_wide)),
            ('b', 'l' | 'x') => (Self::BX, Wide(is_wide)),
            ('a', 'h') | ('s', 'p') => (Self::SP, Wide(is_wide)),
            ('c', 'h') | ('b', 'p') => (Self::BP, Wide(is_wide)),
            ('d', 'h') | ('s', 'i') => (Self::SI, Wide(is_wide)),
            ('b', 'h') | ('d', 'i') => (Self::DI, Wide(is_wide)),
            _ => {
                panic!("unexpected chars")
            }
        }
    }

    #[must_use]
    pub const fn to_chars(&self, w: Wide) -> (char, char) {
        let w = w.0;
        match (self, w) {
            (Self::AX, false) => ('a', 'l'),
            (Self::AX, true) => ('a', 'x'),
            (Self::CX, false) => ('c', 'l'),
            (Self::CX, true) => ('c', 'x'),
            (Self::DX, false) => ('d', 'l'),
            (Self::DX, true) => ('d', 'x'),
            (Self::BX, false) => ('b', 'l'),
            (Self::BX, true) => ('b', 'x'),
            (Self::SP, false) => ('a', 'h'),
            (Self::SP, true) => ('s', 'p'),
            (Self::SI, false) => ('c', 'h'),
            (Self::BP, true) => ('b', 'p'),
            (Self::BP, false) => ('d', 'h'),
            (Self::SI, true) => ('s', 'i'),
            (Self::DI, false) => ('b', 'h'),
            (Self::DI, true) => ('d', 'i'),
        }
    }
    #[must_use]
    pub fn from_str(reg: &str) -> (Self, Wide) {
        let mut chars = reg.chars();
        let first = chars.next().unwrap();
        let second = chars.next().unwrap();
        Self::from_chars(first, second)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_ixs() {
        let content =
            std::fs::read_to_string("fixtures/listing_0037_single_register_mov.asm").unwrap();
        let derived_set = InstructionSet::from_decoded_asm_file(&content);

        let expected = InstructionSet {
            instructions: vec![Instruction::Mov {
                dest: Register::from_chars('c', 'x'),
                source: Register::from_chars('b', 'x'),
            }],
            bits: 16,
        };

        assert_eq!(derived_set, expected);
    }
}
