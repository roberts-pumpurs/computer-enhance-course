use crate::instruction::Instruction;

use super::{InstructionSet, Register, Wide};

impl InstructionSet {
    pub fn from_decoded_asm_file(file_content: &str) -> Self {
        const BITS_PREFIX: usize = "bits ".len();
        let mut instructions = Vec::new();

        let mut lines = file_content
            .lines()
            .filter(|x| !x.starts_with(";"))
            .filter(|x| !x.is_empty());
        let bit_line = lines.next().unwrap();
        let bits = bit_line.split_at(BITS_PREFIX).1.parse().unwrap();

        for line in lines {
            let instr = Instruction::from_line(line);
            instructions.push(instr);
        }

        InstructionSet { instructions, bits }
    }
}

impl Instruction {
    pub fn from_line(line: &str) -> Self {
        let Some((ix, regs)) = line.split_once(" ") else {
            panic!("Invalid instruction: {}", line);
        };

        let Some((dest, source)) = regs.split_once(", ") else {
            panic!("Invalid instruction: {}", line);
        };

        let dest = Register::from_str(dest);
        let source = Register::from_str(source);

        match ix {
            "mov" => Instruction::Mov { dest, source },
            _ => panic!("Invalid instruction: {}", line),
        }
    }
}

impl Register {
    pub fn from_chars(left: char, right: char) -> (Register, Wide) {
        let is_wide = matches!(right, 'x' | 'p' | 'i');
        match (left, right) {
            ('a', 'l' | 'x') => (Register::AX, Wide(is_wide)),
            ('c', 'l' | 'x') => (Register::CX, Wide(is_wide)),
            ('d', 'l' | 'x') => (Register::DX, Wide(is_wide)),
            ('b', 'l' | 'x') => (Register::BX, Wide(is_wide)),
            ('a', 'h') | ('s', 'p') => (Register::SP, Wide(is_wide)),
            ('c', 'h') | ('b', 'p') => (Register::BP, Wide(is_wide)),
            ('d', 'h') | ('s', 'i') => (Register::SI, Wide(is_wide)),
            ('b', 'h') | ('d', 'i') => (Register::DI, Wide(is_wide)),
            _ => {
                panic!("unexpected chars")
            }
        }
    }

    pub fn to_chars(&self, w: Wide) -> (char, char) {
        let w = w.0;
        match (self, w) {
            (Register::AX, false) => ('a', 'l'),
            (Register::AX, true) => ('a', 'x'),
            (Register::CX, false) => ('c', 'l'),
            (Register::CX, true) => ('c', 'x'),
            (Register::DX, false) => ('d', 'l'),
            (Register::DX, true) => ('d', 'x'),
            (Register::BX, false) => ('b', 'l'),
            (Register::BX, true) => ('b', 'x'),
            (Register::SP, false) => ('a', 'h'),
            (Register::SP, true) => ('s', 'p'),
            (Register::SI, false) => ('c', 'h'),
            (Register::BP, true) => ('b', 'p'),
            (Register::BP, false) => ('d', 'h'),
            (Register::SI, true) => ('s', 'i'),
            (Register::DI, false) => ('b', 'h'),
            (Register::DI, true) => ('d', 'i'),
        }
    }
    pub fn from_str(reg: &str) -> (Self, Wide) {
        let mut chars = reg.chars();
        let first = chars.next().unwrap();
        let second = chars.next().unwrap();
        Register::from_chars(first, second)
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
