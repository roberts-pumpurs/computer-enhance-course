use crate::instruction::Instruction;

use super::{InstructionSet, Register};

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
    pub fn from_chars(left: char, right: char) -> Register {
        match (left, right) {
            ('a', 'l' | 'x') => Register::R000,
            ('c', 'l' | 'x') => Register::R001,
            ('d', 'l' | 'x') => Register::R010,
            ('b', 'l' | 'x') => Register::R100,
            ('a', 'h') | ('s', 'p') => Register::R011,
            ('c', 'h') | ('b', 'p') => Register::R101,
            ('d', 'h') | ('s', 'i') => Register::R110,
            ('b', 'h') | ('d', 'i') => Register::R111,
            _ => {
                panic!("unexpected chars")
            }
        }
    }

    pub fn to_chars(&self, w: bool) -> (char, char) {
        match (self, w) {
            (Register::R000, false) => ('a', 'l'),
            (Register::R000, true) => ('a', 'x'),
            (Register::R001, false) => ('c', 'l'),
            (Register::R001, true) => ('c', 'x'),
            (Register::R010, false) => ('d', 'l'),
            (Register::R010, true) => ('d', 'x'),
            (Register::R100, false) => ('b', 'l'),
            (Register::R100, true) => ('b', 'x'),
            (Register::R011, false) => ('a', 'h'),
            (Register::R011, true) => ('s', 'p'),
            (Register::R101, false) => ('c', 'h'),
            (Register::R101, true) => ('b', 'p'),
            (Register::R110, false) => ('d', 'h'),
            (Register::R110, true) => ('s', 'i'),
            (Register::R111, false) => ('b', 'h'),
            (Register::R111, true) => ('d', 'i'),
        }
    }
    pub fn from_str(reg: &str) -> Self {
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
        let content = std::fs::read_to_string("fixtures/single_reg_mov.asm").unwrap();
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
