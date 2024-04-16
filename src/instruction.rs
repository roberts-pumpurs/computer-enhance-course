#[derive(Debug, PartialEq)]
pub struct InstructionSet {
    pub instructions: Vec<Instruction>,
    pub bits: u8,
}

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

#[derive(Debug, PartialEq)]
pub struct Register {
    repr: (char, char),
}

impl Register {
    pub fn from_str(reg: &str) -> Self {
        let mut chars = reg.chars();
        let first = chars.next().unwrap();
        let second = chars.next().unwrap();
        Register {
            repr: (first, second),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Instruction {
    Mov { dest: Register, source: Register },
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_ixs() {
        let content = std::fs::read_to_string("fixtures/single_reg_mov.asm").unwrap();
        let derived_set = InstructionSet::from_decoded_asm_file(&content);

        let expected = InstructionSet {
            instructions: vec![Instruction::Mov {
                dest: Register { repr: ('c', 'x') },
                source: Register { repr: ('b', 'x') },
            }],
            bits: 16,
        };

        assert_eq!(derived_set, expected);
    }
}
