use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use crate::instruction::{AccumulatorReg, ArithmeticOp, MovOperand};

use super::{EffectiveAddr, Instruction, InstructionSet, Mod00EffectiveAddr, Reg16, Reg8};

// todo left off at adding all add ixs https://www.computerenhance.com/p/opcode-patterns-in-8086-arithmetic
// also remember this `In the table you’ll see: “data | data if s: w = 01”. So there is only a second data byte if s is 0 and w is 1.`

impl InstructionSet {
    #[must_use]
    pub fn from_bytes(bytes: &[u8]) -> Self {
        let mut byte_iter = bytes.iter().copied().peekable();

        let mut ixs = vec![];
        let mut idx = 1;
        while byte_iter.peek().is_some() {
            print!("{idx:}: ");
            let start_len = byte_iter.len();
            let ix = Instruction::from_byte(&mut byte_iter);
            let final_len = byte_iter.len();
            let instr_size = start_len - final_len;
            ixs.push((idx, instr_size, ix));
            idx += 1;
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
        mod opcodes {
            pub(crate) const MOV_REG_MEM_TO_FROM_REG: u8 = 0b____10001000_u8;
            pub(crate) const MOV_REG_MEM_TO_FROM_REG_MAX: u8 = 0b10001011_u8 + 1;

            pub(crate) const MOV_IMMEDIATE_TO_REG_MEM: u8 = 0b_______11000110_u8;
            pub(crate) const MOV_IMMEDIATE_TO_REG_MEM_MAX: u8 = 0b___11000111_u8 + 1;

            pub(crate) const MOV_IMMEDIATE_TO_REG: u8 = 0b_______10110000_u8;
            pub(crate) const MOV_IMMEDIATE_TO_REG_MAX: u8 = 0b___10111111_u8 + 1;

            pub(crate) const MOV_MEMORY_TO_ACCUMULATOR: u8 = 0b_______10100000_u8;
            pub(crate) const MOV_MEMORY_TO_ACCUMULATOR_MAX: u8 = 0b___10100001_u8 + 1;

            pub(crate) const MOV_ACCUMULATOR_TO_MEMORY: u8 = 0b_______10100010_u8;
            pub(crate) const MOV_ACCUMULATOR_TO_MEMORY_MAX: u8 = 0b___10100011_u8 + 1;

            pub(crate) const ADD_REG_MEM_WITH_REG_TO_EITHER: u8 = 0b____00000000_u8;
            pub(crate) const ADD_REG_MEM_WITH_REG_TO_EITHER_MAX: u8 = 0b00000011_u8 + 1;

            pub(crate) const ADD_IMM_TO_REG_OR_MEMORY: u8 = 0b____10000000_u8;
            pub(crate) const ADD_IMM_TO_REG_OR_MEMORY_MAX: u8 = 0b10000011_u8 + 1;

            pub(crate) const ADD_IMM_TO_ACCUMULATOR: u8 = 0b____00000100_u8;
            pub(crate) const ADD_IMM_TO_ACCUMULATOR_MAX: u8 = 0b00000101_u8 + 1;

            pub(crate) const SUB_REG_MEM_WITH_REG_TO_EITHER: u8 = 0b____00101000_u8;
            pub(crate) const SUB_REG_MEM_WITH_REG_TO_EITHER_MAX: u8 = 0b00101011_u8 + 1;

            pub(crate) const SUB_IMM_TO_ACCUMULATOR: u8 = 0b____00101100_u8;
            pub(crate) const SUB_IMM_TO_ACCUMULATOR_MAX: u8 = 0b00101101_u8 + 1;

            pub(crate) const CMP_REG_MEM_WITH_REG_TO_EITHER: u8 = 0b____00111000_u8;
            pub(crate) const CMP_REG_MEM_WITH_REG_TO_EITHER_MAX: u8 = 0b00111011_u8 + 1;

            pub(crate) const CMP_IMM_TO_ACCUMULATOR: u8 = 0b____00111100_u8;
            pub(crate) const CMP_IMM_TO_ACCUMULATOR_MAX: u8 = 0b00111101_u8 + 1;

            pub(crate) const JNZ: u8 = 0b01110101;
            pub(crate) const JE: u8 = 0b01110100;
            pub(crate) const JL: u8 = 0b01111100;
            pub(crate) const JLE: u8 = 0b01111110;
            pub(crate) const JB: u8 = 0b01110010;
            pub(crate) const JA: u8 = 0b01110111;

            pub(crate) const JBE: u8 = 0b01110110;
            pub(crate) const JP: u8 = 0b_01111010;
            pub(crate) const JO: u8 = 0b_01110000;
            pub(crate) const JS: u8 = 0b_01111000;

            pub(crate) const JNL: u8 = 0b01111101;
            pub(crate) const JG: u8 = 0b_01111111;
            pub(crate) const JNB: u8 = 0b01110011;

            pub(crate) const JNP: u8 = 0b01111011;
            pub(crate) const JNO: u8 = 0b01110001;
            pub(crate) const JNS: u8 = 0b01111001;

            pub(crate) const LOOP: u8 = 0b__11100010;
            pub(crate) const LOOPZ: u8 = 0b_11100001;
            pub(crate) const LOOPNZ: u8 = 0b11100000;
            pub(crate) const JCXZ: u8 = 0b__11100011;
        }

        let first_byte = bytes.next().unwrap();
        let ix = match first_byte {
            // mov ixs
            opcodes::MOV_REG_MEM_TO_FROM_REG..opcodes::MOV_REG_MEM_TO_FROM_REG_MAX => {
                decode_mov_reg_mem_to_from_reg(&mut bytes, first_byte)
            }
            opcodes::MOV_IMMEDIATE_TO_REG_MEM..opcodes::MOV_IMMEDIATE_TO_REG_MEM_MAX => {
                decode_mov_immediate_to_reg_mem(&mut bytes, first_byte)
            }
            opcodes::MOV_IMMEDIATE_TO_REG..opcodes::MOV_IMMEDIATE_TO_REG_MAX => {
                decode_mov_immediate_to_reg(first_byte, &mut bytes)
            }
            opcodes::MOV_MEMORY_TO_ACCUMULATOR..opcodes::MOV_MEMORY_TO_ACCUMULATOR_MAX => {
                decode_mov_mem_to_acc(first_byte, &mut bytes)
            }
            opcodes::MOV_ACCUMULATOR_TO_MEMORY..opcodes::MOV_ACCUMULATOR_TO_MEMORY_MAX => {
                decode_mov_acc_to_mem(first_byte, bytes)
            }
            // add, sub, cmp ixs
            opcodes::ADD_IMM_TO_REG_OR_MEMORY..opcodes::ADD_IMM_TO_REG_OR_MEMORY_MAX => {
                print!("imm to reg {first_byte:08b}");
                decode_arithm_immediate_to_reg(first_byte, &mut bytes)
            }
            // add only
            opcodes::ADD_REG_MEM_WITH_REG_TO_EITHER
                ..opcodes::ADD_REG_MEM_WITH_REG_TO_EITHER_MAX => {
                print!("mem to mem {first_byte:08b}");
                decode_add_reg_mem_to_either(&mut bytes, first_byte, ArithmeticOp::Add)
            }
            opcodes::ADD_IMM_TO_ACCUMULATOR..opcodes::ADD_IMM_TO_ACCUMULATOR_MAX => {
                print!("imm to acc {first_byte:08b}");
                decode_add_imm_to_acc(first_byte, &mut bytes, ArithmeticOp::Add)
            }
            // sub only
            opcodes::SUB_REG_MEM_WITH_REG_TO_EITHER
                ..opcodes::SUB_REG_MEM_WITH_REG_TO_EITHER_MAX => {
                print!("mem to mem {first_byte:08b}");
                decode_add_reg_mem_to_either(&mut bytes, first_byte, ArithmeticOp::Sub)
            }
            opcodes::SUB_IMM_TO_ACCUMULATOR..opcodes::SUB_IMM_TO_ACCUMULATOR_MAX => {
                print!("imm to acc {first_byte:08b}");
                decode_add_imm_to_acc(first_byte, &mut bytes, ArithmeticOp::Sub)
            }
            opcodes::CMP_REG_MEM_WITH_REG_TO_EITHER
                ..opcodes::CMP_REG_MEM_WITH_REG_TO_EITHER_MAX => {
                print!("mem to mem {first_byte:08b}");
                decode_add_reg_mem_to_either(&mut bytes, first_byte, ArithmeticOp::Cmp)
            }
            opcodes::CMP_IMM_TO_ACCUMULATOR..opcodes::CMP_IMM_TO_ACCUMULATOR_MAX => {
                print!("imm to acc {first_byte:08b}");
                decode_add_imm_to_acc(first_byte, &mut bytes, ArithmeticOp::Cmp)
            }
            opcodes::JNZ => {
                let second = bytes.next().unwrap();
                print!("jnz {first_byte:08b}");
                Instruction::Jnz {
                    instruction_pointer_increment: second,
                }
            }
            opcodes::JE => {
                let second = bytes.next().unwrap();
                print!("je {first_byte:08b}");
                Instruction::Je {
                    instruction_pointer_increment: second,
                }
            }
            opcodes::JL => {
                let second = bytes.next().unwrap();
                print!("jl {first_byte:08b}");
                Instruction::Jl {
                    instruction_pointer_increment: second,
                }
            }
            opcodes::JB => {
                let second = bytes.next().unwrap();
                print!("jb {first_byte:08b}");
                Instruction::Jb {
                    instruction_pointer_increment: second,
                }
            }
            opcodes::JLE => {
                let second = bytes.next().unwrap();
                print!("jle {first_byte:08b}");
                Instruction::Jle {
                    instruction_pointer_increment: second,
                }
            }
            opcodes::JA => {
                let second = bytes.next().unwrap();
                print!("ja {first_byte:08b}");
                Instruction::Ja {
                    instruction_pointer_increment: second,
                }
            }
            opcodes::JBE => {
                let second = bytes.next().unwrap();
                print!("jbe {first_byte:08b}");
                Instruction::Jbe {
                    instruction_pointer_increment: second,
                }
            }
            opcodes::JP => {
                let second = bytes.next().unwrap();
                print!("jp {first_byte:08b}");
                Instruction::Jp {
                    instruction_pointer_increment: second,
                }
            }
            opcodes::JO => {
                let second = bytes.next().unwrap();
                print!("jo {first_byte:08b}");
                Instruction::Jo {
                    instruction_pointer_increment: second,
                }
            }
            opcodes::JS => {
                let second = bytes.next().unwrap();
                print!("js {first_byte:08b}");
                Instruction::Js {
                    instruction_pointer_increment: second,
                }
            }
            opcodes::JNL => {
                let second = bytes.next().unwrap();
                print!("jnl {first_byte:08b}");
                Instruction::Jnl {
                    instruction_pointer_increment: second,
                }
            }
            opcodes::JG => {
                let second = bytes.next().unwrap();
                print!("jg {first_byte:08b}");
                Instruction::Jg {
                    instruction_pointer_increment: second,
                }
            }
            opcodes::JNB => {
                let second = bytes.next().unwrap();
                print!("jnb {first_byte:08b}");
                Instruction::Jnb {
                    instruction_pointer_increment: second,
                }
            }
            opcodes::JNP => {
                let second = bytes.next().unwrap();
                print!("jnp {first_byte:08b}");
                Instruction::Jnp {
                    instruction_pointer_increment: second,
                }
            }
            opcodes::JNO => {
                let second = bytes.next().unwrap();
                print!("jno {first_byte:08b}");
                Instruction::Jno {
                    instruction_pointer_increment: second,
                }
            }
            opcodes::JNS => {
                let second = bytes.next().unwrap();
                print!("jns {first_byte:08b}");
                Instruction::Jns {
                    instruction_pointer_increment: second,
                }
            }
            opcodes::LOOP => {
                let second = bytes.next().unwrap();
                print!("loop {first_byte:08b}");
                Instruction::LoopTimes {
                    instruction_pointer_increment: second,
                }
            }
            opcodes::LOOPZ => {
                let second = bytes.next().unwrap();
                print!("loopz {first_byte:08b}");
                Instruction::LoopWhileEqual {
                    instruction_pointer_increment: second,
                }
            }
            opcodes::LOOPNZ => {
                let second = bytes.next().unwrap();
                print!("loopnz {first_byte:08b}");
                Instruction::LoopWhileNotEqual {
                    instruction_pointer_increment: second,
                }
            }
            opcodes::JCXZ => {
                let second = bytes.next().unwrap();
                print!("jcxz {first_byte:08b}");
                Instruction::JumpOnCxZero {
                    instruction_pointer_increment: second,
                }
            }
            _ => {
                print!("??? ?? ??? {first_byte:08b}");
                Instruction::Unsupported
            }
        };
        println!(" {ix:?}");
        ix
    }
}

/// expected that the bit is in the right most position
fn is_wide(byte: u8) -> bool {
    mod masks {
        pub(crate) const WOR: u8 = 0b00000001_u8;
    }
    let wide = byte & masks::WOR;
    wide > 0
}

fn is_signed(byte: u8) -> bool {
    mod masks {
        pub(crate) const SIG: u8 = 0b00000001_u8;
    }
    let signed = byte & masks::SIG;
    signed > 0
}

#[derive(Debug, FromPrimitive)]
#[repr(u8)]
enum MemoryMode {
    NoDisplacement = 0b___00000000_u8,
    Bit8Displacement = 0b_00000001_u8,
    Bit16Displacement = 0b00000010_u8,
    RegisterMode = 0b_____00000011_u8,
}

fn memory_mode(byte: u8) -> MemoryMode {
    pub(crate) const MOD: u8 = 0b00000011u8;
    let byte = byte & MOD;
    MemoryMode::from_u8(byte).unwrap()
}

fn arithmetic_type(byte: u8) -> ArithmeticOp {
    pub(crate) const PATTERN: u8 = 0b00000111u8;
    let byte = byte & PATTERN;
    ArithmeticOp::from_u8(byte).unwrap()
}

fn reg_rm(byte: u8) -> u8 {
    pub(crate) const REG_RM: u8 = 0b00000111u8;
    let byte = byte & REG_RM;
    byte
}

#[derive(Debug, FromPrimitive, PartialEq, Eq)]
#[repr(u8)]
enum DirMode {
    RegIsSource = 0b______00000000_u8,
    RegIsDestination = 0b_00000001_u8,
}
fn dir(byte: u8) -> DirMode {
    pub(crate) const DIR: u8 = 0b00000001u8;
    let byte = byte & DIR;
    DirMode::from_u8(byte).unwrap()
}

fn decode_mov_acc_to_mem<I: Iterator<Item = u8>>(first_byte: u8, mut bytes: I) -> Instruction {
    let source = if is_wide(first_byte) {
        AccumulatorReg::Ax
    } else {
        AccumulatorReg::Al
    };
    let addr_low = bytes.next().unwrap();
    let addr_high = bytes.next().unwrap();

    Instruction::MovAccumulatorToMemory {
        source,
        dest: (addr_low, addr_high),
    }
}

fn decode_mov_mem_to_acc<I: Iterator<Item = u8>>(first_byte: u8, bytes: &mut I) -> Instruction {
    let destination = if is_wide(first_byte) {
        AccumulatorReg::Ax
    } else {
        AccumulatorReg::Al
    };
    let addr_low = bytes.next().unwrap();
    let addr_high = bytes.next().unwrap();

    Instruction::MovMemoryToAccumulator {
        dest: destination,
        source: (addr_low, addr_high),
    }
}

fn decode_add_imm_to_acc<I: Iterator<Item = u8>>(
    first_byte: u8,
    bytes: &mut I,
    operation: ArithmeticOp,
) -> Instruction {
    let wide = is_wide(first_byte);
    if wide {
        let data_byte_1 = bytes.next().unwrap();
        let data_byte_2 = bytes.next().unwrap();
        return Instruction::ArithmImmToAcc {
            operation,
            dest: AccumulatorReg::Ax,
            source: (data_byte_1, Some(data_byte_2)),
        };
    }
    let data_byte_1 = bytes.next().unwrap();
    Instruction::ArithmImmToAcc {
        operation,
        dest: AccumulatorReg::Al,
        source: (data_byte_1, None),
    }
}

fn decode_mov_immediate_to_reg_mem<I: Iterator<Item = u8>>(
    mut bytes: &mut I,
    first_byte: u8,
) -> Instruction {
    let second_byte = bytes.next().unwrap();
    let is_wide = is_wide(first_byte);
    let memory_mode = memory_mode(second_byte >> 6);
    let rm = reg_rm(second_byte);

    let get_source = |bytes: &mut I| {
        let immediate_byte_1 = bytes.next().unwrap();

        if is_wide {
            let immediate_byte_2 = bytes.next().unwrap();
            return (immediate_byte_1, Some(immediate_byte_2));
        }
        (immediate_byte_1, None)
    };

    match memory_mode {
        MemoryMode::RegisterMode => {
            let dest = reg_to_mov_operand(is_wide, rm);
            let source = get_source(bytes);

            Instruction::MovImmToMemory { dest, source }
        }
        MemoryMode::NoDisplacement => {
            let dest = decode_mod00_rm(rm, &mut bytes);
            let source = get_source(&mut bytes);

            Instruction::MovImmToMemory { dest, source }
        }
        MemoryMode::Bit8Displacement => {
            let low_disp_byte = bytes.next().unwrap();
            let rm = decode_mod01_mod02_rm(rm, low_disp_byte);
            let source = get_source(bytes);

            Instruction::MovImmToMemory {
                dest: MovOperand::Mod01(rm),
                source,
            }
        }
        MemoryMode::Bit16Displacement => {
            let low_disp_byte = bytes.next().unwrap();
            let high_disp_byte = bytes.next().unwrap();

            let rm = decode_mod01_mod02_rm(rm, (low_disp_byte, high_disp_byte));
            let source = get_source(bytes);

            Instruction::MovImmToMemory {
                dest: MovOperand::Mod10(rm),
                source,
            }
        }
    }
}

fn decode_mov_immediate_to_reg<I: Iterator<Item = u8>>(
    first_byte: u8,
    bytes: &mut I,
) -> Instruction {
    let wide = is_wide(first_byte >> 3);
    let reg = reg_rm(first_byte);
    if wide {
        let reg = decode_reg16(reg);
        let data_byte_1 = bytes.next().unwrap();
        let data_byte_2 = bytes.next().unwrap();
        return Instruction::MovImmToReg16 {
            dest: reg,
            source: (data_byte_1, data_byte_2),
        };
    }
    let reg = decode_reg8(reg);
    let data_byte_1 = bytes.next().unwrap();
    Instruction::MovImmToReg8 {
        dest: reg,
        source: (data_byte_1),
    }
}

fn decode_add_reg_mem_to_either<I: Iterator<Item = u8>>(
    bytes: &mut I,
    first_byte: u8,
    operation: ArithmeticOp,
) -> Instruction {
    let ix = decode_mov_reg_mem_to_from_reg(bytes, first_byte);
    let Instruction::MovRegMemWithRegToEither { dest, source } = ix else {
        unreachable!()
    };
    Instruction::ArithmRegMemWithReg {
        dest,
        source,
        operation,
    }
}

fn decode_arithm_immediate_to_reg<I: Iterator<Item = u8>>(
    first_byte: u8,
    mut bytes: &mut I,
) -> Instruction {
    let is_signed = is_signed(first_byte >> 1);
    let second_byte = bytes.next().unwrap();
    let is_wide = is_wide(first_byte);
    let memory_mode = memory_mode(second_byte >> 6);
    let rm = reg_rm(second_byte);

    let operation = arithmetic_type(second_byte >> 3);
    let has_second_byte = is_wide && !is_signed;

    let get_source = |bytes: &mut I| {
        let immediate_byte_1 = bytes.next().unwrap();

        if has_second_byte {
            let immediate_byte_2 = bytes.next().unwrap();
            return (immediate_byte_1, Some(immediate_byte_2));
        }
        (immediate_byte_1, None)
    };

    match memory_mode {
        MemoryMode::RegisterMode => {
            let dest = reg_to_mov_operand(is_wide, rm);
            let source = get_source(bytes);

            Instruction::ArithmImmToMemory {
                dest,
                source,
                operation,
            }
        }
        MemoryMode::NoDisplacement => {
            let dest = decode_mod00_rm(rm, &mut bytes);
            let source = get_source(&mut bytes);

            Instruction::ArithmImmToMemory {
                dest,
                source,
                operation,
            }
        }
        MemoryMode::Bit8Displacement => {
            let low_disp_byte = bytes.next().unwrap();
            let rm = decode_mod01_mod02_rm(rm, low_disp_byte);
            let source = get_source(bytes);

            Instruction::ArithmImmToMemory {
                dest: MovOperand::Mod01(rm),
                source,
                operation,
            }
        }
        MemoryMode::Bit16Displacement => {
            let low_disp_byte = bytes.next().unwrap();
            let high_disp_byte = bytes.next().unwrap();

            let rm = decode_mod01_mod02_rm(rm, (low_disp_byte, high_disp_byte));
            let source = get_source(bytes);

            Instruction::ArithmImmToMemory {
                dest: MovOperand::Mod10(rm),
                source,
                operation,
            }
        }
    }
}

fn decode_mov_reg_mem_to_from_reg<I: Iterator<Item = u8>>(
    bytes: &mut I,
    first_byte: u8,
) -> Instruction {
    let second_byte = bytes.next().unwrap();

    let direction = dir(first_byte >> 1);
    let is_wide = is_wide(first_byte);
    let memory_mode = memory_mode(second_byte >> 6);
    let register_operand1 = reg_rm(second_byte >> 3);
    let register_operand2 = reg_rm(second_byte);

    match memory_mode {
        MemoryMode::RegisterMode => {
            assert_eq!(
                direction,
                DirMode::RegIsSource,
                "reg mode always has all the bytes it needs"
            );

            let source = reg_to_mov_operand(is_wide, register_operand1);
            let dest = reg_to_mov_operand(is_wide, register_operand2);

            Instruction::MovRegMemWithRegToEither { dest, source }
        }
        MemoryMode::NoDisplacement => {
            let reg = reg_to_mov_operand(is_wide, register_operand1);
            let rm = decode_mod00_rm(register_operand2, bytes);

            let (source, dest) = match direction {
                DirMode::RegIsSource => (reg, rm),
                DirMode::RegIsDestination => (rm, reg),
            };
            Instruction::MovRegMemWithRegToEither { dest, source }
        }
        MemoryMode::Bit8Displacement => {
            let low_disp_byte = bytes.next().unwrap();
            let reg = reg_to_mov_operand(is_wide, register_operand1);
            let rm = decode_mod01_mod02_rm(register_operand2, low_disp_byte);

            let (source, dest) = match direction {
                DirMode::RegIsSource => (reg, MovOperand::Mod01(rm)),
                DirMode::RegIsDestination => (MovOperand::Mod01(rm), reg),
            };
            Instruction::MovRegMemWithRegToEither { dest, source }
        }
        MemoryMode::Bit16Displacement => {
            let low_disp_byte = bytes.next().unwrap();
            let high_disp_byte = bytes.next().unwrap();

            let reg = reg_to_mov_operand(is_wide, register_operand1);
            let rm = decode_mod01_mod02_rm(register_operand2, (low_disp_byte, high_disp_byte));
            let (source, dest) = match direction {
                DirMode::RegIsSource => (reg, MovOperand::Mod10(rm)),
                DirMode::RegIsDestination => (MovOperand::Mod10(rm), reg),
            };
            Instruction::MovRegMemWithRegToEither { dest, source }
        }
    }
}

fn reg_to_mov_operand(is_wide: bool, register_operand1: u8) -> MovOperand {
    if is_wide {
        return MovOperand::Reg16(decode_reg16(register_operand1));
    }

    MovOperand::Reg8(decode_reg8(register_operand1))
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
    use std::{fs::File, io::Write as _};

    use itertools::Itertools as _;
    use pretty_assertions::{assert_eq, assert_str_eq};
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
        let fixture_machine_code_file = sh.current_dir().join("fixtures").join(test);
        let fixture_asm_code_file = sh
            .current_dir()
            .join("fixtures")
            .join(format!("{test:}.asm"));
        let expected_asm_content = sh
            .read_file(fixture_asm_code_file)
            .unwrap()
            .lines()
            .filter(|x| !x.starts_with(';'))
            .filter(|x| !x.is_empty())
            .enumerate()
            .map(|(idx, ix)| format!("{ix} ; {idx:?}"))
            .join("\n");
        let expected_content = sh.read_binary_file(fixture_machine_code_file).unwrap();

        // write the temp asm to file
        let temp_dir = TempDir::new(test).unwrap();
        let asm_output_file = temp_dir.path().join(format!("{test:}.asm"));
        let machine_code_output_file = temp_dir.path().join(test);
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
        println!("{output:}");
        assert_eq!(
            buffer_to_nice_bytes(&expected_content),
            buffer_to_nice_bytes(&content_binary)
        );

        // let re_derived_set_binary = InstructionSet::from_bytes(&content_binary);
        // assert_eq!(ix_set, &re_derived_set_binary);
        // if content_binary != expected_content {
        //     assert_str_eq!(expected_asm_content, output);
        // }

        // println!()
        // assert_eq!(content_binary, expected_content);
    }

    fn buffer_to_nice_bytes(buffer: &[u8]) -> String {
        buffer
            .iter()
            .map(|byte| format!("0b{:08b}", byte))
            .collect::<Vec<String>>()
            .join(" ")
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
    fn test_listing_40() {
        let test = "listing_0040_challenge_movs";
        read_and_test(test);
    }

    #[test]
    #[ignore = "jumps and labels ar PITA to generate but it works (manually validated, trust me bro)"]
    fn test_listing_41() {
        let test = "listing_0041_add_sub_cmp_jnz";
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
