use std::ops::{Index, IndexMut};

use num_traits::ToBytes;

// use crate::instruction::{EffectiveAddr, Instruction, Mod00EffectiveAddr, MovOperand, Reg16, Reg8};

#[derive(Debug, PartialEq, Eq)]
pub struct Cpu {
    reg: [u16; 8],
    memory: [u8; 1024], // 1024 bytes
    instruction_pointer: u16,
    flags: u16,
    code_segment: u16,
    data_segment: u16,
    stack_segment: u16,
    extra_segment: u16,
}

// impl Index<Reg16> for Cpu {
//     type Output = u16;
//     fn index(&self, idx: Reg16) -> &Self::Output {
//         let idx = idx as u8;
//         &self.reg[idx as usize]
//     }
// }

// impl IndexMut<Reg16> for Cpu {
//     fn index_mut(&mut self, idx: Reg16) -> &mut Self::Output {
//         let idx = idx as u8;
//         &mut self.reg[idx as usize]
//     }
// }

// impl Index<Reg8> for Cpu {
//     type Output = u8;
//     fn index(&self, idx: Reg8) -> &Self::Output {
//         let bytes = bytemuck::bytes_of(&self.reg);
//         &bytes[idx as u8 as usize]
//     }
// }
// impl IndexMut<Reg8> for Cpu {
//     fn index_mut(&mut self, idx: Reg8) -> &mut Self::Output {
//         let bytes = bytemuck::bytes_of_mut(&mut self.reg);
//         &mut bytes[idx as u8 as usize]
//     }
// }

// impl Default for Cpu {
//     fn default() -> Self {
//         Self::new()
//     }
// }

// impl Cpu {
//     #[must_use]
//     pub const fn new() -> Self {
//         Self {
//             reg: [0_u16; 8],
//             instruction_pointer: 0,
//             flags: 0,
//             code_segment: 0,
//             data_segment: 0,
//             stack_segment: 0,
//             extra_segment: 0,
//             memory: [0_u8; 1024],
//         }
//     }

//     pub fn execute(&mut self, asm: &[u8]) {
//         let ixs = crate::instruction::InstructionSet::from_bytes(asm);
//         for (ix_idx, size, ix) in ixs.instructions {
//             dbg!(ix_idx);
//             use MovOperand::*;
//             self.instruction_pointer += size as u16;
//             match ix {
//                 Instruction::MovRegMemWithRegToEither { dest, source } => match (dest, source) {
//                     (Reg8(dest_reg8), Reg8(src_reg8)) => {
//                         self[dest_reg8] = self[src_reg8];
//                     }
//                     (Reg8(dest_reg8), Reg16(src_reg16)) => {
//                         self[dest_reg8] = self[src_reg16].to_le_bytes()[0];
//                     }
//                     (Reg8(dest_reg8), Mod00(src_mod00_effective_addr)) => todo!(),
//                     (Reg8(dest_reg8), Mod01(src_effective_addr)) => todo!(),
//                     (Reg8(dest_reg8), Mod10(src_effective_addr)) => todo!(),
//                     (Reg16(dest_reg16), Reg8(src_reg8)) => {
//                         self[dest_reg16] = self[src_reg8] as u16;
//                     }
//                     (Reg16(dest_reg16), Reg16(src_reg16)) => {
//                         self[dest_reg16] = self[src_reg16];
//                     }
//                     (Reg16(dest_reg16), Mod00(src_mod00_effective_addr)) => {
//                         match src_mod00_effective_addr {
//                             Mod00EffectiveAddr::BxPlusSi => todo!(),
//                             Mod00EffectiveAddr::BxPlusDi => todo!(),
//                             Mod00EffectiveAddr::BPPlusSi => todo!(),
//                             Mod00EffectiveAddr::BPPlusDi => todo!(),
//                             Mod00EffectiveAddr::Si => todo!(),
//                             Mod00EffectiveAddr::Di => todo!(),
//                             Mod00EffectiveAddr::DirectAddr(addr) => {
//                                 let val = self.get_mem_u16(addr);
//                                 self[dest_reg16] = val;
//                             }
//                             Mod00EffectiveAddr::Bx => todo!(),
//                         }
//                     }
//                     (Reg16(dest_reg16), Mod01(src_effective_addr)) => todo!(),
//                     (Reg16(dest_reg16), Mod10(src_effective_addr)) => todo!(),
//                     (Mod00(dest_mod00_effective_addr), Reg8(src_reg8)) => todo!(),
//                     (Mod00(dest_mod00_effective_addr), Reg16(src_reg16)) => todo!(),
//                     (Mod00(dest_mod00_effective_addr), Mod00(src_mod00_effective_addr)) => todo!(),
//                     (Mod00(dest_mod00_effective_addr), Mod01(src_effective_addr)) => todo!(),
//                     (Mod00(dest_mod00_effective_addr), Mod10(src_effective_addr)) => todo!(),
//                     (Mod01(dest_effective_addr), Reg8(src_reg8)) => todo!(),
//                     (Mod01(dest_effective_addr), Reg16(src_reg16)) => todo!(),
//                     (Mod01(dest_effective_addr), Mod00(src_mod00_effective_addr)) => todo!(),
//                     (Mod01(dest_effective_addr), Mod01(src_effective_addr)) => {
//                         todo!()
//                     }
//                     (Mod01(dest_effective_addr), Mod10(src_effective_addr)) => {
//                         todo!()
//                     }
//                     (Mod10(dest_effective_addr), Reg8(src_reg8)) => todo!(),
//                     (Mod10(dest_effective_addr), Reg16(src_reg16)) => todo!(),
//                     (Mod10(dest_effective_addr), Mod00(src_mod00_effective_addr)) => todo!(),
//                     (Mod10(dest_effective_addr), Mod01(src_effective_addr)) => {
//                         todo!()
//                     }
//                     (Mod10(dest_effective_addr), Mod10(src_effective_addr)) => {
//                         todo!()
//                     }
//                 },
//                 Instruction::MovImmToReg8 { dest, source } => {
//                     self[dest] = source;
//                 }
//                 Instruction::MovImmToReg16 { dest, source } => {
//                     let val = (u16::from(source.1) << 8) | u16::from(source.0);
//                     self[dest] = val;
//                 }
//                 Instruction::MovImmToMemory { dest, source } => match dest {
//                     Reg8(reg8) => {
//                         self[reg8] = source.0;
//                     }
//                     Reg16(reg16) => {
//                         let val = (u16::from(source.1.unwrap()) << 8) | u16::from(source.0);
//                         self[reg16] = val;
//                     }
//                     Mod00(mod00_effective_addr) => match mod00_effective_addr {
//                         Mod00EffectiveAddr::BxPlusSi => todo!(),
//                         Mod00EffectiveAddr::BxPlusDi => todo!(),
//                         Mod00EffectiveAddr::BPPlusSi => todo!(),
//                         Mod00EffectiveAddr::BPPlusDi => todo!(),
//                         Mod00EffectiveAddr::Si => todo!(),
//                         Mod00EffectiveAddr::Di => todo!(),
//                         Mod00EffectiveAddr::DirectAddr(addr) => {
//                             let val = (u16::from(source.1.unwrap()) << 8) | u16::from(source.0);
//                             self.store_val_to_mem_addr(addr, val);
//                         }
//                         Mod00EffectiveAddr::Bx => {
//                             let val = (u16::from(source.1.unwrap()) << 8) | u16::from(source.0);
//                             self[crate::instruction::Reg16::BX] = val;
//                         }
//                     },
//                     Mod01(effective_addr) => match effective_addr {
//                         EffectiveAddr::BxPlusSi(_) => todo!(),
//                         EffectiveAddr::BxPlusDi(_) => todo!(),
//                         EffectiveAddr::BPPlusSi(_) => todo!(),
//                         EffectiveAddr::BPPlusDi(_) => todo!(),
//                         EffectiveAddr::Si(_) => todo!(),
//                         EffectiveAddr::Di(_) => todo!(),
//                         EffectiveAddr::Bp(_) => todo!(),
//                         EffectiveAddr::Bx(offset) => {
//                             let memory_addr = self.bx_offset_mem_addr(offset).to_le_bytes();
//                             self.store_val_to_mem_addr(
//                                 (memory_addr[0], memory_addr[1]),
//                                 u16::from_le_bytes([source.0, source.1.unwrap()]),
//                             );
//                         }
//                     },
//                     Mod10(effective_addr) => todo!(),
//                 },
//                 Instruction::MovMemoryToAccumulator { dest, source } => todo!(),
//                 Instruction::MovAccumulatorToMemory { source, dest } => todo!(),
//                 _ => todo!(),
//             };
//             dbg!(&self.reg);
//         }
//     }

//     fn bx_offset_mem_addr(&mut self, offset: u8) -> u16 {
//         let bx = self[crate::instruction::Reg16::BX];
//         let memory_addr = bx + offset as u16;
//         memory_addr
//     }

//     fn store_val_to_mem_addr(&mut self, addr: (u8, u8), value: u16) {
//         let value = value.to_le_bytes();
//         let memory_addr = ((u16::from(addr.1) << 8) | u16::from(addr.0)) as usize;

//         // Store lower byte
//         self.memory[memory_addr] = value[0];

//         // Store higher byte
//         self.memory[memory_addr + 1] = value[1];
//     }

//     fn get_mem_u16(&self, addr: (u8, u8)) -> u16 {
//         let memory_addr = ((u16::from(addr.1) << 8) | u16::from(addr.0)) as usize;

//         let lower_byte = self.memory[memory_addr];
//         let higher_byte = self.memory[memory_addr + 1];

//         u16::from_le_bytes([lower_byte, higher_byte])
//     }
// }

// #[cfg(test)]
// mod tests {
//     use crate::instruction::Reg16;

//     use super::Cpu;

//     fn read_and_test(test: &str) -> Cpu {
//         let asm = std::fs::read(format!("fixtures/{test:}")).unwrap();
//         let mut cpu = Cpu::new();
//         cpu.execute(&asm);
//         cpu
//     }

//     // todo -- listing 43, 44, opt 45
//     #[test]
//     fn test_listing_0043() {
//         let test = "listing_0043_immediate_movs";
//         let result = read_and_test(test);
//         let mut expected_cpu = Cpu::new();
//         expected_cpu[Reg16::AX] = 1;
//         expected_cpu[Reg16::BX] = 2;
//         expected_cpu[Reg16::CX] = 3;
//         expected_cpu[Reg16::DX] = 4;
//         expected_cpu[Reg16::SP] = 5;
//         expected_cpu[Reg16::BP] = 6;
//         expected_cpu[Reg16::SI] = 7;
//         expected_cpu[Reg16::DI] = 8;
//         expected_cpu.instruction_pointer = 24;

//         // just copy over the memory contents, we only arssert regs
//         expected_cpu.memory = result.memory;

//         assert_eq!(result, expected_cpu);
//     }

//     #[test]
//     fn test_listing_0044() {
//         let test = "listing_0044_register_movs";
//         let result = read_and_test(test);
//         let mut expected_cpu = Cpu::new();
//         expected_cpu[Reg16::AX] = 4;
//         expected_cpu[Reg16::BX] = 3;
//         expected_cpu[Reg16::CX] = 2;
//         expected_cpu[Reg16::DX] = 1;
//         expected_cpu[Reg16::SP] = 1;
//         expected_cpu[Reg16::BP] = 2;
//         expected_cpu[Reg16::SI] = 3;
//         expected_cpu[Reg16::DI] = 4;
//         expected_cpu.instruction_pointer = 28;

//         // just copy over the memory contents, we only arssert regs
//         expected_cpu.memory = result.memory;

//         assert_eq!(result, expected_cpu);
//     }

//     #[test]
//     fn test_listing_0045() {
//         let test = "listing_0045_challenge_register_movs";
//         let result = read_and_test(test);
//         let mut expected_cpu = Cpu::new();
//         expected_cpu[Reg16::AX] = 17425;
//         expected_cpu[Reg16::BX] = 13124;
//         expected_cpu[Reg16::CX] = 26231;
//         expected_cpu[Reg16::DX] = 30600;
//         expected_cpu[Reg16::SP] = 17425;
//         expected_cpu[Reg16::BP] = 13124;
//         expected_cpu[Reg16::SI] = 26231;
//         expected_cpu[Reg16::DI] = 30600;
//         expected_cpu.extra_segment = 26231;
//         expected_cpu.stack_segment = 17425;
//         expected_cpu.data_segment = 13124;
//         expected_cpu.instruction_pointer = 28;

//         // just copy over the memory contents, we only arssert regs
//         expected_cpu.memory = result.memory;

//         assert_eq!(result, expected_cpu);
//     }

//     #[test]
//     fn test_listing_0051() {
//         let test = "listing_0051_memory_mov";
//         let result = read_and_test(test);
//         let mut expected_cpu = Cpu::new();
//         expected_cpu[Reg16::BX] = 1;
//         expected_cpu[Reg16::CX] = 2;
//         expected_cpu[Reg16::DX] = 10;
//         expected_cpu[Reg16::BP] = 4;
//         expected_cpu.instruction_pointer = 48;

//         // just copy over the memory contents, we only arssert regs
//         expected_cpu.memory = result.memory;

//         assert_eq!(result, expected_cpu);
//     }
// }
