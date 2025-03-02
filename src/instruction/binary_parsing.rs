use bitvec::prelude::*;

pub struct Instruction {
    size: u8,
    operation: &'static str,
    flags: u8,
    operands: [Operand; 2],
}

#[repr(u8)]
pub enum RegisterIndex {
    // 8-bit
    AL,
    CL,
    DL,
    BL,
    AH,
    CH,
    DH,
    BH,
    // 16-bit
    AX,
    CX,
    DX,
    BX,
    SP,
    BP,
    SI,
    DI,
}

enum Operand {
    None,
    Address(EffectiveAddressExpansion),
    Register(RegisterAccess),
    Immediate(Immediate),
}

pub struct IxDef<'a> {
    name: &'static str,
    items: Vec<P<'a>>,
}

// Map (reg_bits,w) -> a unique register index, in a 0..7 space for 8-bit or 16-bit
fn decode_8086_register_index(reg_bits: u8, w: bool) -> RegisterIndex {
    match (w, reg_bits) {
        (false, 0) => RegisterIndex::AL,
        (false, 1) => RegisterIndex::CL,
        (false, 2) => RegisterIndex::DL,
        (false, 3) => RegisterIndex::BL,
        (false, 4) => RegisterIndex::AH,
        (false, 5) => RegisterIndex::CH,
        (false, 6) => RegisterIndex::DH,
        (false, 7) => RegisterIndex::BH,

        (true, 0) => RegisterIndex::AX,
        (true, 1) => RegisterIndex::CX,
        (true, 2) => RegisterIndex::DX,
        (true, 3) => RegisterIndex::BX,
        (true, 4) => RegisterIndex::SP,
        (true, 5) => RegisterIndex::BP,
        (true, 6) => RegisterIndex::SI,
        (true, 7) => RegisterIndex::DI,
        _ => panic!("unknown register"),
    }
}

impl<'a> IxDef<'a> {
    pub const fn new(name: &'static str, items: Vec<P<'a>>) -> Self {
        Self { name, items }
    }

    /// return the decoded instruction and the subslice of input bytes from where to continue further parsing
    pub fn from_bytes<'b, 'c>(&'c self, bytes: &'b [u8]) -> Option<(Instruction, &'b [u8])> {
        let bits = bytes.view_bits::<Msb0>();
        let mut bit_offset = 0usize;

        // local fields
        let mut d_val: Option<bool> = None;
        let mut w_val: Option<bool> = None;
        let mut mod_val: Option<u8> = None;
        let mut reg_val: Option<u8> = None;
        let mut rm_val: Option<u8> = None;
        let mut data_lo: Option<u8> = None;
        let mut data_hi: Option<u8> = None;

        // Helper that returns a bit slice rather than a numeric
        let mut read_bits = |length: usize| -> Option<&BitSlice<u8, Msb0>> {
            if bit_offset + length > bits.len() {
                return None;
            }
            let slice = &bits[bit_offset..bit_offset + length];
            bit_offset += length;
            Some(slice)
        };

        for item in &self.items {
            match item {
                // We compare the next `pattern.len()` bits to `pattern`.
                P::C(pattern) => {
                    let len = pattern.len();
                    let candidate = read_bits(len)?;
                    if !candidate.eq(pattern) {
                        return None;
                    }
                }
                P::D => {
                    // Next bit is the D flag
                    let bit_slice = read_bits(1)?;
                    d_val = Some(bit_slice[0]);
                }
                P::W => {
                    // Next bit is the W flag
                    let bit_slice = read_bits(1)?;
                    w_val = Some(bit_slice[0]);
                }
                P::MOD => {
                    // Next 3 bits are the mod field
                    let slice = read_bits(3)?;
                    // convert that 3-bit slice to u8
                    let mut val = 0u8;
                    for bit in slice {
                        val = (val << 1) | (*bit as u8);
                    }
                    mod_val = Some(val);
                }
                P::REG => {
                    // Next 3 bits are the reg field
                    let slice = read_bits(3)?;
                    let mut val = 0u8;
                    for bit in slice {
                        val = (val << 1) | (*bit as u8);
                    }
                    reg_val = Some(val);
                }
                P::RM => {
                    // Next 2 bits are the rm field
                    let slice = read_bits(2)?;
                    let mut val = 0u8;
                    for bit in slice {
                        val = (val << 1) | (*bit as u8);
                    }
                    rm_val = Some(val);
                }
                P::DATA => {
                    // Next 8 bits is the lower data byte
                    let slice = read_bits(8)?;
                    let mut val = 0u8;
                    for bit in slice {
                        val = (val << 1) | (*bit as u8);
                    }
                    data_lo = Some(val);
                }
                P::DataIfW => {
                    // If W=1, read 8 more bits
                    if w_val == Some(true) {
                        let slice = read_bits(8)?;
                        let mut val = 0u8;
                        for bit in slice {
                            val = (val << 1) | (*bit as u8);
                        }
                        data_hi = Some(val);
                    }
                }
                P::ImplD(value) => {
                    d_val = Some(*value);
                }
            }
        }

        let bytes_consumed = (bit_offset + 7) / 8; // round up
        if bytes_consumed > bytes.len() {
            return None;
        }

        // Construct the instruction (minimal example)
        match self.name {
            "mov" => {
                let d_bit = if let Some(d) = d_val { d as u8 } else { 0 };
                let w_bit = if let Some(w) = w_val { w as u8 } else { 0 };
                let flags = (d_bit << 1) | w_bit;

                // todo decode the operands
                let inst = Instruction {
                    size: bytes_consumed as u8,
                    operation: self.name,
                    flags,
                    operands: [Operand::None, Operand::None],
                };

                Some((inst, &bytes[bytes_consumed..]))
            }
            _ => unimplemented!(),
        }
    }
}

/// parsable item
enum P<'a> {
    /// binary constant
    C(&'a BitSlice<u8, Msb0>),
    /// The data flag
    D,
    /// wide flag
    W,
    /// mod bits
    MOD,
    /// reg bits
    REG,
    /// RM bits
    RM,
    /// expect data byte to be present
    DATA,
    /// expect data byte to be present if W = 1
    DataIfW,
    /// implied value of D flag
    ImplD(bool),
}

impl<'a> P<'a> {
    fn bit_len(&self) -> u8 {
        match self {
            P::C(value) => {
                dbg!(value);
                value.len() as u8
            }
            P::D => 1,
            P::W => 1,
            P::MOD => 3,
            P::REG => 3,
            P::RM => 2,
            P::DATA => 8,
            P::DataIfW => 8,
            P::ImplD(_) => 0,
        }
    }
}

// inspiration: https://github.com/cmuratori/computer_enhance/blob/main/perfaware/sim86/sim86_instruction_table.inl
pub fn ix_table() -> [IxDef<'static>; 2] {
    use P::*;

    [
        IxDef::new(
            "mov",
            vec![
                C(bits!(static u8, Msb0; 1, 0, 0, 0, 1, 0)),
                D,
                W,
                MOD,
                REG,
                RM,
            ],
        ),
        IxDef::new(
            "mov",
            vec![
                C(bits!(static u8, Msb0; 1, 1, 0, 0, 0, 1, 1)),
                W,
                MOD,
                C(bits!(static u8, Msb0; 0, 0, 0)),
                RM,
                DATA,
                DataIfW,
                ImplD(true),
            ],
        ),
    ]
}

mod table_tests {
    use super::*;

    #[test]
    fn test_bit_len_constants() {
        // 0b100010 = 34 decimal => highest bit is the 6th from right
        assert_eq!(P::C(bits!(static u8, Msb0; 1,1,1,1,1,1)).bit_len(), 6);

        // 0b1 => 1 bit
        assert_eq!(P::C(bits!(static u8, Msb0; 1)).bit_len(), 1);

        // 0b0 => we treat 0 as at least 1 bit, per our .max(1) logic
        assert_eq!(P::C(bits!(static u8, Msb0; 0)).bit_len(), 1);

        // 0b0 => we treat 0 as at least 1 bit, per our .max(1) logic
        assert_eq!(P::C(bits!(static u8, Msb0; 0, 0, 0)).bit_len(), 3);

        // 0xFF => 8 bits
        assert_eq!(
            P::C(bits!(static u8, Msb0; 1, 1, 1 ,1 ,1, 1, 1, 1, )).bit_len(),
            8
        );

        // 0b10000000 => 128 decimal => 8 bits
        assert_eq!(
            P::C(bits!(static u8, Msb0; 1, 0, 0 ,0 ,0, 0, 0, 0)).bit_len(),
            8
        );
    }

    #[test]
    fn test_bit_len_flags() {
        assert_eq!(P::D.bit_len(), 1);
        assert_eq!(P::W.bit_len(), 1);
    }

    #[test]
    fn test_bit_len_mod_reg_rm() {
        assert_eq!(P::MOD.bit_len(), 3);
        assert_eq!(P::REG.bit_len(), 3);
        assert_eq!(P::RM.bit_len(), 2);
    }

    #[test]
    fn test_bit_len_data() {
        assert_eq!(P::DATA.bit_len(), 8);
        assert_eq!(P::DataIfW.bit_len(), 8);
    }

    #[test]
    fn test_bit_len_impld() {
        // ImplD doesn't consume bits
        assert_eq!(P::ImplD(true).bit_len(), 0);
        assert_eq!(P::ImplD(false).bit_len(), 0);
    }
}

// fn decode_reg8(reg: u8) -> Reg8 {
//     match reg {
//         0b000 => Reg8::AL,
//         0b001 => Reg8::CL,
//         0b010 => Reg8::DL,
//         0b011 => Reg8::BL,
//         0b100 => Reg8::AH,
//         0b101 => Reg8::CH,
//         0b110 => Reg8::DH,
//         0b111 => Reg8::BH,
//         _ => unreachable!(),
//     }
// }

// fn decode_reg16(reg: u8) -> Reg16 {
//     match reg {
//         0b000 => Reg16::AX,
//         0b001 => Reg16::CX,
//         0b010 => Reg16::DX,
//         0b011 => Reg16::BX,
//         0b100 => Reg16::SP,
//         0b101 => Reg16::BP,
//         0b110 => Reg16::SI,
//         0b111 => Reg16::DI,
//         _ => unreachable!(),
//     }
// }

// fn decode_mod00_rm<I: Iterator<Item = u8>>(rm: u8, mut bytes: I) -> MovOperand {
//     MovOperand::Mod00(match rm {
//         0b000 => Mod00EffectiveAddr::BxPlusSi,
//         0b001 => Mod00EffectiveAddr::BxPlusDi,
//         0b010 => Mod00EffectiveAddr::BPPlusSi,
//         0b011 => Mod00EffectiveAddr::BPPlusDi,
//         0b100 => Mod00EffectiveAddr::Si,
//         0b101 => Mod00EffectiveAddr::Di,
//         0b110 => Mod00EffectiveAddr::DirectAddr((bytes.next().unwrap(), bytes.next().unwrap())),
//         0b111 => Mod00EffectiveAddr::Bx,
//         _ => unreachable!(),
//     })
// }

// fn decode_mod01_mod02_rm<T>(rm: u8, addr: T) -> EffectiveAddr<T> {
//     match rm {
//         0b000 => EffectiveAddr::BxPlusSi(addr),
//         0b001 => EffectiveAddr::BxPlusDi(addr),
//         0b010 => EffectiveAddr::BPPlusSi(addr),
//         0b011 => EffectiveAddr::BPPlusDi(addr),
//         0b100 => EffectiveAddr::Si(addr),
//         0b101 => EffectiveAddr::Di(addr),
//         0b110 => EffectiveAddr::Bp(addr),
//         0b111 => EffectiveAddr::Bx(addr),
//         _ => unreachable!(),
//     }
// }

// #[cfg(test)]
// mod tests {
//     use std::{fs::File, io::Write as _};

//     use itertools::Itertools as _;
//     use pretty_assertions::{assert_eq, assert_str_eq};
//     use tempdir::TempDir;
//     use xshell::cmd;

//     use super::*;

//     fn read_and_test(test: &str) {
//         let content_binary = std::fs::read(format!("fixtures/{test:}")).unwrap();
//         let derived_set_binary = InstructionSet::from_bytes(&content_binary);

//         generate_and_compare_machine_code(test, &derived_set_binary);
//     }

//     fn generate_and_compare_machine_code(test: &str, ix_set: &InstructionSet) {
//         let output = format!("{ix_set:}");
//         let sh = xshell::Shell::new().unwrap();
//         let fixture_machine_code_file = sh.current_dir().join("fixtures").join(test);
//         let fixture_asm_code_file = sh
//             .current_dir()
//             .join("fixtures")
//             .join(format!("{test:}.asm"));
//         let expected_asm_content = sh
//             .read_file(fixture_asm_code_file)
//             .unwrap()
//             .lines()
//             .filter(|x| !x.starts_with(';'))
//             .filter(|x| !x.is_empty())
//             .enumerate()
//             .map(|(idx, ix)| format!("{ix} ; {idx:?}"))
//             .join("\n");
//         let expected_content = sh.read_binary_file(fixture_machine_code_file).unwrap();

//         // write the temp asm to file
//         let temp_dir = TempDir::new(test).unwrap();
//         let asm_output_file = temp_dir.path().join(format!("{test:}.asm"));
//         let machine_code_output_file = temp_dir.path().join(test);
//         let mut f = File::create(&asm_output_file).unwrap();
//         f.write_all(output.as_bytes()).unwrap();
//         f.sync_all().unwrap();

//         // run the nasm command
//         let _g = sh.push_dir(temp_dir.path());
//         cmd!(sh, "nasm {asm_output_file} -o {machine_code_output_file}")
//             .run()
//             .unwrap();

//         // read the generated binary
//         let content_binary = sh.read_binary_file(&machine_code_output_file).unwrap();
//         println!("{output:}");
//         assert_eq!(
//             buffer_to_nice_bytes(&expected_content),
//             buffer_to_nice_bytes(&content_binary)
//         );

//         // let re_derived_set_binary = InstructionSet::from_bytes(&content_binary);
//         // assert_eq!(ix_set, &re_derived_set_binary);
//         // if content_binary != expected_content {
//         //     assert_str_eq!(expected_asm_content, output);
//         // }

//         // println!()
//         // assert_eq!(content_binary, expected_content);
//     }

//     fn buffer_to_nice_bytes(buffer: &[u8]) -> String {
//         buffer
//             .iter()
//             .map(|byte| format!("0b{:08b}", byte))
//             .collect::<Vec<String>>()
//             .join(" ")
//     }

//     #[test]
//     fn test_listing_37() {
//         let test = "listing_0037_single_register_mov";
//         read_and_test(test);
//     }

//     #[test]
//     fn test_listing_38() {
//         let test = "listing_0038_many_register_mov";
//         read_and_test(test);
//     }

//     #[test]
//     fn test_listing_39() {
//         let test = "listing_0039_more_movs";
//         read_and_test(test);
//     }

//     #[test]
//     fn test_listing_40() {
//         let test = "listing_0040_challenge_movs";
//         read_and_test(test);
//     }

//     #[test]
//     #[ignore = "jumps and labels ar PITA to generate but it works (manually validated, trust me bro)"]
//     fn test_listing_41() {
//         let test = "listing_0041_add_sub_cmp_jnz";
//         read_and_test(test);
//     }

//     #[test]
//     fn test_get_expected_register() {
//         let base_data = 0b00000111_u8;
//         let register = decode_reg8(base_data);
//         assert_eq!(register, Reg8::BH);

//         let base_data = 0b00111000_u8;
//         let register = decode_reg8(base_data >> 3);
//         assert_eq!(register, Reg8::BH);
//     }
// }
