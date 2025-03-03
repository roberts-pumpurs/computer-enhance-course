use bitvec::prelude::*;

#[derive(Debug)]
pub struct Instruction {
    size: u8,
    operation: &'static str,
    flags: u8,
    operands: [Operand; 2],
}

#[repr(u8)]
#[derive(Debug)]
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

/// An operand can be one of several kinds.
#[derive(Debug)]
enum Operand {
    None,
    Address(EffectiveAddress),
    Register(RegisterIndex),
    Immediate(Immediate),
}

/// Immediate values may be 8 or 16 bits.
#[derive(Debug)]
pub enum Immediate {
    Byte(u8),
    Word(u16),
}

/// For memory operands, we support both direct addressing and register indirect addressing with optional displacement.
#[derive(Debug)]
pub enum EffectiveAddress {
    /// When mod == 0 and rm == 6, the operand is a 16-bit direct address.
    Direct(u16),
    /// For other cases, we record the base registers plus an optional displacement.
    Indirect {
        base: EffectiveAddressBase,
        disp: Option<i16>,
    },
}

/// The possible base register combinations used in 8086 effective address computation.
#[derive(Debug)]
pub enum EffectiveAddressBase {
    BX_SI,
    BX_DI,
    BP_SI,
    BP_DI,
    SI,
    DI,
    BP,
    BX,
}

// Map (reg_bits,w) -> a unique register index, in a 0..7 space for 8-bit or 16-bit.
fn decode_8086_register_index(reg_bits: u8, w: bool) -> RegisterIndex {
    match (w, reg_bits) {
        (false, 0b000) => RegisterIndex::AL,
        (false, 0b001) => RegisterIndex::CL,
        (false, 0b010) => RegisterIndex::DL,
        (false, 0b011) => RegisterIndex::BL,
        (false, 0b100) => RegisterIndex::AH,
        (false, 0b101) => RegisterIndex::CH,
        (false, 0b110) => RegisterIndex::DH,
        (false, 0b111) => RegisterIndex::BH,
        (true, 0b000) => RegisterIndex::AX,
        (true, 0b001) => RegisterIndex::CX,
        (true, 0b010) => RegisterIndex::DX,
        (true, 0b011) => RegisterIndex::BX,
        (true, 0b100) => RegisterIndex::SP,
        (true, 0b101) => RegisterIndex::BP,
        (true, 0b110) => RegisterIndex::SI,
        (true, 0b111) => RegisterIndex::DI,
        _ => panic!("unknown register"),
    }
}

/// Decode the rm operand based on mod and rm values. When mod == 3 the operand is a register;
/// otherwise it is a memory operand. This function also reads any displacement bytes from `bytes`.
fn decode_rm_operand(mod_val: u8, rm_val: u8, w: bool, bytes: &[u8]) -> Option<(Operand, usize)> {
    match mod_val {
        0b11 => {
            let reg = decode_8086_register_index(rm_val, w);
            Some((Operand::Register(reg), 0))
        }
        0b00 => {
            if rm_val == 0b110 {
                // Direct addressing: need a 16-bit address.
                if bytes.len() < 2 {
                    panic!("not enough data bytes for direct addressing");
                }
                let addr = u16::from_le_bytes([bytes[0], bytes[1]]);
                Some((Operand::Address(EffectiveAddress::Direct(addr)), 2))
            } else {
                let base = match rm_val {
                    0b000 => EffectiveAddressBase::BX_SI,
                    0b001 => EffectiveAddressBase::BX_DI,
                    0b010 => EffectiveAddressBase::BP_SI,
                    0b011 => EffectiveAddressBase::BP_DI,
                    0b100 => EffectiveAddressBase::SI,
                    0b101 => EffectiveAddressBase::DI,
                    0b111 => EffectiveAddressBase::BX,
                    _ => panic!("invalid rm value"),
                };
                Some((
                    Operand::Address(EffectiveAddress::Indirect { base, disp: None }),
                    0,
                ))
            }
        }
        0b01 => {
            // 8-bit displacement follows.
            if bytes.len() < 1 {
                return None;
            }
            let disp = bytes[0] as i8 as i16;
            let base = match rm_val {
                0 => EffectiveAddressBase::BX_SI,
                1 => EffectiveAddressBase::BX_DI,
                2 => EffectiveAddressBase::BP_SI,
                3 => EffectiveAddressBase::BP_DI,
                4 => EffectiveAddressBase::SI,
                5 => EffectiveAddressBase::DI,
                6 => EffectiveAddressBase::BP,
                7 => EffectiveAddressBase::BX,
                _ => panic!("invalid rm value"),
            };
            Some((
                Operand::Address(EffectiveAddress::Indirect {
                    base,
                    disp: Some(disp),
                }),
                1,
            ))
        }
        0b10 => {
            // 16-bit displacement follows.
            if bytes.len() < 2 {
                return None;
            }
            let disp = i16::from_le_bytes([bytes[0], bytes[1]]);
            let base = match rm_val {
                0 => EffectiveAddressBase::BX_SI,
                1 => EffectiveAddressBase::BX_DI,
                2 => EffectiveAddressBase::BP_SI,
                3 => EffectiveAddressBase::BP_DI,
                4 => EffectiveAddressBase::SI,
                5 => EffectiveAddressBase::DI,
                6 => EffectiveAddressBase::BP,
                7 => EffectiveAddressBase::BX,
                _ => return None,
            };
            Some((
                Operand::Address(EffectiveAddress::Indirect {
                    base,
                    disp: Some(disp),
                }),
                2,
            ))
        }
        _ => panic!("invalid mod value"),
    }
}

/// A parsable item in the instruction format.
enum P<'a> {
    /// Binary constant pattern.
    C(&'a BitSlice<u8, Msb0>),
    /// The data (direction) flag.
    D,
    /// The wide flag.
    W,
    /// mod bits.
    MOD,
    /// reg bits.
    REG,
    /// rm bits.
    RM,
    /// Expect a data byte.
    DATA,
    /// Expect a data byte if W == 1.
    DataIfW,
    /// Implied value of the D flag.
    ImplD(bool),
}

impl<'a> P<'a> {
    fn bit_len(&self) -> u8 {
        match self {
            P::C(value) => value.len() as u8,
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

/// Instruction definition.
pub struct IxDef<'a> {
    name: &'static str,
    items: Vec<P<'a>>,
}

impl<'a> IxDef<'a> {
    pub const fn new(name: &'static str, items: Vec<P<'a>>) -> Self {
        Self { name, items }
    }

    /// Returns the decoded instruction and the subslice of input bytes where to continue parsing.
    pub fn from_bytes<'b, 'c>(&'c self, bytes: &'b [u8]) -> Option<(Instruction, &'b [u8])> {
        let bits = bytes.view_bits::<Msb0>();
        let mut bit_offset = 0usize;

        // Local fields.
        let mut d_val: Option<bool> = None;
        let mut w_val: Option<bool> = None;
        let mut mod_val: Option<u8> = None;
        let mut reg_val: Option<u8> = None;
        let mut rm_val: Option<u8> = None;
        let mut data_lo: Option<u8> = None;
        let mut data_hi: Option<u8> = None;

        // Helper: read the next `length` bits.
        let mut read_bits = |length: usize| -> Option<&BitSlice<u8, Msb0>> {
            if bit_offset + length > bits.len() {
                tracing::error!("invalid bit offset");
                return None;
            }
            let slice = &bits[bit_offset..bit_offset + length];
            bit_offset += length;
            Some(slice)
        };

        for item in &self.items {
            match item {
                // Compare the next bits to the expected pattern.
                P::C(pattern) => {
                    let len = pattern.len();
                    let candidate = read_bits(len)?;
                    if !candidate.eq(pattern) {
                        return None;
                    }
                }
                P::D => {
                    let slice = read_bits(1)?;
                    d_val = Some(slice[0]);
                }
                P::W => {
                    let slice = read_bits(1)?;
                    w_val = Some(slice[0]);
                }
                P::MOD => {
                    let slice = read_bits(2)?;
                    let mut val = 0u8;
                    for bit in slice {
                        val = (val << 1) | (*bit as u8);
                    }
                    mod_val = Some(val);
                }
                P::REG => {
                    let slice = read_bits(3)?;
                    let mut val = 0u8;
                    for bit in slice {
                        val = (val << 1) | (*bit as u8);
                    }
                    reg_val = Some(val);
                }
                P::RM => {
                    let slice = read_bits(3)?;
                    let mut val = 0u8;
                    for bit in slice {
                        val = (val << 1) | (*bit as u8);
                    }
                    rm_val = Some(val);
                }
                P::DATA => {
                    let slice = read_bits(8)?;
                    let mut val = 0u8;
                    for bit in slice {
                        val = (val << 1) | (*bit as u8);
                    }
                    data_lo = Some(val);
                }
                P::DataIfW => {
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

        let mut bytes_consumed = (bit_offset + 7) / 8; // round up

        // Depending on the instruction variant, decode operands.
        match self.name {
            "mov" => {
                let d_bit = if let Some(d) = d_val { d as u8 } else { 0 };
                let w_bit = if let Some(w) = w_val { w as u8 } else { 0 };
                let flags = (d_bit << 1) | w_bit;

                // Two variants: one that has a REG field (mov r/m, r) and one without (mov r/m, imm).
                if reg_val.is_some() {
                    // Variant: mov r/m, r
                    let rm = rm_val?;
                    let m = mod_val?;
                    let w = w_val.unwrap();
                    // Decode the r/m operand (which might consume extra bytes if there’s a displacement)
                    let (rm_operand, extra) =
                        decode_rm_operand(m, rm, w, &bytes[bytes_consumed..])?;
                    bytes_consumed += extra;
                    let reg_operand =
                        Operand::Register(decode_8086_register_index(reg_val.unwrap(), w));
                    // d flag selects which operand is the destination register.
                    let (first_operand, second_operand) = if d_val.unwrap() {
                        (reg_operand, rm_operand)
                    } else {
                        (rm_operand, reg_operand)
                    };
                    let inst = Instruction {
                        size: bytes_consumed as u8,
                        operation: self.name,
                        flags,
                        operands: [first_operand, second_operand],
                    };
                    Some((inst, &bytes[bytes_consumed..]))
                } else {
                    // Variant: mov r/m, imm
                    let rm = rm_val?;
                    let m = mod_val?;
                    let w = w_val.unwrap();
                    let (rm_operand, extra) =
                        decode_rm_operand(m, rm, w, &bytes[bytes_consumed..])?;
                    bytes_consumed += extra;
                    // Immediate value is already read from the pattern.
                    let imm_operand = if w {
                        // For a word immediate, we expect two bytes.
                        let lo = data_lo?;
                        let hi = data_hi?;
                        let word = u16::from_le_bytes([lo, hi]);
                        Operand::Immediate(Immediate::Word(word))
                    } else {
                        let byte = data_lo?;
                        Operand::Immediate(Immediate::Byte(byte))
                    };
                    // In the mov r/m, imm encoding, r/m is the destination.
                    let inst = Instruction {
                        size: bytes_consumed as u8,
                        operation: self.name,
                        flags,
                        operands: [rm_operand, imm_operand],
                    };
                    Some((inst, &bytes[bytes_consumed..]))
                }
            }
            _ => unimplemented!(),
        }
    }
}

// Example instruction table.
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

#[cfg(test)]
mod tests {
    use core::panic;
    use std::{fs::File, io::Write as _};

    use itertools::Itertools as _;
    use pretty_assertions::{assert_eq, assert_str_eq};
    use tempdir::TempDir;
    use test_log::test;
    use xshell::cmd;

    use super::*;

    fn read_and_test(test: &str) {
        let content_binary = std::fs::read(format!("fixtures/{test:}")).unwrap();
        let (derived_set_binary, _) = ix_table()[0].from_bytes(&content_binary).unwrap();

        // generate_and_compare_machine_code(test, &derived_set_binary);
        dbg!(&derived_set_binary);
        panic!()
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
}
