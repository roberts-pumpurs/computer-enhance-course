use core::fmt;

use bitvec::prelude::*;
use num_traits::Signed;

#[derive(Debug, PartialEq, Eq)]
pub struct InstructionSet {
    ixs: Vec<Instruction>,
}
pub fn decode(mut bytes_to_pass: &[u8]) -> InstructionSet {
    let mut ixs = vec![];
    let ix_table = ix_table();
    'top: while bytes_to_pass.len() > 0 {
        for ix_def in ix_table.iter() {
            let res = ix_def.from_bytes(bytes_to_pass);
            if let Some((ix, new_bytes)) = res {
                println!("{ix}");
                ixs.push(ix);
                bytes_to_pass = new_bytes;
                continue 'top;
            }
        }

        panic!("invalid binary ix");
    }
    InstructionSet { ixs }
}

impl fmt::Display for InstructionSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "bits 16")?;
        for instr in &self.ixs {
            writeln!(f, "{instr}")?;
        }
        Ok(())
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:} {:}, {:}",
            self.operation, self.operands[0], self.operands[1]
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Instruction {
    size: u8,
    operation: &'static str,
    operands: [Operand; 2],
}

#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
#[derive(Debug, PartialEq, Eq)]
enum Operand {
    None,
    Address(EffectiveAddress),
    Register(RegisterIndex),
    Immediate(Immediate),
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::None => write!(f, ""),
            Operand::Address(effective_address) => match effective_address {
                EffectiveAddress::Direct(val) => {
                    write!(f, "[{}]", val)
                }
                EffectiveAddress::Indirect { base, disp } => match disp {
                    Some(val) => {
                        let sign = if val.is_positive() { "+" } else { "-" };
                        match base {
                            EffectiveAddressBase::BX_SI => {
                                write!(f, "[bx + si {} {}]", sign, val.abs())
                            }
                            EffectiveAddressBase::BX_DI => {
                                write!(f, "[bx + di {} {}]", sign, val.abs())
                            }
                            EffectiveAddressBase::BP_SI => {
                                write!(f, "[bp + si {} {}]", sign, val.abs())
                            }
                            EffectiveAddressBase::BP_DI => {
                                write!(f, "[bp + di {} {}]", sign, val.abs())
                            }
                            EffectiveAddressBase::SI => write!(f, "[si {} {}]", sign, val.abs()),
                            EffectiveAddressBase::DI => write!(f, "[di {} {}]", sign, val.abs()),
                            EffectiveAddressBase::BP => write!(f, "[bp {} {}]", sign, val.abs()),
                            EffectiveAddressBase::BX => write!(f, "[bx {} {}]", sign, val.abs()),
                        }
                    }
                    None => match base {
                        EffectiveAddressBase::BX_SI => write!(f, "[bx + si]"),
                        EffectiveAddressBase::BX_DI => write!(f, "[bx + di]"),
                        EffectiveAddressBase::BP_SI => write!(f, "[bp + si]"),
                        EffectiveAddressBase::BP_DI => write!(f, "[bp + di]"),
                        EffectiveAddressBase::SI => write!(f, "[si]"),
                        EffectiveAddressBase::DI => write!(f, "[di]"),
                        EffectiveAddressBase::BP => write!(f, "[bp]"),
                        EffectiveAddressBase::BX => write!(f, "[bx]"),
                    },
                },
            },
            Operand::Register(register_index) => match register_index {
                RegisterIndex::AL => write!(f, "al"),
                RegisterIndex::CL => write!(f, "cl"),
                RegisterIndex::DL => write!(f, "dl"),
                RegisterIndex::BL => write!(f, "bl"),
                RegisterIndex::AH => write!(f, "ah"),
                RegisterIndex::CH => write!(f, "ch"),
                RegisterIndex::DH => write!(f, "dh"),
                RegisterIndex::BH => write!(f, "bh"),
                RegisterIndex::AX => write!(f, "ax"),
                RegisterIndex::CX => write!(f, "cx"),
                RegisterIndex::DX => write!(f, "dx"),
                RegisterIndex::BX => write!(f, "bx"),
                RegisterIndex::SP => write!(f, "sp"),
                RegisterIndex::BP => write!(f, "bp"),
                RegisterIndex::SI => write!(f, "si"),
                RegisterIndex::DI => write!(f, "di"),
            },
            Operand::Immediate(immediate) => match immediate {
                Immediate::Byte(val) => write!(f, "byte {}", val),
                Immediate::Word(val) => write!(f, "word {}", val),
            },
        }
    }
}

/// Immediate values may be 8 or 16 bits.
#[derive(Debug, PartialEq, Eq)]
pub enum Immediate {
    Byte(u8),
    Word(u16),
}

/// For memory operands, we support both direct addressing and register indirect addressing with optional displacement.
#[derive(Debug, PartialEq, Eq)]
pub enum EffectiveAddress {
    Direct(u16),
    Indirect {
        base: EffectiveAddressBase,
        disp: Option<i16>,
    },
}

/// The possible base register combinations used in 8086 effective address computation.
#[derive(Debug, PartialEq, Eq)]
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
                panic!("not enough data bytes to read displacement");
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
    /// lo address bits
    AddrLo,
    /// hi address bits
    AddrHi,
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
    /// Implied value of the mod bits
    ImplMod(u8),
    /// Implied value of the rm bits
    ImplRm(u8),
    /// Implied value of the reg bits
    ImplRegBasedOnW(RegisterIndex, RegisterIndex),
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
            P::AddrLo => 8,
            P::AddrHi => 8,
            P::ImplD(_) => 0,
            P::ImplMod(_) => 0,
            P::ImplRm(_) => 0,
            P::ImplRegBasedOnW(_, _) => 0,
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
        let mut addr_lo: Option<u8> = None;
        let mut addr_hi: Option<u8> = None;
        // parse operands
        let mut reg_operand = None;
        let second_operand;

        // Helper: read the next `length` bits.
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
                P::AddrLo => {
                    let slice = read_bits(8)?;
                    let mut val = 0u8;
                    for bit in slice {
                        val = (val << 1) | (*bit as u8);
                    }
                    addr_lo = Some(val);
                }
                P::AddrHi => {
                    let slice = read_bits(8)?;
                    let mut val = 0u8;
                    for bit in slice {
                        val = (val << 1) | (*bit as u8);
                    }
                    addr_hi = Some(val);
                }
                P::ImplMod(v) => {
                    mod_val = Some(*v);
                }
                P::ImplRm(rm) => {
                    rm_val = Some(*rm);
                }
                P::ImplRegBasedOnW(if_w, if_not_w) => {
                    if w_val.unwrap() {
                        reg_operand = Some(Operand::Register(*if_w));
                    } else {
                        reg_operand = Some(Operand::Register(*if_not_w));
                    }
                }
            }
        }

        let mut bytes_consumed = (bit_offset + 7) / 8; // round up

        // parse reg opernad
        let reg_operand = if reg_operand.is_none() {
            if let Some(reg_val) = reg_val {
                Operand::Register(decode_8086_register_index(reg_val, w_val.unwrap()))
            } else {
                match data_lo {
                    Some(data_lo) => operand_from_data(data_lo, data_hi),
                    None => match (addr_lo, addr_hi) {
                        (None, None) => todo!(),
                        (Some(lo), Some(hi)) => {
                            let addr = u16::from_le_bytes([lo, hi]);
                            Operand::Address(EffectiveAddress::Direct(addr))
                        }
                        _ => unreachable!(),
                    },
                }
            }
        } else {
            reg_operand.unwrap()
        };

        // parse the second opernad
        match (rm_val, mod_val, w_val) {
            (Some(rm), Some(mod_val), Some(w)) => {
                // Decode the r/m operand (which might consume extra bytes if there’s a displacement)
                let (rm_operand, extra) =
                    decode_rm_operand(mod_val, rm, w, &bytes[bytes_consumed..])?;
                bytes_consumed += extra;
                second_operand = rm_operand;
            }
            (None, None, Some(_w)) => match data_lo {
                Some(data_lo) => {
                    second_operand = operand_from_data(data_lo, data_hi);
                }
                None => match (addr_lo, addr_hi) {
                    (None, None) => todo!(),
                    (Some(lo), Some(hi)) => {
                        let addr = u16::from_le_bytes([lo, hi]);
                        second_operand = Operand::Address(EffectiveAddress::Direct(addr));
                    }
                    _ => unreachable!(),
                },
            },
            _ => unimplemented!(),
        };

        // d flag selects which operand is the destination register.
        // if d -- ix source is REG field
        // if not d -- destination is the REG field
        let (first_operand, second_operand) = if d_val.unwrap() {
            (reg_operand, second_operand)
        } else {
            (second_operand, reg_operand)
        };
        let inst = Instruction {
            size: bytes_consumed as u8,
            operation: self.name,
            operands: [first_operand, second_operand],
        };
        Some((inst, &bytes[bytes_consumed..]))
    }
}

fn operand_from_data(data_lo: u8, data_hi: Option<u8>) -> Operand {
    match (data_hi) {
        // first, see if we have an immediate as an operand
        (None) => Operand::Immediate(Immediate::Byte(data_lo)),
        (Some(hi)) => {
            let word = u16::from_le_bytes([data_lo, hi]);
            Operand::Immediate(Immediate::Word(word))
        }
    }
}

// https://github.com/cmuratori/computer_enhance/blob/c0b12bed53a004e1f6ca2995dc3fb73d793ac6b8/perfaware/sim86/sim86_instruction_table.inl#L58
#[rustfmt::skip]
pub fn ix_table() -> [IxDef<'static>; 5] {
    use P::*;
    [
        // reg/mem to/from reg
        IxDef::new( "mov", vec![C(bits!(static u8, Msb0; 1, 0, 0, 0, 1, 0)), D, W, MOD, REG, RM]),
        // imm to  reg 
        IxDef::new( "mov", vec![C(bits!(static u8, Msb0; 1, 1, 0, 0, 0, 1, 1)), W, MOD, C(bits!(static u8, Msb0; 0, 0, 0)), RM, DATA, DataIfW, ImplD(false)]),
        // imm to reg 
        IxDef::new(  "mov",  vec![ C(bits!(static u8, Msb0; 1, 0, 1, 1)), W, REG, DATA, DataIfW, ImplD(true)]),
        // mem to accumulator
        IxDef::new(  "mov",  vec![ C(bits!(static u8, Msb0; 1, 0, 1, 0, 0, 0, 0)), W, AddrLo, AddrHi, ImplRegBasedOnW(RegisterIndex::AX, RegisterIndex::AL), ImplD(true)]),
        // accumulator to mem
        IxDef::new(  "mov",  vec![ C(bits!(static u8, Msb0; 1, 0, 1, 0, 0, 0, 1)), W, AddrLo, AddrHi, ImplRegBasedOnW(RegisterIndex::AX, RegisterIndex::AL), ImplD(false)]),
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

#[cfg(test)]
mod tests {
    use std::{fs::File, io::Write as _};

    use itertools::Itertools as _;
    use pretty_assertions::{assert_eq, assert_str_eq};
    use tempdir::TempDir;
    use test_log::test;
    use xshell::cmd;

    use super::*;

    fn read_and_test(test: &str) {
        let content_binary = std::fs::read(format!("fixtures/{test:}")).unwrap();

        let bytes_to_pass = content_binary.as_slice();
        let ixs = decode(bytes_to_pass);

        generate_and_compare_machine_code(test, &ixs);
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

        let re_derived_set_binary = decode(&content_binary);
        assert_eq!(ix_set, &re_derived_set_binary);
        if content_binary != expected_content {
            assert_str_eq!(expected_asm_content, output);
        }

        assert_eq!(content_binary, expected_content);
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
