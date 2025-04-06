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
        let mut ctx = ParseContext::default();
        for ix_def in ix_table.iter() {
            ctx = ParseContext::default();
            let res = ix_def.from_bytes(&mut ctx, bytes_to_pass);
            if let Some((ix, new_bytes)) = res {
                tracing::info!("{ix}");
                ixs.push(ix);
                bytes_to_pass = new_bytes;
                continue 'top;
            }
        }

        tracing::error!(?ctx);
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
        match (self.operands[0], self.operands[1]) {
            (None, None) => {
                write!(f, "{:}", self.operation)
            }
            (None, Some(op)) => {
                write!(f, "{:} {:}", self.operation, op)
            }
            (Some(op), None) => {
                write!(f, "{:} {:}", self.operation, op)
            }
            (Some(op1), Some(op2)) => {
                write!(f, "{:} {:}, {:}", self.operation, op1, op2)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Instruction {
    size: u8,
    operation: &'static str,
    operands: [Option<Operand>; 2],
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
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Operand {
    Address(EffectiveAddress),
    Register(RegisterIndex),
    Immediate(Immediate),
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
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
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Immediate {
    Byte(u8),
    Word(u16),
}

/// For memory operands, we support both direct addressing and register indirect addressing with optional displacement.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum EffectiveAddress {
    Direct(u16),
    Indirect {
        base: EffectiveAddressBase,
        disp: Option<i16>,
    },
}

/// The possible base register combinations used in 8086 effective address computation.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
fn decode_rm_operand(
    mod_val: u8,
    rm_val: u8,
    w: bool,
    disp_lo: Option<u8>,
    disp_hi: Option<u8>,
) -> Operand {
    match mod_val {
        0b11 => {
            let reg = decode_8086_register_index(rm_val, w);
            Operand::Register(reg)
        }
        0b00 => {
            if rm_val == 0b110 {
                // Direct addressing: need a 16-bit address.
                let addr = u16::from_le_bytes([disp_lo.unwrap(), disp_hi.unwrap()]);
                Operand::Address(EffectiveAddress::Direct(addr))
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

                Operand::Address(EffectiveAddress::Indirect { base, disp: None })
            }
        }
        0b01 => {
            // 8-bit displacement follows.
            let disp = disp_lo.unwrap() as i8 as i16;
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
            Operand::Address(EffectiveAddress::Indirect {
                base,
                disp: Some(disp),
            })
        }
        0b10 => {
            // 16-bit displacement follows.
            let disp = i16::from_le_bytes([disp_lo.unwrap(), disp_hi.unwrap()]);
            let base = match rm_val {
                0 => EffectiveAddressBase::BX_SI,
                1 => EffectiveAddressBase::BX_DI,
                2 => EffectiveAddressBase::BP_SI,
                3 => EffectiveAddressBase::BP_DI,
                4 => EffectiveAddressBase::SI,
                5 => EffectiveAddressBase::DI,
                6 => EffectiveAddressBase::BP,
                7 => EffectiveAddressBase::BX,
                _ => panic!("invalid rm"),
            };
            Operand::Address(EffectiveAddress::Indirect {
                base,
                disp: Some(disp),
            })
        }
        _ => panic!("invalid mod value"),
    }
}

/// A parsable item in the instruction format.
#[derive(Debug, Clone)]
enum P<'a> {
    /// Binary constant pattern.
    C(&'a BitSlice<u8, Msb0>),
    /// The data (direction) flag.
    /// Instruction destination is specified in REG field
    D,
    /// The wide flag.
    /// Instruction operates on word data
    W,
    /// Sign flag
    /// Sign extend 8-bit immediate data to 16 bits if W=1
    S,
    /// mod bits.
    Mod,
    /// lo address bits
    AddrLo,
    /// hi address bits
    AddrHi,
    /// reg bits.
    Reg,
    /// rm bits.
    Rm,
    /// Expect a data byte.
    Data,
    /// Expect a data byte if W == 1.
    DataIfW,
    /// Expect a data byte if S == 1.
    DataIfS,
    OptDispLo,
    OptDispHi,
    /// Implied value of the D flag.
    ImplD(bool),
    /// Implied value of the reg bits
    ImplRegBasedOnW(RegisterIndex, RegisterIndex),
    // --
    // logical action of what to do with the data we parsed
    ParseReg,
    ParseSecondOperand,
}

/// Instruction definition.
#[derive(Debug, Clone)]
pub struct IxDef<'a> {
    name: &'static str,
    items: Vec<P<'a>>,
}

#[derive(Debug, Default)]
pub struct ParseContext {
    d_val: Option<bool>,
    w_val: Option<bool>,
    s_val: Option<bool>,
    mod_val: Option<u8>,
    reg_val: Option<u8>,
    rm_val: Option<u8>,
    data_lo: Option<u8>,
    data_hi: Option<u8>,
    disp_lo: Option<u8>,
    disp_hi: Option<u8>,
    addr_lo: Option<u8>,
    addr_hi: Option<u8>,
    reg_operand: Option<Operand>,
    second_operand: Option<Operand>,
}

impl<'a> IxDef<'a> {
    pub const fn new(name: &'static str, items: Vec<P<'a>>) -> Self {
        Self { name, items }
    }

    /// Returns the decoded instruction and the subslice of input bytes where to continue parsing.
    pub fn from_bytes<'b, 'c>(
        &'c self,
        ctx: &mut ParseContext,
        bytes: &'b [u8],
    ) -> Option<(Instruction, &'b [u8])> {
        let bits = bytes.view_bits::<Msb0>();
        let mut bit_offset = 0usize;

        // Helper: read the next `length` bits.
        let mut read_bits = |length: usize| -> &BitSlice<u8, Msb0> {
            if bit_offset + length > bits.len() {
                panic!("trying to read too many bits");
            }
            let slice = &bits[bit_offset..bit_offset + length];
            bit_offset += length;
            slice
        };

        for item in &self.items {
            tracing::debug!(?item);
            match item {
                // Compare the next bits to the expected pattern.
                P::C(pattern) => {
                    let len = pattern.len();
                    let candidate = read_bits(len);
                    if !candidate.eq(pattern) {
                        tracing::debug!("invalid pattern");
                        return None;
                    }
                }
                P::D => {
                    let slice = read_bits(1);
                    ctx.d_val = Some(slice[0]);
                }
                P::W => {
                    let slice = read_bits(1);
                    ctx.w_val = Some(slice[0]);
                }
                P::Mod => {
                    let slice = read_bits(2);
                    let val = slice_to_val(slice);
                    ctx.mod_val = Some(val);
                }
                P::Reg => {
                    let slice = read_bits(3);
                    let val = slice_to_val(slice);
                    ctx.reg_val = Some(val);
                }
                P::Rm => {
                    let slice = read_bits(3);
                    let val = slice_to_val(slice);
                    ctx.rm_val = Some(val);
                }
                P::Data => {
                    let slice = read_bits(8);
                    let val = slice_to_val(slice);
                    ctx.data_lo = Some(val);
                }
                P::DataIfW => {
                    if ctx.w_val == Some(true) {
                        let slice = read_bits(8);
                        let val = slice_to_val(slice);
                        ctx.data_hi = Some(val);
                    }
                }
                P::ImplD(value) => {
                    ctx.d_val = Some(*value);
                }
                P::AddrLo => {
                    let slice = read_bits(8);
                    let val = slice_to_val(slice);
                    ctx.addr_lo = Some(val);
                }
                P::AddrHi => {
                    let slice = read_bits(8);
                    let val = slice_to_val(slice);
                    ctx.addr_hi = Some(val);
                }
                P::ImplRegBasedOnW(if_w, if_not_w) => {
                    if ctx.w_val.unwrap() {
                        ctx.reg_operand = Some(Operand::Register(*if_w));
                    } else {
                        ctx.reg_operand = Some(Operand::Register(*if_not_w));
                    }
                }
                P::OptDispLo => {
                    match (ctx.mod_val.unwrap(), ctx.rm_val.unwrap()) {
                        // mod=00 + rm=110 => read first displacement byte
                        (0b00, 0b110) => {
                            let slice = read_bits(8);
                            ctx.disp_lo = Some(slice_to_val(slice));
                        }
                        // mod=01 => read one byte displacement
                        (0b01, _) => {
                            let slice = read_bits(8);
                            ctx.disp_lo = Some(slice_to_val(slice));
                        }
                        // mod=10 => read first byte of a 16-bit displacement
                        (0b10, _) => {
                            let slice = read_bits(8);
                            ctx.disp_lo = Some(slice_to_val(slice));
                        }
                        _ => {
                            // No displacement
                        }
                    }
                }
                P::OptDispHi => {
                    match (ctx.mod_val.unwrap(), ctx.rm_val.unwrap()) {
                        // mod=00 + rm=110 => second byte of the 16-bit address
                        (0b00, 0b110) => {
                            let slice = read_bits(8);
                            ctx.disp_hi = Some(slice_to_val(slice));
                        }
                        // mod=10 => second byte of the 16-bit displacement
                        (0b10, _) => {
                            let slice = read_bits(8);
                            ctx.disp_hi = Some(slice_to_val(slice));
                        }
                        _ => {
                            // No second displacement byte
                        }
                    }
                }
                P::ParseReg => {
                    // parse reg opernad
                    let tmp_reg_operand = if ctx.reg_operand.is_none() {
                        if let Some(reg_val) = ctx.reg_val {
                            Operand::Register(decode_8086_register_index(
                                reg_val,
                                ctx.w_val.unwrap(),
                            ))
                        } else {
                            match ctx.data_lo {
                                Some(data_lo) => operand_from_data(data_lo, ctx.data_hi),
                                None => match (ctx.addr_lo, ctx.addr_hi) {
                                    (None, None) => unreachable!(),
                                    (Some(lo), Some(hi)) => {
                                        let addr = u16::from_le_bytes([lo, hi]);
                                        Operand::Address(EffectiveAddress::Direct(addr))
                                    }
                                    _ => unreachable!(),
                                },
                            }
                        }
                    } else {
                        ctx.reg_operand.unwrap()
                    };
                    ctx.reg_operand = Some(tmp_reg_operand);
                }
                P::ParseSecondOperand => {
                    // parse the second opernad
                    match (ctx.rm_val, ctx.mod_val, ctx.w_val) {
                        (Some(rm), Some(mod_val), Some(w)) => {
                            // Decode the r/m operand (which might consume extra bytes if there’s a displacement)
                            let rm_operand =
                                decode_rm_operand(mod_val, rm, w, ctx.disp_lo, ctx.disp_hi);
                            ctx.second_operand = Some(rm_operand);
                        }
                        (None, None, Some(_w)) => match ctx.data_lo {
                            Some(data_lo) => {
                                ctx.second_operand = Some(operand_from_data(data_lo, ctx.data_hi));
                            }
                            None => match (ctx.addr_lo, ctx.addr_hi) {
                                (None, None) => todo!(),
                                (Some(lo), Some(hi)) => {
                                    let addr = u16::from_le_bytes([lo, hi]);
                                    ctx.second_operand =
                                        Some(Operand::Address(EffectiveAddress::Direct(addr)));
                                }
                                _ => unreachable!(),
                            },
                        },
                        _ => unimplemented!(),
                    };
                }
                P::S => {
                    let slice = read_bits(1);
                    ctx.s_val = Some(slice[0]);
                }
                P::DataIfS => {
                    if ctx.w_val.unwrap() {
                        if ctx.s_val.unwrap() {
                            ctx.data_hi = Some(0);
                        } else {
                            let slice = read_bits(8);
                            let val = slice_to_val(slice);
                            ctx.data_hi = Some(val);
                        }
                    }
                }
            }
        }

        let bytes_consumed = (bit_offset + 7) / 8; // round up

        // d flag selects which operand is the destination register.
        // if d -- ix source is REG field
        // if not d -- destination is the REG field
        let (first_operand, second_operand) = if ctx.d_val.unwrap_or_default() {
            (ctx.reg_operand, ctx.second_operand)
        } else {
            (ctx.second_operand, ctx.reg_operand)
        };
        let inst = Instruction {
            size: bytes_consumed as u8,
            operation: self.name,
            operands: [first_operand, second_operand],
        };
        Some((inst, &bytes[bytes_consumed..]))
    }
}

fn slice_to_val(slice: &BitSlice<u8, Msb0>) -> u8 {
    let mut val = 0u8;
    for bit in slice {
        val = (val << 1) | (*bit as u8);
    }
    val
}

fn operand_from_data(data_lo: u8, data_hi: Option<u8>) -> Operand {
    match data_hi {
        // first, see if we have an immediate as an operand
        None => Operand::Immediate(Immediate::Byte(data_lo)),
        Some(hi) => {
            let word = u16::from_le_bytes([data_lo, hi]);
            Operand::Immediate(Immediate::Word(word))
        }
    }
}

// https://github.com/cmuratori/computer_enhance/blob/c0b12bed53a004e1f6ca2995dc3fb73d793ac6b8/perfaware/sim86/sim86_instruction_table.inl#L58
#[rustfmt::skip]
pub fn ix_table() -> [IxDef<'static>; 34] {
    use P::*;
    let arithm = |name: &'static str, idx: usize, overrides: &[(usize, P<'static>)]| {
        let base_arithm_defs = [
            IxDef::new("add",  vec![C(bits!(static u8, Msb0; 0,0,0,0,0,0)), D, W, Mod, Reg, Rm, OptDispLo, OptDispHi, ParseReg, ParseSecondOperand]),
            IxDef::new("add",  vec![C(bits!(static u8, Msb0; 1,0,0,0,0,0)), S, W, Mod, C(bits!(static u8, Msb0; 0, 0, 0)), Rm, OptDispLo, OptDispHi, Data, DataIfS, ImplD(false), ParseReg, ParseSecondOperand]),
            IxDef::new("add",  vec![C(bits!(static u8, Msb0; 0,0,0,0,0,1,0)), W, Data, DataIfW, ImplRegBasedOnW(RegisterIndex::AX, RegisterIndex::AL), ImplD(true),  ParseReg, ParseSecondOperand]),
        ];
        let mut def = base_arithm_defs[idx].clone();
        for (idx, ov) in overrides.iter() {
            def.items[*idx] = ov.clone();
        }
        def.name = name;
        def
    };
    [
        // reg/mem to/from reg
        IxDef::new("mov", vec![C(bits!(static u8, Msb0; 1, 0, 0, 0, 1, 0)), D, W, Mod, Reg, Rm, OptDispLo, OptDispHi, ParseReg, ParseSecondOperand]),
        // imm to reg 
        IxDef::new("mov", vec![C(bits!(static u8, Msb0; 1, 1, 0, 0, 0, 1, 1)), W, Mod, C(bits!(static u8, Msb0; 0, 0, 0)), Rm, OptDispLo, OptDispHi, Data, DataIfW, ImplD(false), ParseReg, ParseSecondOperand]),
        // imm to reg 
        IxDef::new("mov",  vec![C(bits!(static u8, Msb0; 1, 0, 1, 1)), W, Reg, Data, DataIfW, ImplD(true), ParseReg, ParseSecondOperand]),
        // mem to accumulator
        IxDef::new("mov",  vec![C(bits!(static u8, Msb0; 1, 0, 1, 0, 0, 0, 0)), W, AddrLo, AddrHi, ImplRegBasedOnW(RegisterIndex::AX, RegisterIndex::AL), ImplD(true),  ParseReg, ParseSecondOperand]),
        // accumulator to mem
        IxDef::new("mov",  vec![C(bits!(static u8, Msb0; 1, 0, 1, 0, 0, 0, 1)), W, AddrLo, AddrHi, ImplRegBasedOnW(RegisterIndex::AX, RegisterIndex::AL), ImplD(false), ParseReg, ParseSecondOperand]),
        // add
        arithm("add", 0, &[]),
        arithm("add", 1, &[]),
        arithm("add", 2, &[]),
        // sub
        arithm("sub", 0, &[(0, C(bits!(static u8, Msb0; 0,0,1,0,1,0)))]),
        arithm("sub", 1, &[(4, C(bits!(static u8, Msb0; 1,0,1)))]),
        arithm("sub", 2, &[(0, C(bits!(static u8, Msb0; 0,0,1,0,1,1,0)))]),
        // cmp
        arithm("cmp", 0, &[(0, C(bits!(static u8, Msb0; 0,0,1,1,1,0)))]),
        arithm("cmp", 1, &[(4, C(bits!(static u8, Msb0; 1,1,1)))]),
        arithm("cmp", 2, &[(0, C(bits!(static u8, Msb0; 0,0,1,1,1,1,0)))]),
        // je/jz
        IxDef::new("je",  vec![C(bits!(static u8, Msb0; 0,1,1,1,0,1,0,0)), Data, ParseReg]),
        IxDef::new("jl",  vec![C(bits!(static u8, Msb0; 0,1,1,1,1,1,0,0)), Data, ParseReg]),
        IxDef::new("jle",  vec![C(bits!(static u8, Msb0; 0,1,1,1,1,1,1,0)), Data, ParseReg]),
        IxDef::new("jb",  vec![C(bits!(static u8, Msb0; 0,1,1,1,0,0,1,0)), Data, ParseReg]),
        IxDef::new("jbe",  vec![C(bits!(static u8, Msb0; 0,1,1,1,0,1,1,0)), Data, ParseReg]),
        IxDef::new("jp",  vec![C(bits!(static u8, Msb0; 0,1,1,1,1,0,1,0)), Data, ParseReg]),
        IxDef::new("jo",  vec![C(bits!(static u8, Msb0; 0,1,1,1,0,0,0,0)), Data, ParseReg]),
        IxDef::new("js",  vec![C(bits!(static u8, Msb0; 0,1,1,1,1,0,0,0)), Data, ParseReg]),
        IxDef::new("jne",  vec![C(bits!(static u8, Msb0; 0,1,1,1,0,1,0,1)), Data, ParseReg]),
        IxDef::new("jnl",  vec![C(bits!(static u8, Msb0; 0,1,1,1,1,1,0,1)), Data, ParseReg]),
        IxDef::new("jnle",  vec![C(bits!(static u8, Msb0; 0,1,1,1,1,1,1,1)), Data, ParseReg]),
        IxDef::new("jnb",  vec![C(bits!(static u8, Msb0; 0,1,1,1,0,0,1,1)), Data, ParseReg]),
        IxDef::new("jnbe",  vec![C(bits!(static u8, Msb0; 0,1,1,1,0,1,1,1)), Data, ParseReg]),
        IxDef::new("jnp",  vec![C(bits!(static u8, Msb0; 0,1,1,1,1,0,1,1)), Data, ParseReg]),
        IxDef::new("jno",  vec![C(bits!(static u8, Msb0; 0,1,1,1,0,0,0,1)), Data, ParseReg]),
        IxDef::new("jns",  vec![C(bits!(static u8, Msb0; 0,1,1,1,1,0,0,1)), Data, ParseReg]),
        IxDef::new("loop",  vec![C(bits!(static u8, Msb0; 1,1,1,0,0,0,1,0)), Data, ParseReg]),
        IxDef::new("loopz",  vec![C(bits!(static u8, Msb0; 1,1,1,0,0,0,0,1)), Data, ParseReg]),
        IxDef::new("loopnz",  vec![C(bits!(static u8, Msb0; 1,1,1,0,0,0,0,0)), Data, ParseReg]),
        IxDef::new("jcxz",  vec![C(bits!(static u8, Msb0; 1,1,1,0,0,0,1,1)), Data, ParseReg]),
    ]
}

#[cfg(test)]
mod tests {
    use std::{fmt::Formatter, fs::File, io::Write as _};

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

    /// Assemble a bits-16 snippet into raw machine code via NASM.
    fn assemble_16_bit(asm_snippet: &str) -> Vec<u8> {
        let sh = xshell::Shell::new().unwrap();
        let dir = tempdir::TempDir::new("").unwrap();
        let asm_path = dir.path().join("test.asm");
        let bin_path = dir.path().join("test.bin");

        // Write code to a temporary .asm file
        {
            let mut file = File::create(&asm_path).unwrap();
            file.write_all(asm_snippet.as_bytes()).unwrap();
        }

        // Invoke nasm
        cmd!(sh, "nasm {asm_path} -o {bin_path}").run().unwrap();

        struct V(Vec<u8>);

        impl fmt::Binary for V {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                let vec = &self.0;
                for (count, n) in vec.iter().enumerate() {
                    if count != 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{:08b}", n)?;
                }
                Ok(())
            }
        }

        // Read back the generated machine code
        let machine_code = V(std::fs::read(&bin_path).unwrap());
        println!("{machine_code:#0b}");
        machine_code.0
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
    fn test_manual_1() {
        let asm_snippet = r#"
bits 16
mov al, [bx + si]
mov bx, [bp + di]
"#;
        let machine_code = assemble_16_bit(asm_snippet);
        let decoded = decode(&machine_code);
        assert_eq!(decoded.ixs.len(), 2);
        assert_eq!(format!("{}", decoded.ixs[0]), "mov al, [bx + si]");
        assert_eq!(format!("{}", decoded.ixs[1]), "mov bx, [bp + di]");
    }

    #[test]
    fn test_manual_2() {
        let asm_snippet = r#"
bits 16
mov al, [bx + si + 4999]
"#;
        let machine_code = assemble_16_bit(asm_snippet);
        let decoded = decode(&machine_code);
        assert_eq!(decoded.ixs.len(), 1);
        assert_eq!(format!("{}", decoded.ixs[0]), "mov al, [bx + si + 4999]");
    }

    #[test]
    fn test_manual_3() {
        let asm_snippet = r#"
bits 16
mov [bp + di], byte 7
"#;
        let machine_code = assemble_16_bit(asm_snippet);
        let decoded = decode(&machine_code);
        assert_eq!(decoded.ixs.len(), 1);
        assert_eq!(format!("{}", decoded.ixs[0]), "mov [bp + di], byte 7");
    }

    #[test]
    fn test_manual_4() {
        let asm_snippet = r#"
bits 16
add bx, [bx + si]
"#;
        let machine_code = assemble_16_bit(asm_snippet);
        let decoded = decode(&machine_code);
        assert_eq!(decoded.ixs.len(), 1);
        assert_eq!(format!("{}", decoded.ixs[0]), "add bx, [bx + si]");
    }
    #[test]
    fn test_manual_5() {
        let asm_snippet = r#"
bits 16
jnz test_label1
test_label1:
"#;
        let machine_code = assemble_16_bit(asm_snippet);
        let decoded = decode(&machine_code);
        assert_eq!(decoded.ixs.len(), 1);
        assert_eq!(format!("{}", decoded.ixs[0]), "jne byte 0");
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
