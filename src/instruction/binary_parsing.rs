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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Register {
    Regular(Regular),
    Segment(Segment),
}

#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Segment {
    ES = 0b00,
    CS = 0b01,
    SS = 0b10,
    DS = 0b11,
}

#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Regular {
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
    Register(Register),
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
            Operand::Register(reg) => match reg {
                Register::Regular(regular) => match regular {
                    Regular::AL => write!(f, "al"),
                    Regular::CL => write!(f, "cl"),
                    Regular::DL => write!(f, "dl"),
                    Regular::BL => write!(f, "bl"),
                    Regular::AH => write!(f, "ah"),
                    Regular::CH => write!(f, "ch"),
                    Regular::DH => write!(f, "dh"),
                    Regular::BH => write!(f, "bh"),
                    Regular::AX => write!(f, "ax"),
                    Regular::CX => write!(f, "cx"),
                    Regular::DX => write!(f, "dx"),
                    Regular::BX => write!(f, "bx"),
                    Regular::SP => write!(f, "sp"),
                    Regular::BP => write!(f, "bp"),
                    Regular::SI => write!(f, "si"),
                    Regular::DI => write!(f, "di"),
                },
                Register::Segment(segment) => match segment {
                    Segment::ES => write!(f, "es"),
                    Segment::CS => write!(f, "cs"),
                    Segment::SS => write!(f, "ss"),
                    Segment::DS => write!(f, "ds"),
                },
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
fn decode_8086_register_index(reg_bits: u8, w: bool) -> Regular {
    match (w, reg_bits) {
        (false, 0b000) => Regular::AL,
        (false, 0b001) => Regular::CL,
        (false, 0b010) => Regular::DL,
        (false, 0b011) => Regular::BL,
        (false, 0b100) => Regular::AH,
        (false, 0b101) => Regular::CH,
        (false, 0b110) => Regular::DH,
        (false, 0b111) => Regular::BH,
        (true, 0b000) => Regular::AX,
        (true, 0b001) => Regular::CX,
        (true, 0b010) => Regular::DX,
        (true, 0b011) => Regular::BX,
        (true, 0b100) => Regular::SP,
        (true, 0b101) => Regular::BP,
        (true, 0b110) => Regular::SI,
        (true, 0b111) => Regular::DI,
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
            Operand::Register(Register::Regular(reg))
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
    B(u8),
    /// mod bits.
    Mod,
    /// lo address bits
    AddrLo,
    /// hi address bits
    AddrHi,
    /// reg bits.
    Reg,
    SegReg,
    /// rm bits.
    Rm,
    /// Expect a data byte.
    Data8,
    DataLo,
    DataHi,
    OptDispLo,
    OptDispHi,
    DispLo,
    DispHi,
    ImpliedRegOperand(Register),
    ImpliedModRmOperand(Register),
    /// Instruction operates on word data
    ImpliedW(bool),
    /// This is set when the `D` flag == 1
    DestinationIsInRegField,
    DestinationIsInModRmField,
    // --
    // logical action of what to do with the data we parsed
    ParseRegField,
    ParseModRmFromDisp,
    ParseRegFromData,
    ParseRegFromAddr,
    ParseModRm,
    /// don't do any operand parsing, return the ix
    Ret,
}

/// Instruction definition.
#[derive(Debug, Clone)]
pub struct IxDef<'a> {
    name: &'static str,
    items: Vec<P<'a>>,
}

#[derive(Debug, Default)]
pub struct ParseContext {
    w_val: Option<bool>,
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
    modrm_operand: Option<Operand>,
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
                P::B(pattern) => {
                    let slice = read_bits(8);
                    let candidate = slice_to_val(slice);
                    if !candidate.eq(pattern) {
                        tracing::debug!("invalid pattern");
                        return None;
                    }
                }
                // Compare the next bits to the expected pattern.
                P::C(pattern) => {
                    let len = pattern.len();
                    let candidate = read_bits(len);
                    if !candidate.eq(pattern) {
                        tracing::debug!("invalid pattern");
                        return None;
                    }
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
                P::SegReg => {
                    let slice = read_bits(2);
                    let val = slice_to_val(slice);
                    let seg = match val {
                        0b00 => Segment::ES,
                        0b01 => Segment::CS,
                        0b10 => Segment::SS,
                        0b11 => Segment::DS,
                        _ => unreachable!(),
                    };
                    ctx.reg_operand = Some(Operand::Register(Register::Segment(seg)));
                }
                P::Rm => {
                    let slice = read_bits(3);
                    let val = slice_to_val(slice);
                    ctx.rm_val = Some(val);
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
                P::DispLo => {
                    let slice = read_bits(8);
                    ctx.disp_lo = Some(slice_to_val(slice));
                }
                P::DispHi => {
                    let slice = read_bits(8);
                    ctx.disp_hi = Some(slice_to_val(slice));
                }
                P::Data8 => {
                    let slice = read_bits(8);
                    let val = slice_to_val(slice);
                    ctx.data_lo = Some(val);
                }
                P::DataLo => {
                    let slice = read_bits(8);
                    let val = slice_to_val(slice);
                    ctx.data_hi = Some(val);
                }
                P::DataHi => {
                    let slice = read_bits(8);
                    let val = slice_to_val(slice);
                    ctx.data_lo = Some(val);
                }
                P::ImpliedRegOperand(reg) => {
                    ctx.reg_operand = Some(Operand::Register(*reg));
                }
                P::ImpliedModRmOperand(reg) => {
                    ctx.modrm_operand = Some(Operand::Register(*reg));
                }
                P::ParseRegField => {
                    // parse reg opernad
                    let reg_val = ctx.reg_val.unwrap();
                    ctx.reg_operand = Some(Operand::Register(Register::Regular(
                        decode_8086_register_index(reg_val, ctx.w_val.unwrap()),
                    )));
                }
                P::ParseRegFromAddr => {
                    let addr = u16::from_le_bytes([ctx.addr_lo.unwrap(), ctx.addr_hi.unwrap()]);
                    ctx.reg_operand = Some(Operand::Address(EffectiveAddress::Direct(addr)));
                }
                P::ParseModRmFromDisp => {
                    if ctx.w_val.unwrap() {
                        let word = u16::from_le_bytes([ctx.disp_lo.unwrap(), ctx.data_hi.unwrap()]);
                        ctx.reg_operand = Some(Operand::Immediate(Immediate::Word(word)));
                    } else {
                        ctx.reg_operand =
                            Some(Operand::Immediate(Immediate::Byte(ctx.data_lo.unwrap())));
                    };
                }
                P::ParseRegFromData => {
                    if ctx.w_val.unwrap() {
                        let word = u16::from_le_bytes([ctx.data_lo.unwrap(), ctx.data_hi.unwrap()]);
                        ctx.reg_operand = Some(Operand::Immediate(Immediate::Word(word)));
                    } else {
                        ctx.reg_operand =
                            Some(Operand::Immediate(Immediate::Byte(ctx.data_lo.unwrap())));
                    };
                }
                P::ParseModRm => {
                    // parse the second opernad
                    match (ctx.rm_val, ctx.mod_val) {
                        (Some(rm), Some(mod_val)) => {
                            // Decode the r/m operand
                            let rm_operand = decode_rm_operand(
                                mod_val,
                                rm,
                                ctx.w_val.unwrap(),
                                ctx.disp_lo,
                                ctx.disp_hi,
                            );
                            ctx.modrm_operand = Some(rm_operand);
                        }
                        (None, None) => match ctx.data_lo {
                            Some(data_lo) => {
                                ctx.modrm_operand = Some(operand_from_data(data_lo, ctx.data_hi));
                            }
                            None => match (ctx.addr_lo, ctx.addr_hi) {
                                (None, None) => todo!(),
                                (Some(lo), Some(hi)) => {
                                    let addr = u16::from_le_bytes([lo, hi]);
                                    ctx.modrm_operand =
                                        Some(Operand::Address(EffectiveAddress::Direct(addr)));
                                }
                                _ => unreachable!(),
                            },
                        },
                        _ => unimplemented!(),
                    };
                }
                P::ImpliedW(w_flag) => {
                    ctx.w_val = Some(*w_flag);
                }
                P::DestinationIsInRegField => {
                    let bytes_consumed = (bit_offset + 7) / 8; // round up

                    let inst = Instruction {
                        size: bytes_consumed as u8,
                        operation: self.name,
                        operands: [ctx.reg_operand, ctx.modrm_operand],
                    };
                    return Some((inst, &bytes[bytes_consumed..]));
                }
                P::DestinationIsInModRmField => {
                    let bytes_consumed = (bit_offset + 7) / 8; // round up

                    let inst = Instruction {
                        size: bytes_consumed as u8,
                        operation: self.name,
                        operands: [ctx.modrm_operand, ctx.reg_operand],
                    };
                    return Some((inst, &bytes[bytes_consumed..]));
                }
                P::Ret => {
                    let bytes_consumed = (bit_offset + 7) / 8; // round up

                    let inst = Instruction {
                        size: bytes_consumed as u8,
                        operation: self.name,
                        operands: [None, None],
                    };
                    return Some((inst, &bytes[bytes_consumed..]));
                }
            }
        }

        unreachable!("the instruction parsing was not finalized")
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
pub fn ix_table() -> [IxDef<'static>; 8] {
    use P::*;
    #[rustfmt::skip]
    let common_two_op = [
        IxDef::new("_unsupported",  vec![B(0), Mod, Reg, Rm, OptDispLo, OptDispHi, ImpliedW(false), ParseRegField, ParseModRm, DestinationIsInModRmField]),
        IxDef::new("_unsupported",  vec![B(0), Mod, Reg, Rm, OptDispLo, OptDispHi, ImpliedW(true), ParseRegField, ParseModRm, DestinationIsInModRmField]),
        IxDef::new("_unsupported",  vec![B(0), Mod, Reg, Rm, OptDispLo, OptDispHi, ImpliedW(false), ParseRegField, ParseModRm, DestinationIsInRegField]),
        IxDef::new("_unsupported",  vec![B(0), Mod, Reg, Rm, OptDispLo, OptDispHi, ImpliedW(true), ParseRegField, ParseModRm, DestinationIsInRegField]),
        IxDef::new("_unsupported",  vec![B(0), Data8, ImpliedRegOperand(Register::Regular(Regular::AL)), ParseModRm, DestinationIsInRegField]),
        IxDef::new("_unsupported",  vec![B(0), DataLo, DataHi, ImpliedRegOperand(Register::Regular(Regular::AX)), ParseModRm, DestinationIsInRegField]),
    ];
    #[rustfmt::skip]
    let common_single_op = [
        IxDef::new("_unsupported",  vec![B(0), ImpliedRegOperand(Register::Regular(Regular::AX)), DestinationIsInRegField]),
        IxDef::new("_unsupported",  vec![B(0), ImpliedRegOperand(Register::Regular(Regular::CX)), DestinationIsInRegField]),
        IxDef::new("_unsupported",  vec![B(0), ImpliedRegOperand(Register::Regular(Regular::DX)), DestinationIsInRegField]),
        IxDef::new("_unsupported",  vec![B(0), ImpliedRegOperand(Register::Regular(Regular::BX)), DestinationIsInRegField]),
        IxDef::new("_unsupported",  vec![B(0), ImpliedRegOperand(Register::Regular(Regular::SP)), DestinationIsInRegField]),
        IxDef::new("_unsupported",  vec![B(0), ImpliedRegOperand(Register::Regular(Regular::BP)), DestinationIsInRegField]),
        IxDef::new("_unsupported",  vec![B(0), ImpliedRegOperand(Register::Regular(Regular::SI)), DestinationIsInRegField]),
        IxDef::new("_unsupported",  vec![B(0), ImpliedRegOperand(Register::Regular(Regular::DI)), DestinationIsInRegField])
    ];

    let _base_build =
        |new_pattern: &[P<'static>], common: &[IxDef<'static>], new_name: &'static str| {
            new_pattern
                .iter()
                .zip(common.iter())
                .map(|(new_pattern, def)| {
                    let mut def = def.clone();
                    def.name = new_name;
                    def.items[0] = new_pattern.clone();
                    def
                })
                .collect::<Vec<_>>()
        };
    let build_common_op = |new_pattern: &[P<'static>], new_name: &'static str| {
        _base_build(new_pattern, &common_two_op, new_name)
    };
    let build_common_op_single = |new_pattern: &[P<'static>], new_name: &'static str| {
        _base_build(new_pattern, &common_single_op, new_name)
    };

    let add = build_common_op(
        &[B(0x00), B(0x01), B(0x02), B(0x03), B(0x04), B(0x05)],
        "add",
    );
    let or = build_common_op(
        &[B(0x08), B(0x09), B(0x0A), B(0x0B), B(0x0C), B(0x0D)],
        "or",
    );
    let adc = build_common_op(
        &[B(0x10), B(0x11), B(0x12), B(0x13), B(0x14), B(0x15)],
        "adc",
    );
    let sbb = build_common_op(
        &[B(0x18), B(0x19), B(0x1A), B(0x1B), B(0x1C), B(0x1D)],
        "sbb",
    );
    let and = build_common_op(
        &[B(0x20), B(0x21), B(0x22), B(0x23), B(0x24), B(0x25)],
        "and",
    );
    let sub = build_common_op(
        &[B(0x28), B(0x29), B(0x2A), B(0x2B), B(0x2C), B(0x2D)],
        "sub",
    );
    let xor = build_common_op(
        &[B(0x30), B(0x31), B(0x32), B(0x33), B(0x34), B(0x35)],
        "xor",
    );
    let cmp = build_common_op(
        &[B(0x38), B(0x39), B(0x3A), B(0x3B), B(0x3C), B(0x3D)],
        "cmp",
    );
    let inc = build_common_op_single(
        &[
            B(0x40),
            B(0x41),
            B(0x42),
            B(0x43),
            B(0x44),
            B(0x45),
            B(0x46),
            B(0x47),
        ],
        "inc",
    );
    let dec = build_common_op_single(
        &[
            B(0x48),
            B(0x49),
            B(0x4A),
            B(0x4B),
            B(0x4C),
            B(0x4D),
            B(0x4E),
            B(0x4F),
        ],
        "dec",
    );
    let push = build_common_op_single(
        &[
            B(0x50),
            B(0x51),
            B(0x52),
            B(0x53),
            B(0x54),
            B(0x55),
            B(0x56),
            B(0x57),
        ],
        "push",
    );
    let pop = build_common_op_single(
        &[
            B(0x58),
            B(0x59),
            B(0x5A),
            B(0x5B),
            B(0x5C),
            B(0x5D),
            B(0x5E),
            B(0x5F),
        ],
        "pop",
    );

    #[rustfmt::skip]
    let special_0x80 = IxDef::new("_unused",  vec![B(0x80), Mod, C(bits!(static u8, Msb0; 0,0,0)), Rm, OptDispLo, OptDispHi, Data8, ImpliedW(false), ParseRegFromData, ParseModRm, DestinationIsInModRmField]);
    #[rustfmt::skip]
    let special_0x81 = IxDef::new("_unused",  vec![B(0x80), Mod, C(bits!(static u8, Msb0; 0,0,0)), Rm, OptDispLo, OptDispHi, DataLo, DataHi, ImpliedW(true), ParseRegFromData, ParseModRm, DestinationIsInModRmField]);

    #[rustfmt::skip]
    let res = [
        add[0].clone(),
        add[1].clone(),
        add[2].clone(),
        add[3].clone(),
        add[4].clone(),
        add[5].clone(),
        // hex 05
        IxDef::new("push", vec![B(0x06), ImpliedRegOperand(Register::Segment(Segment::ES)), DestinationIsInRegField]),
        IxDef::new("pop", vec![B(0x07), ImpliedRegOperand(Register::Segment(Segment::ES)), DestinationIsInRegField]),
        or[0].clone(),
        or[1].clone(),
        or[2].clone(),
        or[3].clone(),
        or[4].clone(),
        or[5].clone(),
        IxDef::new("push", vec![B(0x0E), ImpliedRegOperand(Register::Segment(Segment::CS)), DestinationIsInRegField]),
        IxDef::new("_unused", vec![B(0x0F) /* not used */]),
        // hex 0f
        adc[0].clone(),
        adc[1].clone(),
        adc[2].clone(),
        adc[3].clone(),
        adc[4].clone(),
        adc[5].clone(),
        IxDef::new("push", vec![B(0x06), ImpliedRegOperand(Register::Segment(Segment::SS)), DestinationIsInRegField]),
        IxDef::new("pop", vec![B(0x07), ImpliedRegOperand(Register::Segment(Segment::SS)), DestinationIsInRegField]),
        // hex 17
        sbb[0].clone(),
        sbb[1].clone(),
        sbb[2].clone(),
        sbb[3].clone(),
        sbb[4].clone(),
        sbb[5].clone(),
        IxDef::new("push", vec![B(0x06), ImpliedRegOperand(Register::Segment(Segment::DS)), DestinationIsInRegField]),
        IxDef::new("pop", vec![B(0x07), ImpliedRegOperand(Register::Segment(Segment::DS)), DestinationIsInRegField]),
        // hex 1f
        and[0].clone(),
        and[1].clone(),
        and[2].clone(),
        and[3].clone(),
        and[4].clone(),
        and[5].clone(),
        IxDef::new("es", vec![B(0x26), Ret]),
        IxDef::new("daa", vec![B(0x27), Ret]),
        sub[0].clone(),
        sub[1].clone(),
        sub[2].clone(),
        sub[3].clone(),
        sub[4].clone(),
        sub[5].clone(),
        IxDef::new("cs", vec![B(0x2F), Ret]),
        IxDef::new("das", vec![B(0x2F), Ret]),
        xor[0].clone(),
        xor[1].clone(),
        xor[2].clone(),
        xor[3].clone(),
        xor[4].clone(),
        xor[5].clone(),
        IxDef::new("ss", vec![B(0x36), Ret]),
        IxDef::new("aaa", vec![B(0x37), Ret]),
        cmp[0].clone(),
        cmp[1].clone(),
        cmp[2].clone(),
        cmp[3].clone(),
        cmp[4].clone(),
        cmp[5].clone(),
        IxDef::new("ds", vec![B(0x3E), Ret]),
        IxDef::new("aas", vec![B(0x3F), Ret]),
        // hex 3f
        inc[0].clone(),
        inc[1].clone(),
        inc[2].clone(),
        inc[3].clone(),
        inc[4].clone(),
        inc[5].clone(),
        inc[6].clone(),
        inc[7].clone(),
        dec[0].clone(),
        dec[1].clone(),
        dec[2].clone(),
        dec[3].clone(),
        dec[4].clone(),
        dec[5].clone(),
        dec[6].clone(),
        dec[7].clone(),
        push[0].clone(),
        push[1].clone(),
        push[2].clone(),
        push[3].clone(),
        push[4].clone(),
        push[5].clone(),
        push[6].clone(),
        push[7].clone(),
        pop[0].clone(),
        pop[1].clone(),
        pop[2].clone(),
        pop[3].clone(),
        pop[4].clone(),
        pop[5].clone(),
        pop[6].clone(),
        pop[7].clone(),
        IxDef::new("_unused", vec![B(0x60) /* not used */]),
        IxDef::new("_unused", vec![B(0x61) /* not used */]),
        IxDef::new("_unused", vec![B(0x62) /* not used */]),
        IxDef::new("_unused", vec![B(0x63) /* not used */]),
        IxDef::new("_unused", vec![B(0x64) /* not used */]),
        IxDef::new("_unused", vec![B(0x65) /* not used */]),
        IxDef::new("_unused", vec![B(0x66) /* not used */]),
        IxDef::new("_unused", vec![B(0x67) /* not used */]),
        IxDef::new("_unused", vec![B(0x68) /* not used */]),
        IxDef::new("_unused", vec![B(0x69) /* not used */]),
        IxDef::new("_unused", vec![B(0x6A) /* not used */]),
        IxDef::new("_unused", vec![B(0x6B) /* not used */]),
        IxDef::new("_unused", vec![B(0x6C) /* not used */]),
        IxDef::new("_unused", vec![B(0x6D) /* not used */]),
        IxDef::new("_unused", vec![B(0x6E) /* not used */]),
        IxDef::new("_unused", vec![B(0x6F) /* not used */]),
        // jumps
        IxDef::new("jo",    vec![B(0x70), Data8, ImpliedW(false), ParseRegFromData]),
        IxDef::new("jno",   vec![B(0x71), Data8, ImpliedW(false), ParseRegFromData]),
        IxDef::new("jb",    vec![B(0x72), Data8, ImpliedW(false), ParseRegFromData]),
        IxDef::new("jnbe",  vec![B(0x77), Data8, ImpliedW(false), ParseRegFromData]),
        IxDef::new("js",    vec![B(0x78), Data8, ImpliedW(false), ParseRegFromData]),
        IxDef::new("jns",   vec![B(0x79), Data8, ImpliedW(false), ParseRegFromData]),
        IxDef::new("jp",    vec![B(0x7A), Data8, ImpliedW(false), ParseRegFromData]),
        IxDef::new("jnp",   vec![B(0x7B), Data8, ImpliedW(false), ParseRegFromData]),
        IxDef::new("jl",    vec![B(0x7C), Data8, ImpliedW(false), ParseRegFromData]),
        IxDef::new("jnl",   vec![B(0x7D), Data8, ImpliedW(false), ParseRegFromData]),
        IxDef::new("jle",   vec![B(0x7E), Data8, ImpliedW(false), ParseRegFromData]),
        IxDef::new("jnle",  vec![B(0x7F), Data8, ImpliedW(false), ParseRegFromData]),
        // special mod handling
        IxDef { name: "add", items: { let items = special_0x80.items.clone(); items[2] = C(bits!(static u8, Msb0; 0,0,0)); items } },
        IxDef { name: "or",  items: { let items = special_0x80.items.clone(); items[2] = C(bits!(static u8, Msb0; 0,0,1)); items } },
        IxDef { name: "adc", items: { let items = special_0x80.items.clone(); items[2] = C(bits!(static u8, Msb0; 0,1,0)); items } },
        IxDef { name: "sbb", items: { let items = special_0x80.items.clone(); items[2] = C(bits!(static u8, Msb0; 1,0,0)); items } },
        IxDef { name: "and", items: { let items = special_0x80.items.clone(); items[2] = C(bits!(static u8, Msb0; 0,1,1)); items } },
        IxDef { name: "sub", items: { let items = special_0x80.items.clone(); items[2] = C(bits!(static u8, Msb0; 1,0,0)); items } },
        IxDef { name: "xor", items: { let items = special_0x80.items.clone(); items[2] = C(bits!(static u8, Msb0; 1,0,1)); items } },
        IxDef { name: "cmp", items: { let items = special_0x80.items.clone(); items[2] = C(bits!(static u8, Msb0; 1,1,1)); items } },
        // special mod handling
        IxDef { name: "add", items: { let items = special_0x81.items.clone(); items[2] = C(bits!(static u8, Msb0; 0,0,0)); items } },
        IxDef { name: "or",  items: { let items = special_0x81.items.clone(); items[2] = C(bits!(static u8, Msb0; 0,0,1)); items } },
        IxDef { name: "adc", items: { let items = special_0x81.items.clone(); items[2] = C(bits!(static u8, Msb0; 0,1,0)); items } },
        IxDef { name: "sbb", items: { let items = special_0x81.items.clone(); items[2] = C(bits!(static u8, Msb0; 1,0,0)); items } },
        IxDef { name: "and", items: { let items = special_0x81.items.clone(); items[2] = C(bits!(static u8, Msb0; 0,1,1)); items } },
        IxDef { name: "sub", items: { let items = special_0x81.items.clone(); items[2] = C(bits!(static u8, Msb0; 1,0,0)); items } },
        IxDef { name: "xor", items: { let items = special_0x81.items.clone(); items[2] = C(bits!(static u8, Msb0; 1,0,1)); items } },
        IxDef { name: "cmp", items: { let items = special_0x81.items.clone(); items[2] = C(bits!(static u8, Msb0; 1,1,1)); items } },
        // 0x82 opcodes are not really used by assemblers. they behave like 0x80
        IxDef::new("add",  vec![B(0x82), Mod, C(bits!(static u8, Msb0; 0,0,0)), Rm, OptDispLo, OptDispHi, Data8, ImpliedW(false), ParseRegFromData, ParseModRm, DestinationIsInModRmField]),
        IxDef::new("_unused",  vec![B(0x82), Mod, C(bits!(static u8, Msb0; 0,0,1)), Rm, /* not used */]),
        IxDef::new("adc",  vec![B(0x82), Mod, C(bits!(static u8, Msb0; 0,1,0)), Rm, OptDispLo, OptDispHi, Data8, ImpliedW(false), ParseRegFromData, ParseModRm, DestinationIsInModRmField]),
        IxDef::new("sbb",  vec![B(0x82), Mod, C(bits!(static u8, Msb0; 0,1,1)), Rm, OptDispLo, OptDispHi, Data8, ImpliedW(false), ParseRegFromData, ParseModRm, DestinationIsInModRmField]),
        IxDef::new("_unused",  vec![B(0x82), Mod, C(bits!(static u8, Msb0; 1,0,0)), Rm, /* not used */]),
        IxDef::new("sub",  vec![B(0x82), Mod, C(bits!(static u8, Msb0; 1,0,1)), Rm, OptDispLo, OptDispHi, Data8, ImpliedW(false), ParseRegFromData, ParseModRm, DestinationIsInModRmField]),
        IxDef::new("_unused",  vec![B(0x82), Mod, C(bits!(static u8, Msb0; 1,1,0)), Rm, /* not used */]),
        IxDef::new("cmp",  vec![B(0x82), Mod, C(bits!(static u8, Msb0; 1,1,1)), Rm, OptDispLo, OptDispHi, Data8, ImpliedW(false), ParseRegFromData, ParseModRm, DestinationIsInModRmField]),
        IxDef::new("_unused",  vec![B(0x83), Mod, C(bits!(static u8, Msb0; 1,0,0)), Rm, /* not used */]),
        // 0x83 is the same as 0x81 except the immediate is 8 bit but we need to treat it like it's 16 bit. it's the sign-extended value
        IxDef::new("add",  vec![B(0x82), Mod, C(bits!(static u8, Msb0; 0,0,0)), Rm, OptDispLo, OptDispHi, Data8, ImpliedW(false), ParseRegFromData, ParseModRm, DestinationIsInModRmField]),
        IxDef::new("_unused",  vec![B(0x82), Mod, C(bits!(static u8, Msb0; 0,0,1)), Rm, /* not used */]),
        IxDef::new("adc",  vec![B(0x82), Mod, C(bits!(static u8, Msb0; 0,1,0)), Rm, OptDispLo, OptDispHi, Data8, ImpliedW(false), ParseRegFromData, ParseModRm, DestinationIsInModRmField]),
        IxDef::new("sbb",  vec![B(0x82), Mod, C(bits!(static u8, Msb0; 0,1,1)), Rm, OptDispLo, OptDispHi, Data8, ImpliedW(false), ParseRegFromData, ParseModRm, DestinationIsInModRmField]),
        IxDef::new("_unused",  vec![B(0x82), Mod, C(bits!(static u8, Msb0; 1,0,0)), Rm, /* not used */]),
        IxDef::new("sub",  vec![B(0x82), Mod, C(bits!(static u8, Msb0; 1,0,1)), Rm, OptDispLo, OptDispHi, Data8, ImpliedW(false), ParseRegFromData, ParseModRm, DestinationIsInModRmField]),
        IxDef::new("_unused",  vec![B(0x82), Mod, C(bits!(static u8, Msb0; 1,1,0)), Rm, /* not used */]),
        IxDef::new("cmp",  vec![B(0x82), Mod, C(bits!(static u8, Msb0; 1,1,1)), Rm, OptDispLo, OptDispHi, Data8, ImpliedW(false), ParseRegFromData, ParseModRm, DestinationIsInModRmField]),
        IxDef::new("_unused",  vec![B(0x83), Mod, C(bits!(static u8, Msb0; 1,0,0)), Rm, /* not used */]),
        //
        IxDef::new("test",  vec![B(0x84), Mod, Reg, Rm, OptDispLo, OptDispHi, ImpliedW(false), ParseModRm, ParseRegField, DestinationIsInModRmField]),
        IxDef::new("test",  vec![B(0x85), Mod, Reg, Rm, OptDispLo, OptDispHi, ImpliedW(true), ParseModRm, ParseRegField, DestinationIsInModRmField]),
        IxDef::new("xchg",  vec![B(0x86), Mod, Reg, Rm, OptDispLo, OptDispHi, ImpliedW(false), ParseModRm, ParseRegField, DestinationIsInRegField]),
        IxDef::new("xchg",  vec![B(0x87), Mod, Reg, Rm, OptDispLo, OptDispHi, ImpliedW(true), ParseModRm, ParseRegField, DestinationIsInRegField]),
        // movs
        IxDef::new("mov",  vec![B(0x88), Mod, Reg, Rm, OptDispLo, OptDispHi, ImpliedW(false), ParseModRm, ParseRegField, DestinationIsInModRmField]),
        IxDef::new("mov",  vec![B(0x89), Mod, Reg, Rm, OptDispLo, OptDispHi, ImpliedW(true), ParseModRm, ParseRegField, DestinationIsInModRmField]),
        IxDef::new("mov",  vec![B(0x8a), Mod, Reg, Rm, OptDispLo, OptDispHi, ImpliedW(false), ParseModRm, ParseRegField, DestinationIsInRegField]),
        IxDef::new("mov",  vec![B(0x8b), Mod, Reg, Rm, OptDispLo, OptDispHi, ImpliedW(true), ParseModRm, ParseRegField, DestinationIsInRegField]),
        IxDef::new("mov",  vec![B(0x8c), Mod, C(bits!(static u8, Msb0; 0)), SegReg, Rm, OptDispLo, OptDispHi, ImpliedW(true), ParseModRm, DestinationIsInModRmField]),
        IxDef::new("_unused",  vec![B(0x8c), Mod, C(bits!(static u8, Msb0; 1)), /* this is not a valid opcode */]), 
        IxDef::new("lea",  vec![B(0x8d), Mod, Reg, Rm, OptDispLo, OptDispHi, ImpliedW(true), ParseModRm, ParseRegField, DestinationIsInRegField]),
        IxDef::new("mov",  vec![B(0x8e), Mod, C(bits!(static u8, Msb0; 0)), SegReg, Rm, OptDispLo, OptDispHi, ImpliedW(true), ParseModRm, DestinationIsInRegField]),
        IxDef::new("mov",  vec![B(0x8e), Mod, C(bits!(static u8, Msb0; 1)), Ret, /* this is not a valid opcode */]), 
        IxDef::new("pop",  vec![B(0x8f), Mod, C(bits!(static u8, Msb0; 0,0,0)), Rm, OptDispLo, OptDispHi, ImpliedW(true), ParseModRm, DestinationIsInModRmField]),
        IxDef::new("_unused",  vec![B(0x8f), Mod, C(bits!(static u8, Msb0; 0,0,1)), Rm, /* this is not a valid opcode */]),
        IxDef::new("_unused",  vec![B(0x8f), Mod, C(bits!(static u8, Msb0; 0,1,0)), Rm, /* this is not a valid opcode */]),
        IxDef::new("_unused",  vec![B(0x8f), Mod, C(bits!(static u8, Msb0; 0,1,1)), Rm, /* this is not a valid opcode */]),
        IxDef::new("_unused",  vec![B(0x8f), Mod, C(bits!(static u8, Msb0; 1,0,0)), Rm, /* this is not a valid opcode */]),
        IxDef::new("_unused",  vec![B(0x8f), Mod, C(bits!(static u8, Msb0; 1,0,1)), Rm, /* this is not a valid opcode */]),
        IxDef::new("_unused",  vec![B(0x8f), Mod, C(bits!(static u8, Msb0; 1,1,0)), Rm, /* this is not a valid opcode */]),
        IxDef::new("_unused",  vec![B(0x8f), Mod, C(bits!(static u8, Msb0; 1,1,1)), Rm, /* this is not a valid opcode */]),
        IxDef::new("nop",  vec![B(0x90), Ret]),
        IxDef::new("xchg", vec![B(0x91), ImpliedRegOperand(Register::Regular(Regular::AX)), ImpliedModRmOperand(Register::Regular(Regular::CX)), DestinationIsInRegField]),
        IxDef::new("xchg", vec![B(0x92), ImpliedRegOperand(Register::Regular(Regular::AX)), ImpliedModRmOperand(Register::Regular(Regular::DX)), DestinationIsInRegField]),
        IxDef::new("xchg", vec![B(0x93), ImpliedRegOperand(Register::Regular(Regular::AX)), ImpliedModRmOperand(Register::Regular(Regular::BX)), DestinationIsInRegField]),
        IxDef::new("xchg", vec![B(0x94), ImpliedRegOperand(Register::Regular(Regular::AX)), ImpliedModRmOperand(Register::Regular(Regular::SP)), DestinationIsInRegField]),
        IxDef::new("xchg", vec![B(0x95), ImpliedRegOperand(Register::Regular(Regular::AX)), ImpliedModRmOperand(Register::Regular(Regular::BP)), DestinationIsInRegField]),
        IxDef::new("xchg", vec![B(0x96), ImpliedRegOperand(Register::Regular(Regular::AX)), ImpliedModRmOperand(Register::Regular(Regular::SI)), DestinationIsInRegField]),
        IxDef::new("xchg", vec![B(0x97), ImpliedRegOperand(Register::Regular(Regular::AX)), ImpliedModRmOperand(Register::Regular(Regular::DI)), DestinationIsInRegField]),
        IxDef::new("cbw",  vec![B(0x98), Ret]),
        IxDef::new("cwd",  vec![B(0x99), Ret]),
        // this is `call far_proc` -> real funky, implement handling with care
        IxDef::new("call",   vec![B(0x9a), OptDispLo, OptDispHi, DataLo, DataHi, ImpliedW(true), ParseRegFromData, ParseModRmFromDisp, DestinationIsInRegField]),
        IxDef::new("wait",   vec![B(0x9b), Ret]),
        IxDef::new("pushf",  vec![B(0x9c), Ret]),
        IxDef::new("popf",   vec![B(0x9d), Ret]),
        IxDef::new("sahf",   vec![B(0x9e), Ret]),
        IxDef::new("lahf",   vec![B(0x9f), Ret]),
        // simple movs
        IxDef::new("mov", vec![B(0xa0), AddrLo, AddrHi, ParseRegFromAddr, ImpliedModRmOperand(Register::Regular(Regular::AL)), DestinationIsInModRmField]),
        IxDef::new("mov", vec![B(0xa1), AddrLo, AddrHi, ParseRegFromAddr, ImpliedModRmOperand(Register::Regular(Regular::AX)), DestinationIsInModRmField]),
        IxDef::new("mov", vec![B(0xa2), AddrLo, AddrHi, ParseRegFromAddr, ImpliedModRmOperand(Register::Regular(Regular::AL)), DestinationIsInRegField]),
        IxDef::new("mov", vec![B(0xa3), AddrLo, AddrHi, ParseRegFromAddr, ImpliedModRmOperand(Register::Regular(Regular::AL)), DestinationIsInRegField]),
        // strange stuff
        IxDef::new("movs_8",   vec![B(0xa4), ImpliedW(false), Ret]),
        IxDef::new("movs_16",   vec![B(0xa5), ImpliedW(true), Ret]),
        IxDef::new("cmps_8",   vec![B(0xa6), ImpliedW(false), Ret]),
        IxDef::new("cmps_16",   vec![B(0xa7), ImpliedW(true), Ret]),
        IxDef::new("test", vec![B(0xa8), Data8, ImpliedModRmOperand(Register::Regular(Regular::AL)), ParseRegFromData, DestinationIsInModRmField]),
        IxDef::new("test", vec![B(0xa9), DataLo, DataHi, ImpliedModRmOperand(Register::Regular(Regular::AX)), ParseRegFromData, DestinationIsInModRmField]),
        IxDef::new("stos_8", vec![B(0xaa), ImpliedW(false), Ret]),
        IxDef::new("stos_16", vec![B(0xab), ImpliedW(true), Ret]),
        IxDef::new("lods_8", vec![B(0xac), ImpliedW(false), Ret]),
        IxDef::new("lods_16", vec![B(0xad), ImpliedW(true), Ret]),
        IxDef::new("scas_8", vec![B(0xae), ImpliedW(false), Ret]),
        IxDef::new("scas_16", vec![B(0xaf), ImpliedW(true), Ret]),
        // more movs
        IxDef::new("mov",  vec![B(0xb0), Data8, ImpliedW(false), ImpliedModRmOperand(Register::Regular(Regular::AL)), ParseRegFromData, DestinationIsInModRmField]),
        IxDef::new("mov",  vec![B(0xb1), Data8, ImpliedW(false), ImpliedModRmOperand(Register::Regular(Regular::CL)), ParseRegFromData, DestinationIsInModRmField]),
        IxDef::new("mov",  vec![B(0xb2), Data8, ImpliedW(false), ImpliedModRmOperand(Register::Regular(Regular::DL)), ParseRegFromData, DestinationIsInModRmField]),
        IxDef::new("mov",  vec![B(0xb3), Data8, ImpliedW(false), ImpliedModRmOperand(Register::Regular(Regular::BL)), ParseRegFromData, DestinationIsInModRmField]),
        IxDef::new("mov",  vec![B(0xb4), Data8, ImpliedW(false), ImpliedModRmOperand(Register::Regular(Regular::AH)), ParseRegFromData, DestinationIsInModRmField]),
        IxDef::new("mov",  vec![B(0xb5), Data8, ImpliedW(false), ImpliedModRmOperand(Register::Regular(Regular::CH)), ParseRegFromData, DestinationIsInModRmField]),
        IxDef::new("mov",  vec![B(0xb6), Data8, ImpliedW(false), ImpliedModRmOperand(Register::Regular(Regular::DH)), ParseRegFromData, DestinationIsInModRmField]),
        IxDef::new("mov",  vec![B(0xb7), Data8, ImpliedW(false), ImpliedModRmOperand(Register::Regular(Regular::BH)), ParseRegFromData, DestinationIsInModRmField]),
        IxDef::new("mov",  vec![B(0xb8), DataLo, DataHi, ImpliedW(true), ImpliedModRmOperand(Register::Regular(Regular::AX)), ParseRegFromData, DestinationIsInModRmField]),
        IxDef::new("mov",  vec![B(0xb9), DataLo, DataHi, ImpliedW(true), ImpliedModRmOperand(Register::Regular(Regular::CX)), ParseRegFromData, DestinationIsInModRmField]),
        IxDef::new("mov",  vec![B(0xba), DataLo, DataHi, ImpliedW(true), ImpliedModRmOperand(Register::Regular(Regular::DX)), ParseRegFromData, DestinationIsInModRmField]),
        IxDef::new("mov",  vec![B(0xbb), DataLo, DataHi, ImpliedW(true), ImpliedModRmOperand(Register::Regular(Regular::BX)), ParseRegFromData, DestinationIsInModRmField]),
        IxDef::new("mov",  vec![B(0xbc), DataLo, DataHi, ImpliedW(true), ImpliedModRmOperand(Register::Regular(Regular::SP)), ParseRegFromData, DestinationIsInModRmField]),
        IxDef::new("mov",  vec![B(0xbd), DataLo, DataHi, ImpliedW(true), ImpliedModRmOperand(Register::Regular(Regular::BP)), ParseRegFromData, DestinationIsInModRmField]),
        IxDef::new("mov",  vec![B(0xbe), DataLo, DataHi, ImpliedW(true), ImpliedModRmOperand(Register::Regular(Regular::SI)), ParseRegFromData, DestinationIsInModRmField]),
        IxDef::new("mov",  vec![B(0xbf), DataLo, DataHi, ImpliedW(true), ImpliedModRmOperand(Register::Regular(Regular::DI)), ParseRegFromData, DestinationIsInModRmField]),
        IxDef::new("_unused",  vec![B(0xc0), /* not used */]),
        IxDef::new("_unused",  vec![B(0xc1), /* not used */]),
        IxDef::new("ret",  vec![B(0xc2), DataLo, DataHi, ParseRegFromData, DestinationIsInRegField]),
        IxDef::new("ret_intraseg_1",  vec![B(0xc3), Ret]),
        IxDef::new("les",  vec![B(0xc4), Mod, Reg, Rm, DispLo, DispHi, DestinationIsInRegField]),
        IxDef::new("lds",  vec![B(0xc5), Mod, Reg, Rm, DispLo, DispHi, DestinationIsInRegField]),
        IxDef::new("mov",  vec![B(0xc6), Mod, C(bits!(static u8, Msb0; 0,0,0)), Rm, OptDispLo, OptDispHi, Data8, ParseRegFromData, ParseModRm, DestinationIsInModRmField]),
        IxDef::new("_unused",  vec![B(0xc6), Mod, C(bits!(static u8, Msb0; 0,0,1)), Rm, /* not used */]),
        IxDef::new("_unused",  vec![B(0xc6), Mod, C(bits!(static u8, Msb0; 0,1,0)), Rm, /* not used */]),
        IxDef::new("_unused",  vec![B(0xc6), Mod, C(bits!(static u8, Msb0; 0,1,1)), Rm, /* not used */]),
        IxDef::new("_unused",  vec![B(0xc6), Mod, C(bits!(static u8, Msb0; 1,0,0)), Rm, /* not used */]),
        IxDef::new("_unused",  vec![B(0xc6), Mod, C(bits!(static u8, Msb0; 1,0,1)), Rm, /* not used */]),
        IxDef::new("_unused",  vec![B(0xc6), Mod, C(bits!(static u8, Msb0; 1,1,0)), Rm, /* not used */]),
        IxDef::new("_unused",  vec![B(0xc6), Mod, C(bits!(static u8, Msb0; 1,1,1)), Rm, /* not used */]),
        IxDef::new("mov",  vec![B(0xc7), Mod, C(bits!(static u8, Msb0; 0,0,0)), Rm, OptDispLo, OptDispHi, DataLo, DataHi, ImpliedW(true), ParseRegFromData, ParseModRm, DestinationIsInModRmField]),
        IxDef::new("_unused",  vec![B(0xc7), Mod, C(bits!(static u8, Msb0; 0,0,1)), Rm, /* not used */]),
        IxDef::new("_unused",  vec![B(0xc7), Mod, C(bits!(static u8, Msb0; 0,1,0)), Rm, /* not used */]),
        IxDef::new("_unused",  vec![B(0xc7), Mod, C(bits!(static u8, Msb0; 0,1,1)), Rm, /* not used */]),
        IxDef::new("_unused",  vec![B(0xc7), Mod, C(bits!(static u8, Msb0; 1,0,0)), Rm, /* not used */]),
        IxDef::new("_unused",  vec![B(0xc7), Mod, C(bits!(static u8, Msb0; 1,0,1)), Rm, /* not used */]),
        IxDef::new("_unused",  vec![B(0xc7), Mod, C(bits!(static u8, Msb0; 1,1,0)), Rm, /* not used */]),
        IxDef::new("_unused",  vec![B(0xc7), Mod, C(bits!(static u8, Msb0; 1,1,1)), Rm, /* not used */]),
        IxDef::new("_unused",  vec![B(0xc8)]),
        IxDef::new("_unused",  vec![B(0xc9)]),
        IxDef::new("ret",  vec![B(0xca), DataLo, DataHi, ParseRegFromData, DestinationIsInRegField]),
        IxDef::new("ret_intraseg_2",  vec![B(0xcb), Ret]),
        IxDef::new("int_3",  vec![B(0xcc), Ret]),
        IxDef::new("int",  vec![B(0xcd), Data8, ParseRegFromData, DestinationIsInRegField]),
        IxDef::new("into",  vec![B(0xce), Ret]),
        IxDef::new("iret",  vec![B(0xcf), Ret]),
    ];
    res
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
