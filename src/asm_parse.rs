use mips::{
    asm::*,
    
    instr::{self, Instr, Op, OpR, OpI},
    instrs::Instrs,
    rf::Reg::{self, *},
    vm::Mips,
};

use crate::error::Error; //TODO: Use custom error type

//?#################################################################################################
//?#                                                                                               #
//?#                                     Parse Op and Reg                                          #
//?#                                                                                               #
//?#################################################################################################

pub fn parse_mips_opr_instr(op_str: &str, rd: Reg, rs: Reg, rt: Reg) -> Result<Instr, Error> { //TODO: Use custom error type
    // We simply need to read the string and try to match it to one of the defined OpR
    // OpR are the following:
    // Add,
    // Addu,
    // Sub,
    // Subu,
    // And,
    // Or,
    // Xor,
    // Slt,
    // Sltu,
    // Jr,
    // Then, we return the correct instruction using the helper method
    match op_str {
        "Add" => Ok(add(rd, rs, rt)),
        "Addu" => Ok(addu(rd, rs, rt)),
        "Sub" => Ok(sub(rd, rs, rt)),
        "Subu" => Ok(subu(rd, rs, rt)),
        "And" => Ok(and(rd, rs, rt)),
        "Or" => Ok(or(rd, rs, rt)),
        "Xor" => Ok(xor(rd, rs, rt)),
        "Slt" => Ok(slt(rd, rs, rt)),
        "Sltu" => Ok(sltu(rd, rs, rt)),
        "Jr" => Ok(jr(rs)),
        _ => Err(format!("Invalid input: could not match '{}' as an OpR", op_str)),
    }
}

pub fn parse_mips_opi_instr(op_str: &str, rt: Reg, rs: Reg, imm: i16) -> Result<Instr, Error> { //TODO: Use custom error type
    // We simply need to read the string and try to match it to one of the defined OpI
    // OpI are the following:
    // Addi,
    // Addiu,
    // Lui,
    // Andi,
    // Ori,
    // Xori,
    // Beq,
    // Bne,
    // Blez,
    // Lw,
    // Sw,
    // Slti,
    // Sltiu,
    // Then, we return the correct instruction using the helper method
    match op_str {
        "Addi" => Ok(addi(rt, rs, imm)),
        "Addiu" => Ok(addiu(rt, rs, imm)),
        "Lui" => Ok(lui(rt, imm as u16)),
        "Andi" => Ok(andi(rt, rs, imm as u16)),
        "Ori" => Ok(ori(rt, rs, imm as u16)),
        "Xori" => Ok(xori(rt, rs, imm as u16)),
        "Beq" => Ok(beq(rs, rt, imm)),
        "Bne" => Ok(bne(rs, rt, imm)),
        "Blez" => Ok(blez(rs, imm)),
        "Lw" => Ok(lw(rt, imm, rs)),
        "Sw" => Ok(sw(rt, imm, rs)),
        "Slti" => Ok(slti(rs, rt, imm)), // There is an inversion here for some reason
        "Sltiu" => Ok(sltiu(rs, rt, imm)), // There is an inversion here for some reason
        _ => Err(format!("Invalid input: could not match '{}' as an OpI", op_str)),
    }
}

fn parse_mips_reg(reg_str: &str) -> Result<Reg, Error> { //TODO: Use custom error type
    // We simply need to read the string and try to match it to one of the defined Reg
    // Reg are the following:
    // zero,
    // at,
    // v0,
    // v1,
    // a0,
    // a1,
    // a2,
    // a3,
    // t0,
    // t1,
    // t2,
    // t3,
    // t4,
    // t5,
    // t6,
    // t7,
    // s0,
    // s1,
    // s2,
    // s3,
    // s4,
    // s5,
    // s6,
    // s7,
    // t8,
    // t9,
    // k0,
    // k1,
    // gp,
    // sp,
    // fp,
    // ra,
    match reg_str {
        "zero" => Ok(zero),
        "at" => Ok(at),
        "v0" => Ok(v0),
        "v1" => Ok(v1),
        "a0" => Ok(a0),
        "a1" => Ok(a1),
        "a2" => Ok(a2),
        "a3" => Ok(a3),
        "t0" => Ok(t0),
        "t1" => Ok(t1),
        "t2" => Ok(t2),
        "t3" => Ok(t3),
        "t4" => Ok(t4),
        "t5" => Ok(t5),
        "t6" => Ok(t6),
        "t7" => Ok(t7),
        "s0" => Ok(s0),
        "s1" => Ok(s1),
        "s2" => Ok(s2),
        "s3" => Ok(s3),
        "s4" => Ok(s4),
        "s5" => Ok(s5),
        "s6" => Ok(s6),
        "s7" => Ok(s7),
        "t8" => Ok(t8),
        "t9" => Ok(t9),
        "k0" => Ok(k0),
        "k1" => Ok(k1),
        "gp" => Ok(gp),
        "sp" => Ok(sp),
        "fp" => Ok(fp),
        "ra" => Ok(ra),
        _ => Err(format!("Invalid input: could not match '{}' as a Reg", reg_str)),
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                        Parse Instr                                            #
//?#                                                                                               #
//?#################################################################################################

pub fn parse_instr(instr: &str) -> Result<Instr, Error> { //TODO: Use custom error type
    // An instr always follows one of the following syntaxes:
    // <InstrName>                              // e.g. "Halt"
    // <InstrName> <Reg>                        // e.g. "Jr ra"
    // <InstrName> <Reg>, <Reg>, <Reg>          // e.g. "Addu sp, sp, t0"
    // <InstrName> <Reg>, <Reg>, <Int>          // e.g. "Addiu sp, sp, -4"
    // <InstrName> <Reg>, <Int>[<Reg>]          // e.g. "Sw ra, 4[sp]"

    // We start by removing any ',', and replacing "[" and "]" by " " since we do not need them to identify our case
    let instr = instr.replace(",", "");
    let instr = instr.replace("[", " ");
    let instr = instr.replace("]", " ");

    //TODO: Make the regex be compiled only once and not at each call
    let op_code = r"[A-Z][a-z]+";
    let register_code = r"[a-z]+\d*"; // A register always has at least a letter
    let integer_code = r"-?\d+"; // Supports minus sign for numbers
    let register_integer_code = format!(r"({})|({})", register_code, integer_code);

    let single_code_part = format!(r"(?P<op>{})", op_code);
    let double_code_part = format!(r"(?P<reg1>{})", register_code);
    let quadruple_code_part = format!(r"(?P<reg2>{})\s*(?P<reg3>{})", register_integer_code, register_integer_code);

    let regex_code = format!(r"{}\s*{}?\s*({})?\s*", single_code_part, double_code_part, quadruple_code_part);
    let re = regex::Regex::new(&regex_code).unwrap();

    let capt = re.captures(&instr);

    //? Debug
    // eprintln!("Instr: {}", instr);
    // eprintln!("Regex: {}", regex_code);
    // eprintln!("Matches: {:?}", capt);

    // Processing the result
    match capt {
        Some(capt) => {
            let op_name = capt.name("op").unwrap().as_str();
            let reg1 = capt.name("reg1").map(|m| m.as_str());
            let reg2 = capt.name("reg2").map(|m| m.as_str());
            let reg3 = capt.name("reg3").map(|m| m.as_str());

            //? Debug
            // eprintln!("Op: {}", op_name);
            // eprintln!("Reg1: {:?}", reg1);
            // eprintln!("Reg2: {:?}", reg2);
            // eprintln!("Reg3: {:?}", reg3);

            // First check if the op is a 'Halt'
            if op_name == "Halt" {
                return Ok(halt());
            }

            // Then, we should have at least Reg1. It may be a jump instruction
            if let Some(reg1) = reg1 {
                // It can only be a register
                let reg1 = parse_mips_reg(reg1)?;

                if op_name == "Jr" {
                    return Ok(jr(reg1));
                }

                // Then, we should have Reg2 and Reg3
                if let (Some(reg2), Some(reg3)) = (reg2, reg3) {
                    // Let's first identify if it is an OpR or an OpI
                    let mut is_opi = false;
                    let int_re = regex::Regex::new(r"^\s*-?\d+\s*$").unwrap();
                    let op: Op;

                    // Usually, Reg3 would be the one with the integer value, but it can also be Reg2 due to the reversal of the order in the display
                    if int_re.is_match(reg3) || int_re.is_match(reg2) {
                        is_opi = true;
                    }
                    // If both are integers, we have an error
                    if int_re.is_match(reg3) && int_re.is_match(reg2) {
                        return Err("Invalid instruction. Both Reg2 and Reg3 are integers".to_string());
                    }

                    if is_opi {
                        if int_re.is_match(reg2) {
                            let reg3 = parse_mips_reg(reg3)?;
                            let imm = reg2.parse::<i16>().unwrap();
                            return parse_mips_opi_instr(op_name, reg1, reg3, imm);
                        } else {
                            let reg2 = parse_mips_reg(reg2)?;
                            let imm = reg3.parse::<i16>().unwrap();
                            return parse_mips_opi_instr(op_name, reg1, reg2, imm);
                        }
                    } else {
                        let reg2 = parse_mips_reg(reg2)?;
                        let reg3 = parse_mips_reg(reg3)?;
                        
                        parse_mips_opr_instr(op_name, reg1, reg2, reg3)
                    }
                } else {
                    return Err("Invalid instruction. Not an 'Halt' nor 'Jr' instruction but only one register was given".to_string());
                }
            } else {
                return Err("Invalid instruction. Not an 'Halt' instruction but no registers given".to_string());
            }
        }
        None => {
            return Err("Invalid instruction, does not respect any instr syntax".to_string());
        }
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                    Parse Instrs = ASM                                         #
//?#                                                                                               #
//?#################################################################################################

pub fn parse_instrs(instrs: &str) -> Result<Instrs, Error> {
    let instrs = instrs.split("\n").collect::<Vec<&str>>();
    let mut instrs_res = Instrs::new();

    for instr in instrs {
        let instr = instr.trim();
        if instr.len() > 0 {
            let instr = parse_instr(instr)?;
            instrs_res.push(instr);
        }
    }

    Ok(instrs_res)
}

pub fn parse_asm(asm: &str) -> Result<Mips, Error> {
    let instrs = parse_instrs(asm)?;
    Ok(Mips::new(instrs))
}
