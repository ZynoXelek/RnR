use rnr::asm_parse::*;

use mips::{
    asm::*,
    rf::Reg::*,
};

#[cfg(test)]
pub mod test_asm_parse {
    use super::*;

    //? Showing some examples of instructions generated

    // #[test]
    fn _display_examples() { // Uncomment the test above to be able to run it easily and see the output
        let instr = add(t0, t1, t2);
        println!("add instr");
        println!("{}\n", instr);

        let instr = sub(t0, t1, t2);
        println!("sub instr");
        println!("{}\n", instr);

        let instr = addu(t0, t1, t2);
        println!("addu instr");
        println!("{}\n", instr);

        let instr = subu(t0, t1, t2);
        println!("subu instr");
        println!("{}\n", instr);

        let instr = addi(t0, t1, -4);
        println!("addi instr");
        println!("{}\n", instr);

        let instr = addiu(t0, t1, -4);
        println!("addiu instr");
        println!("{}\n", instr);

        let instr = lui(t0, 4 as u16);
        println!("lui instr");
        println!("{}\n", instr);

        let instr = and(t0, t1, t2);
        println!("and instr");
        println!("{}\n", instr);

        let instr = or(t0, t1, t2);
        println!("or instr");
        println!("{}\n", instr);

        let instr = xor(t0, t1, t2);
        println!("xor instr");
        println!("{}\n", instr);

        let instr = andi(t0, t1, 4);
        println!("andi instr");
        println!("{}\n", instr);

        let instr = ori(t0, t1, 4);
        println!("ori instr");
        println!("{}\n", instr);

        let instr = xori(t0, t1, 4);
        println!("xori instr");
        println!("{}\n", instr);

        let instr = lw(t0, 4, t1);
        println!("lw instr");
        println!("{}\n", instr);

        let instr = sw(t0, 4, t1);
        println!("sw instr");
        println!("{}\n", instr);

        let inst = beq(t0, t1, 4);
        println!("beq instr");
        println!("{}\n", inst);

        let instr = bne(t0, t1, 4);
        println!("bne instr");
        println!("{}\n", instr);

        let instr = blez(t0, 4);
        println!("blez instr");
        println!("{}\n", instr);

        let instr = jr(t0);
        println!("jr instr");
        println!("{}\n", instr);

        let instr = slt(t0, t1, t2);
        println!("slt instr");
        println!("{}\n", instr);

        let instr = sltu(t0, t1, t2);
        println!("sltu instr");
        println!("{}\n", instr);

        let instr = slti(t0, t1, 4);
        println!("slti instr");
        println!("{}\n", instr);

        let instr = sltiu(t0, t1, 4);
        println!("sltiu instr");
        println!("{}\n", instr);

        let instr = halt();
        println!("halt instr");
        println!("{}\n", instr);
    }

    //? Test for each of the examples
    #[test]
    fn test_instr_add() {
        let instr = add(t0, t1, t2);
        let instr_str = format!("{}", instr);
        let parsed_instr = parse_instr(&instr_str).unwrap();
        assert_eq!(instr, parsed_instr);
    }

    #[test]
    fn test_instr_sub() {
        let instr = sub(t0, t1, t2);
        let instr_str = format!("{}", instr);
        let parsed_instr = parse_instr(&instr_str).unwrap();
        assert_eq!(instr, parsed_instr);
    }

    #[test]
    fn test_instr_addu() {
        let instr = addu(t0, t1, t2);
        let instr_str = format!("{}", instr);
        let parsed_instr = parse_instr(&instr_str).unwrap();
        assert_eq!(instr, parsed_instr);
    }

    #[test]
    fn test_instr_subu() {
        let instr = subu(t0, t1, t2);
        let instr_str = format!("{}", instr);
        let parsed_instr = parse_instr(&instr_str).unwrap();
        assert_eq!(instr, parsed_instr);
    }

    #[test]
    fn test_instr_addi() {
        let instr = addi(t0, t1, -4);
        let instr_str = format!("{}", instr);
        let parsed_instr = parse_instr(&instr_str).unwrap();
        assert_eq!(instr, parsed_instr);
    }

    #[test]
    fn test_instr_addiu() {
        let instr = addiu(t0, t1, -4);
        let instr_str = format!("{}", instr);
        let parsed_instr = parse_instr(&instr_str).unwrap();
        assert_eq!(instr, parsed_instr);
    }

    #[test]
    fn test_instr_lui() {
        let instr = lui(t0, 4 as u16);
        let instr_str = format!("{}", instr);
        let parsed_instr = parse_instr(&instr_str).unwrap();
        assert_eq!(instr, parsed_instr);
    }

    #[test]
    fn test_instr_and() {
        let instr = and(t0, t1, t2);
        let instr_str = format!("{}", instr);
        let parsed_instr = parse_instr(&instr_str).unwrap();
        assert_eq!(instr, parsed_instr);
    }

    #[test]
    fn test_instr_or() {
        let instr = or(t0, t1, t2);
        let instr_str = format!("{}", instr);
        let parsed_instr = parse_instr(&instr_str).unwrap();
        assert_eq!(instr, parsed_instr);
    }

    #[test]
    fn test_instr_xor() {
        let instr = xor(t0, t1, t2);
        let instr_str = format!("{}", instr);
        let parsed_instr = parse_instr(&instr_str).unwrap();
        assert_eq!(instr, parsed_instr);
    }

    #[test]
    fn test_instr_andi() {
        let instr = andi(t0, t1, 4);
        let instr_str = format!("{}", instr);
        let parsed_instr = parse_instr(&instr_str).unwrap();
        assert_eq!(instr, parsed_instr);
    }

    #[test]
    fn test_instr_ori() {
        let instr = ori(t0, t1, 4);
        let instr_str = format!("{}", instr);
        let parsed_instr = parse_instr(&instr_str).unwrap();
        assert_eq!(instr, parsed_instr);
    }

    #[test]
    fn test_instr_xori() {
        let instr = xori(t0, t1, 4);
        let instr_str = format!("{}", instr);
        let parsed_instr = parse_instr(&instr_str).unwrap();
        assert_eq!(instr, parsed_instr);
    }

    #[test]
    fn test_instr_lw() {
        let instr = lw(t0, 4, t1);
        let instr_str = format!("{}", instr);
        let parsed_instr = parse_instr(&instr_str).unwrap();
        assert_eq!(instr, parsed_instr);
    }

    #[test]
    fn test_instr_sw() {
        let instr = sw(t0, 4, t1);
        let instr_str = format!("{}", instr);
        let parsed_instr = parse_instr(&instr_str).unwrap();
        assert_eq!(instr, parsed_instr);
    }

    #[test]
    fn test_instr_beq() {
        let instr = beq(t0, t1, 4);
        let instr_str = format!("{}", instr);
        let parsed_instr = parse_instr(&instr_str).unwrap();
        assert_eq!(instr, parsed_instr);
    }

    #[test]
    fn test_instr_bne() {
        let instr = bne(t0, t1, 4);
        let instr_str = format!("{}", instr);
        let parsed_instr = parse_instr(&instr_str).unwrap();
        assert_eq!(instr, parsed_instr);
    }

    #[test]
    fn test_instr_blez() {
        let instr = blez(t0, 4);
        let instr_str = format!("{}", instr);
        let parsed_instr = parse_instr(&instr_str).unwrap();
        assert_eq!(instr, parsed_instr);
    }

    #[test]
    fn test_instr_jr() {
        let instr = jr(t0);
        let instr_str = format!("{}", instr);
        let parsed_instr = parse_instr(&instr_str).unwrap();
        assert_eq!(instr, parsed_instr);
    }

    #[test]
    fn test_instr_slt() {
        let instr = slt(t0, t1, t2);
        let instr_str = format!("{}", instr);
        let parsed_instr = parse_instr(&instr_str).unwrap();
        assert_eq!(instr, parsed_instr);
    }

    #[test]
    fn test_instr_sltu() {
        let instr = sltu(t0, t1, t2);
        let instr_str = format!("{}", instr);
        let parsed_instr = parse_instr(&instr_str).unwrap();
        assert_eq!(instr, parsed_instr);
    }

    #[test]
    fn test_instr_slti() {
        let instr = slti(t0, t1, 4);
        let instr_str = format!("{}", instr);
        let parsed_instr = parse_instr(&instr_str).unwrap();
        assert_eq!(instr, parsed_instr);
    }

    #[test]
    fn test_instr_sltiu() {
        let instr = sltiu(t0, t1, 4);
        let instr_str = format!("{}", instr);
        let parsed_instr = parse_instr(&instr_str).unwrap();
        assert_eq!(instr, parsed_instr);
    }

    #[test]
    fn test_instr_halt() {
        let instr = halt();
        let instr_str = format!("{}", instr);
        let parsed_instr = parse_instr(&instr_str).unwrap();
        assert_eq!(instr, parsed_instr);
    }

    //? Test for a sequence of instructions

    #[test]
    fn test_instrs_1() {
        let instrs = vec![
            add(t0, t1, t2),
            sub(t0, t1, t2),
            addu(t0, t1, t2),
            subu(t0, t1, t2),
            addi(t0, t1, -4),
            addiu(t0, t1, -4),
            lui(t0, 4 as u16),
            and(t0, t1, t2),
            or(t0, t1, t2),
            xor(t0, t1, t2),
            andi(t0, t1, 4),
            ori(t0, t1, 4),
            xori(t0, t1, 4),
            lw(t0, 4, t1),
            sw(t0, 4, t1),
            beq(t0, t1, 4),
            bne(t0, t1, 4),
            blez(t0, 4),
            jr(t0),
            slt(t0, t1, t2),
            sltu(t0, t1, t2),
            slti(t0, t1, 4),
            sltiu(t0, t1, 4),
            halt(),
        ];

        let instrs_str = format!("{}", instrs.iter().map(|instr| format!("{}", instr)).collect::<Vec<String>>().join("\n"));
        let parsed_instrs = parse_instrs(&instrs_str).unwrap();
        
        // Comparing the sizes of the vectors
        assert_eq!(instrs.len(), parsed_instrs.len());
        // And then each instruction one by one
        for (instr, parsed_instr) in instrs.iter().zip(parsed_instrs.iter()) {
            assert_eq!(instr, parsed_instr);
        }
    }
}
