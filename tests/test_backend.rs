use rnr::ast::{Block, Expr, Prog, Type};
use mips::rf::Reg::*;

use rnr::common::parse_mips;

#[cfg(test)]
mod test_bvm {
    use super::*;

    //? Literals --------------------------------------------

    #[test]
    fn test_simple_literal_i32() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("2").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 2);
    }

    #[test]
    fn test_simple_literal_bool() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("true").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 1);
    }

    //? Unary Operations -----------------------------------

    #[test]
    fn test_negative() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("-2").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), (-2 as i32) as u32);
    }

    #[test]
    fn test_bang() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("!true").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 0);
    }

    //? Binary Operations ----------------------------------

    #[test]
    fn test_simple_addition() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("1 + 2").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 3);
    }

    #[test]
    fn test_simple_subtraction() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("5 - 2").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 3);
    }

    #[test]
    #[ignore="Not implemented"]
    fn test_simple_multiplication() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("3 * 2").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 6);
    }

    #[test]
    #[ignore="Not implemented"]
    fn test_simple_division() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("3 / 2").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 1);
    }

    //* ||
    #[test]
    fn test_simple_or_1() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("false || false").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 0);
    }

    #[test]
    fn test_simple_or_2() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("false || true").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 1);
    }

    #[test]
    fn test_simple_or_3() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("true || false").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 1);
    }

    #[test]
    fn test_simple_or_4() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("true || true").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 1);
    }

    //* &&
    #[test]
    fn test_simple_and_1() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("false && false").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 0);
    }

    #[test]
    fn test_simple_and_2() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("false && true").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 0);
    }

    #[test]
    fn test_simple_and_3() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("true && false").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 0);
    }

    #[test]
    fn test_simple_and_4() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("true && true").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 1);
    }

    //* ==
    #[test]
    fn test_eq_1() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("1 == 1").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 1);
    }

    #[test]
    fn test_eq_2() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("1 == 2").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 0);
    }

    //* !=
    #[test]
    fn test_neq_1() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("1 != 1").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 0);
    }

    #[test]
    fn test_neq_2() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("1 != 2").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 1);
    }

    //* <
    #[test]
    fn test_lt_1() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("1 < 2").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 1);
    }

    #[test]
    fn test_lt_2() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("2 < 2").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 0);
    }

    #[test]
    fn test_lt_3() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("3 < 2").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 0);
    }

    //* <=
    #[test]
    fn test_le_1() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("1 <= 2").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 1);
    }

    #[test]
    fn test_le_2() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("2 <= 2").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 1);
    }

    #[test]
    fn test_le_3() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("3 <= 2").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 0);
    }

    //* >
    #[test]
    fn test_gt_1() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("1 > 2").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 0);
    }

    #[test]
    fn test_gt_2() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("2 > 2").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 0);
    }

    #[test]
    fn test_gt_3() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("3 > 2").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 1);
    }

    //* >=
    #[test]
    fn test_ge_1() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("1 >= 2").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 0);
    }

    #[test]
    fn test_ge_2() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("2 >= 2").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 1);
    }

    #[test]
    fn test_ge_3() {
        // Get the resulting mips
        let mips = parse_mips::<Expr>("3 >= 2").unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 1);
    }

    //TODO: Add Get test for arrays

    //? Statements -----------------------------------------
    // as simple blocks

    #[test]
    fn test_simple_let() {
        // Get the resulting mips
        let mips = parse_mips::<Block>(
        "
        {
            let a: i32 = 2;
        }
        "
        ).unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 2);
    }

    #[test]
    fn test_simple_let_and_use_it() {
        // Get the resulting mips
        let mips = parse_mips::<Block>(
        "
        {
            let a: i32 = 2;
            let b = a + 3;
            b
        }
        "
        ).unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 5); // Should also be put on top of the stack
    }

    #[test]
    fn test_simple_let_with_block() {
        // Get the resulting mips
        let mips = parse_mips::<Block>(
        "
        {
            let a: i32 = {
                let b = 2;
                b + 1
            };
        }
        "
        ).unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 3);
    }

    #[test]
    fn test_simple_let_with_block_and_shadowing() {
        // Get the resulting mips
        let mips = parse_mips::<Block>(
        "
        {
            let a = 2;
            let b = {
                let b = a; // This should be 2
                let a = 3;
                a + b // This is 5
            };

            a + b // This is 7
        }
        "
        ).unwrap();

        // Check the result of the mips
        assert_eq!(mips.rf.get(t0), 7);
    }
}
