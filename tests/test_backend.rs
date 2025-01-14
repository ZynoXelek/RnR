use mips::rf::Reg::*;
use rnr::ast::{Block, Expr, Prog};

use rnr::common::{parse_mips, parse_mips_no_run};

#[cfg(test)]
mod test_bvm {
    use super::*;

    //? Literals --------------------------------------------

    #[cfg(test)]
    mod test_backend_literals {
        use super::*;

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
    }

    //? Unary Operations -----------------------------------


    #[cfg(test)]
    mod test_backend_unops {
        use super::*;

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
    }

    //? Binary Operations ----------------------------------

    #[cfg(test)]
    mod test_backend_binops {
        use super::*;

        #[test]
        fn test_simple_addition() {
            // Get the resulting mips
            let mips = parse_mips::<Expr>("4 + 5").unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 9);
        }

        #[test]
        fn test_simple_subtraction() {
            // Get the resulting mips
            let mips = parse_mips::<Expr>("5 - 2").unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 3);
        }

        #[test]
        fn test_simple_subtraction_negative_result() {
            // Get the resulting mips
            let mips = parse_mips::<Expr>("3 - 7").unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), (-4 as i32) as u32);
        }

        #[test]
        fn test_simple_multiplication_1() {
            // Get the resulting mips
            let mips = parse_mips::<Expr>("3 * 5").unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 15);
        }

        #[test]
        fn test_simple_multiplication_2() {
            // Get the resulting mips
            let mips = parse_mips::<Expr>("3 * -5").unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), (-15 as i32) as u32);
        }

        #[test]
        fn test_simple_multiplication_3() {
            // Get the resulting mips
            let mips = parse_mips::<Expr>("-3 * 5").unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), (-15 as i32) as u32);
        }

        #[test]
        fn test_simple_multiplication_4() {
            // Get the resulting mips
            let mips = parse_mips::<Expr>("-3 * -5").unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 15);
        }

        #[test]
        fn test_simple_multiplication_5() {
            // Get the resulting mips
            let mips = parse_mips::<Expr>("0 * 5").unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 0);
        }

        #[test]
        fn test_simple_multiplication_6() {
            // Get the resulting mips
            let mips = parse_mips::<Expr>("3 * 0").unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 0);
        }

        #[test]
        fn test_simple_division_1() {
            // Get the resulting mips
            let mips = parse_mips::<Expr>("16 / 5").unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 3);
        }

        #[test]
        fn test_simple_division_2() {
            // Get the resulting mips
            let mips = parse_mips::<Expr>("-16 / 5").unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), (-3 as i32) as u32);
        }

        #[test]
        fn test_simple_division_3() {
            // Get the resulting mips
            let mips = parse_mips::<Expr>("16 / -5").unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), (-3 as i32) as u32);
        }

        #[test]
        fn test_simple_division_4() {
            // Get the resulting mips
            let mips = parse_mips::<Expr>("-16 / -5").unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 3);
        }

        #[test]
        fn test_simple_division_5() {
            // Get the resulting mips
            let mips = parse_mips::<Expr>("14 / 14").unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 1);
        }

        #[test]
        fn test_simple_division_6() {
            // Get the resulting mips
            let mips = parse_mips::<Expr>("14 / -14").unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), (-1 as i32) as u32);
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

        #[test]
        fn test_lt_negative_value_1() {
            // Get the resulting mips
            let mips = parse_mips::<Expr>("-3 < -2").unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 1);
        }

        #[test]
        fn test_lt_negative_value_2() {
            // Get the resulting mips
            let mips = parse_mips::<Expr>("-1 < -2").unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 0);
        }

        #[test]
        fn test_lt_negative_value_3() {
            // Get the resulting mips
            let mips = parse_mips::<Expr>("-1 < 1").unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 1);
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
    }

    //? Statements -----------------------------------------
    // as simple blocks

    //? Lets

    #[cfg(test)]
    mod test_backend_lets {
        use super::*;

        #[test]
        fn test_simple_let() {
            // Get the resulting mips
            let mips = parse_mips::<Block>(
                "
            {
                let a: i32 = 2;
            }
            ",
            )
            .unwrap();

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
                let b: i32 = a + 3;
                b
            }
            ",
            )
            .unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 5); // Should also be put on top of the stack
        }

        #[test]
        fn test_simple_let_with_block_1() {
            // Get the resulting mips
            let mips = parse_mips::<Block>(
                "
            {
                let a: i32 = {
                    let b: i32 = 2;
                    b + 1
                };
            }
            ",
            )
            .unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 3);
        }

        #[test]
        fn test_simple_let_with_block_2() {
            // Get the resulting mips
            let mips = parse_mips::<Block>(
                "
            {
                let a: bool = {
                    let b: bool = true;
                    b && false
                };
            }
            ",
            )
            .unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 0);
        }

        #[test]
        fn test_simple_let_with_block_and_shadowing() {
            // Get the resulting mips
            let mips = parse_mips::<Block>(
                "
            {
                let a: i32 = 2;
                let b: i32 = {
                    let b: i32 = a; // This should be 2
                    let a: i32 = 3;
                    a + b // This is 5
                };

                a + b // This is 7
            }
            ",
            )
            .unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 7);
        }
    }

    //? Ifs

    #[cfg(test)]
    mod test_backend_ifs {
        use super::*;

        #[test]
        fn test_simple_if_then_else_1() {
            // Get the resulting mips
            let mips = parse_mips::<Block>(
                "
            {
                if true {
                    4
                } else {
                    5
                }
            }
            ",
            )
            .unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 4);
        }

        #[test]
        fn test_simple_if_then_else_2() {
            // Get the resulting mips
            let mips = parse_mips::<Block>(
                "
            {
                if false {
                    4
                } else {
                    5
                }
            }
            ",
            )
            .unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 5);
        }

        #[test]
        fn test_simple_if_then_else_3() {
            // Get the resulting mips
            let mips = parse_mips::<Block>(
                "
            {
                if false {
                    4
                } else if false {
                    5
                } else {
                    6
                }
            }
            ",
            )
            .unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 6);
        }
    }

    //? Assignments

    #[cfg(test)]
    mod test_backend_assignments {
        use super::*;

        #[test]
        fn test_assign() {
            // Get the resulting mips
            let mips = parse_mips::<Block>(
                "
            {
                let mut a: i32 = 2;
                a = 3;
                a
            }
            ",
            )
            .unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 3);
        }

        #[test]
        fn test_late_assign() {
            // Get the resulting mips
            let mips = parse_mips::<Block>(
                "
            {
                let a: i32;
                a = 3;
                a
            }
            ",
            )
            .unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 3);
        }

        #[test]
        fn test_assign_in_block() {
            // Get the resulting mips
            let mips = parse_mips::<Block>(
                "
            {
                let mut a: i32 = 1;
                {
                    a = 3;
                }
                a
            }
            ",
            )
            .unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 3);
        }

        #[test]
        fn test_simple_if_then_else_assign_1() {
            // Get the resulting mips
            let mips = parse_mips::<Block>(
                "
            {
                let mut a: i32 = 1;
                if true {
                    a = 4;
                } else {
                    a = 6;
                }
                a
            }
            ",
            )
            .unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 4);
        }

        #[test]
        fn test_simple_if_then_else_assign_2() {
            // Get the resulting mips
            let mips = parse_mips::<Block>(
                "
            {
                let mut a: i32 = 1;
                a = 2;
                if false {
                    a = 4;
                } else {
                    a = 6;
                }
                a
            }
            ",
            )
            .unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 6);
        }

        #[test]
        fn test_simple_if_then_else_assign_3() {
            // Get the resulting mips
            let mips = parse_mips::<Block>(
                "
            {
                let mut a: i32 = 1;
                if true {
                    a = 4;
                    let b: i32 = a + 2; // 6
                    b + 3 // 9
                } else {
                    a = 6;
                    let b: i32 = a + 2; // 8
                    b + 3 // 12
                }
            }
            ",
            )
            .unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 9);
        }

        #[test]
        fn test_simple_if_then_else_assign_4() {
            // Get the resulting mips
            let mips = parse_mips::<Block>(
                "
            {
                let mut a: i32 = 1;
                if false {
                    a = 4;
                    let b: i32 = a + 2; // 6
                    b + 3 // 9
                } else {
                    a = 6;
                    let b: i32 = a + 2; // 8
                    b + 3 // 11
                }
            }
            ",
            )
            .unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 11);
        }

        #[test]
        fn test_assign_on_if() {
            // Get the resulting mips
            let mips = parse_mips::<Block>(
                "
            {
                let a: i32 = if false {
                    4
                } else {
                    5
                };
                let b: i32 = 2;
                a + b
            }
            ",
            )
            .unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 7);
        }
    }

    //? Whiles

    #[cfg(test)]
    mod test_backend_whiles {
        use super::*;

        #[test]
        fn test_while_false() {
            // Get the resulting mips
            let mut mips = parse_mips_no_run::<Block>(
                "
            {
                let mut a: i32 = 0;
                while false {
                    a = a + 1;
                }
                a
            }
            ",
            )
            .unwrap();

            _ = mips.run();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 0);
        }

        #[test]
        fn test_while_with_var_1() {
            // Get the resulting mips
            let mut mips = parse_mips_no_run::<Block>(
                "
            {
                let mut a: i32 = 0;
                while a < 4 {
                    a = a + 1;
                }
                a
            }
            ",
            )
            .unwrap();

            _ = mips.run();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 4);
        }

        #[test]
        fn test_while_with_var_2() {
            // Get the resulting mips
            let mut mips = parse_mips_no_run::<Block>(
                "
            {
                let mut a: i32 = 0;
                while a > 0 {
                    a = a - 1;
                }
                a
            }
            ",
            )
            .unwrap();

            _ = mips.run();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 0);
        }

        #[test]
        fn test_while_with_var_3() {
            // Get the resulting mips
            let mut mips = parse_mips_no_run::<Block>(
                "
            {
                let mut a: i32 = -5;
                while a >= -10 {
                    a = a - 1;
                }
                a
            }
            ",
            )
            .unwrap();

            _ = mips.run();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), (-11 as i32) as u32);
        }

        #[test]
        fn test_scope_issues() {
            // Get the resulting mips
            let mips = parse_mips::<Block>(
                "
            {
                let a: i32 = 1;
                2;          // Do nothing (should be popped of the stack)
                let b: i32 = 4;
                let c: i32 = {
                    let c: i32 = 5;
                    a + b + c; // Do nothing (should be popped of the stack)
                    let d: i32 = {
                        let a: i32 = 3;
                        b + a + c // 12
                    };
                    d + c // 17
                };

                c
            }
            ",
            )
            .unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 17);
        }
    }

    //? Functions

    #[cfg(test)]
    mod test_backend_functions {
        use super::*;

        #[test]
        fn test_simple_func_1() {
            // Get the resulting mips
            let mut mips = parse_mips_no_run::<Block>(
                "
            {
                fn foo() -> i32 {
                    1
                }

                let a: i32 = foo(); // 1
                a + 3 // 4
            }
            ",
            )
            .unwrap();

            _ = mips.run();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 4);
        }

        #[test]
        fn test_simple_func_2() {
            // Get the resulting mips
            let mut mips = parse_mips_no_run::<Block>(
                "
            {
                let a: i32 = 5;

                fn foo() -> i32 {
                    1
                }
                fn bar() -> i32 {
                    3
                }

                a + foo()
            }
            ",
            )
            .unwrap();

            _ = mips.run();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 6);
        }

        #[test]
        fn test_simple_func_2_bis() {
            // Get the resulting mips
            let mut mips = parse_mips_no_run::<Block>(
                "
            {
                let a: i32 = 5;

                fn foo() -> i32 {
                    1
                }
                fn bar() -> i32 {
                    3
                }

                a + bar()
            }
            ",
            )
            .unwrap();

            _ = mips.run();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 8);
        }

        #[test]
        fn test_simple_func_with_args() {
            // Get the resulting mips
            let mut mips = parse_mips_no_run::<Block>(
                "
            {
                fn add(a: i32, b: i32) -> i32 {
                    a + b
                }

                add(2, 3)
            }
            ",
            )
            .unwrap();

            _ = mips.run();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 5);
        }

        #[test]
        fn test_simple_func_with_more_args() {
            // Get the resulting mips
            let mut mips = parse_mips_no_run::<Block>(
                "
            {
                fn f(a: i32, b: i32, c: i32, d: i32, e: i32) -> i32 {
                    a * (b + e - c) / d
                }

                f(2, 3, 4, 5, 6) // = 2 * (3 + 6 - 4) / 5 = 2 * 5 / 5 = 2
            }
            ",
            )
            .unwrap();

            _ = mips.run();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 2);
        }

        #[test]
        fn test_simple_func_with_args_and_expr() {
            // Get the resulting mips
            let mut mips = parse_mips_no_run::<Block>(
                "
            {
                fn add(a: i32, b: i32) -> i32 {
                    a + b
                }

                let a: i32 = 2;
                add(a, 2 + 1)
            }
            ",
            )
            .unwrap();

            _ = mips.run();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 5);
        }

        #[test]
        fn test_simple_func_called_before() {
            // Get the resulting mips
            let mut mips = parse_mips_no_run::<Block>(
                "
            {
                let a: i32 = add(2, 3);

                fn add(a: i32, b: i32) -> i32 {
                    a + b
                }

                a
            }
            ",
            )
            .unwrap();

            _ = mips.run();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 5);
        }

        #[test]
        fn test_simple_func_calling_another_one_defined_later() {
            // Get the resulting mips
            let mut mips = parse_mips_no_run::<Block>(
                "
            {
                fn foo() -> i32 {
                    add(2, 3)
                }

                fn add(a: i32, b: i32) -> i32 {
                    a + b
                }

                foo()
            }
            ",
            )
            .unwrap();

            _ = mips.run();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 5);
        }

        #[test]
        fn test_more_complex_func() {
            // Get the resulting mips
            let mut mips = parse_mips_no_run::<Block>(
                "
            {
                fn test(n: i32) -> i32 {
                    if n == 0 {
                        1
                    } else {
                        2 * n + 3
                    }
                }

                let a: i32 = test(0); // 1
                let b: i32 = test(5); // 13
                a + b // 14
            }
            ",
            )
            .unwrap();

            _ = mips.run();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 14);
        }

        #[test]
        fn test_recursion() {
            // Get the resulting mips
            let mut mips = parse_mips_no_run::<Block>(
                "
            {
                fn fact(n: i32) -> i32 {
                    if n == 0 {
                        1
                    } else {
                        n * fact(n - 1)
                    }
                }

                fact(5)
            }
            ",
            )
            .unwrap();

            _ = mips.run();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 120);
        }
    }

    //? Programs

    #[cfg(test)]
    mod test_backend_programs {
        use super::*;

        #[test]
        fn test_simple_prog_1() {
            // Get the resulting mips
            let mut mips = parse_mips_no_run::<Prog>(
                "
            fn add(a: i32, b: i32) -> i32 {
                a + b
            }

            fn main() {
                let a: i32 = add(2, 3); // 5
                let b: i32 = add(4, 5); // 9
                let c: i32 = a + b; // 14
                c;
                println!(\"{}\", c); // Should be supported, simply ignored
            }
            ",
            )
            .unwrap();

            _ = mips.run();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 14); // C is in t0 at the end, even though the function does not return anything
        }

        #[test]
        fn test_simple_prog_2() {
            // Get the resulting mips
            let mut mips = parse_mips_no_run::<Prog>(
                "
            fn add(a: i32, b: i32) -> i32 {
                a + b
            }
            
            fn main() {
                let a: i32 = 1;
                let b: i32 = 2;
                let c: i32 = add(a, b);
            }
            ",
            )
            .unwrap();

            _ = mips.run();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 3); // _c is in t0 at the end, even though the function does not return anything
        }

        #[test]
        fn test_simple_prog_with_recursion() {
            // Get the resulting mips
            let mut mips = parse_mips_no_run::<Prog>(
                "
            fn add(a: i32, b: i32) -> i32 {
                a + b
            }

            fn fact(n: i32) -> i32 {
                if n == 0 {
                    1
                } else {
                    n * fact(n - 1)
                }
            }

            fn main() {
                let a: i32 = add(2, 3); // 5
                let b: i32 = fact(5); // 120
                let c: i32 = b / a; // 24
                c;
            }
            ",
            )
            .unwrap();

            _ = mips.run();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 24); // C is in t0 at the end, even though the function does not return anything
        }

        #[test]
        fn test_simple_prog_with_mutual_recursion() {
            // Get the resulting mips
            let mut mips = parse_mips_no_run::<Prog>(
                "
            fn is_even(n: i32) -> bool {
                if n == 0 {
                    true
                } else {
                    is_odd(n - 1)
                }
            }

            fn is_odd(n: i32) -> i32 {
                if n == 0 {
                    false
                } else {
                    is_even(n - 1)
                }
            }

            fn main() {
                let a: bool = is_even(5); // false
                let b: bool = is_odd(5); // true
                a || b // true
            }
            ",
            )
            .unwrap();

            _ = mips.run();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 1);
        }
    }

    //? Additional: Arrays

    #[cfg(test)]
    mod test_backend_array {
        use super::*;
        
        #[test]
        #[ignore = "Arrays not yet implemented"]
        fn test_simple_array_literal_1() {
            // Get the resulting mips
            let mips = parse_mips::<Expr>("[1, 2, 3]").unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 3);
        }

        #[test]
        #[ignore = "Arrays not yet implemented"]
        fn test_simple_array_literal_2() {
            // Get the resulting mips
            let mips = parse_mips::<Expr>("[1, 2]").unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 2);
        }

        #[test]
        #[ignore = "Arrays not yet implemented"]
        fn test_simple_get_1() {
            // Get the resulting mips
            let mips = parse_mips::<Expr>("[1, 2, 3][0]").unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 1);
        }

        #[test]
        #[ignore = "Arrays not yet implemented"]
        fn test_simple_get_2() {
            // Get the resulting mips
            let mips = parse_mips::<Expr>("[1, 2, 3][1]").unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 2);
        }

        #[test]
        #[ignore = "Arrays not yet implemented"]
        fn test_simple_get_3() {
            // Get the resulting mips
            let mips = parse_mips::<Expr>("[1, 2, 3][2]").unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 3);
        }

        #[test]
        #[ignore = "Arrays not yet implemented"]
        fn test_complex_get_1() {
            // Get the resulting mips
            let mips = parse_mips::<Expr>("[[-1, 4], [2, 3]][0][1]").unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 4);
        }

        #[test]
        #[ignore = "Arrays not yet implemented"]
        fn test_complex_get_2() {
            // Get the resulting mips
            let mips = parse_mips::<Expr>("[[[true, true], [false, true]], [[false, false], [true, false]]][0][1][0]").unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 0);
        }

        #[test]
        #[ignore = "Arrays not yet implemented"]
        fn test_let_array() {
            // Get the resulting mips
            let mips = parse_mips::<Block>(
                "
            {
                let a: [i32; 3] = [1, 2, 3];
                a[1]
            }
            ",
            )
            .unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 2);
        }

        #[test]
        #[ignore = "Arrays not yet implemented"]
        fn test_mut_let_array() {
            // Get the resulting mips
            let mips = parse_mips::<Block>(
                "
            {
                let mut a: [i32; 3] = [1, 2, 3];
                a[1] = 5;
                a[1] + a[0] // 6
            }
            ",
            )
            .unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 6);
        }

        #[test]
        #[ignore = "Arrays not yet implemented"]
        fn test_array_with_func() {
            // Get the resulting mips
            let mips = parse_mips::<Block>(
                "
            {
                fn new_arr() -> [[bool; 2]; 2] {
                    [[true, false], [false, true]]
                }

                fn bang_arr(a: [[bool; 2]; 2]) -> [[bool; 2]; 2] {
                    [[!a[0][0], !a[0][1]], [!a[1][0], !a[1][1]]]
                }
                
                let mut a: [[bool; 2]; 2] = new_arr(); // [[true, false], [false, true]]
                a[0] = [false, false]; // [[false, false], [false, true]]
                a = bang_arr(a); // [[true, true], [true, false]]
                a[0][0] // true
            }
            ",
            )
            .unwrap();

            // Check the result of the mips
            assert_eq!(mips.rf.get(t0), 1);
        }
    }
}
