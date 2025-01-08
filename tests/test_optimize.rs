use rnr::ast::{Block, Expr, Literal, Prog};
use rnr::test_util::assert_optimize;

#[cfg(test)]
pub mod test_optimize {
    use rnr::common::parse;

    use super::*;

    //? Testing things that can't be optimized

    #[test]
    fn test_optimize_literal_1() {
        let expr: Expr = parse("true");
        let expected = expr.clone();
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_optimize_literal_2() {
        let expr: Expr = parse("3");
        let expected = expr.clone();
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_optimize_identifier() {
        let expr: Expr = parse("my_var");
        let expected = expr.clone();
        assert_optimize(&expr, expected);
    }

    //? Testing simple unops

    #[test]
    fn test_bang_opti_1() {
        let expr: Expr = parse("!true");
        let expected: Expr = parse("false");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_bang_opti_2() {
        let expr: Expr = parse("!!true");
        let expected: Expr = parse("true");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_bang_opti_3() {
        let expr: Expr = parse("!!!false");
        let expected: Expr = parse("true");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_bang_opti_lit_1() {
        let expr: Expr = parse("!!a");
        let expected: Expr = parse("a");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_bang_opti_lit_2() {
        let expr: Expr = parse("!!!a");
        let expected: Expr = parse("!a");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_neg_opti_1() {
        let expr: Expr = parse("--3");
        let expected: Expr = parse("3");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_neg_opti_2() {
        let expr: Expr = parse("---3");
        // let expected: Expr = parse("-3"); // This is parsed as Neg(3) and not Lit(-3)
        let expected = Expr::Lit(Literal::Int(-3));
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_neg_opti_lit_1() {
        let expr: Expr = parse("--a");
        let expected: Expr = parse("a");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_neg_opti_lit_2() {
        let expr: Expr = parse("---a");
        let expected: Expr = parse("-a");
        assert_optimize(&expr, expected);
    }

    //? Testing simple binops

    #[test]
    fn test_add_opti_1() {
        let expr: Expr = parse("3 + 4");
        let expected: Expr = parse("7");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_add_opti_2() {
        let expr: Expr = parse("3 + 4 + 5");
        let expected: Expr = parse("12");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_add_opti_3() {
        let expr: Expr = parse("3 + 4 + 5 + 6");
        let expected: Expr = parse("18");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_add_opti_with_literals_1() {
        let expr: Expr = parse("3 + 5 + 6 + a");
        let expected: Expr = parse("14 + a");
        assert_optimize(&expr, expected);
    }

    //TODO: Add support for this kind of optimization
    #[test]
    #[ignore = "Not implemented yet (optimization of binops with literals)"]
    fn test_add_opti_with_literals_2() {
        let expr: Expr = parse("3 + a + 5 + 6");
        let expected: Expr = parse("14 + a");
        assert_optimize(&expr, expected);
    }

    // booleans

    #[test]
    fn test_and_opti_1() {
        let expr: Expr = parse("true && false");
        let expected: Expr = parse("false");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_and_opti_2() {
        let expr: Expr = parse("true && false && true");
        let expected: Expr = parse("false");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_and_short_circuit_1() {
        let expr: Expr = parse("false && a");
        let expected: Expr = parse("false");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_and_short_circuit_2() {
        let expr: Expr = parse("true && a");
        let expected: Expr = parse("a");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_or_opti_1() {
        let expr: Expr = parse("true || false");
        let expected: Expr = parse("true");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_or_short_circuit_1() {
        let expr: Expr = parse("true || a");
        let expected: Expr = parse("true");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_or_short_circuit_2() {
        let expr: Expr = parse("false || a");
        let expected: Expr = parse("a");
        assert_optimize(&expr, expected);
    }

    // comparison

    #[test]
    fn test_eq_opti_1() {
        let expr: Expr = parse("3 == 3");
        let expected: Expr = parse("true");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_eq_opti_2() {
        let expr: Expr = parse("3 == 4");
        let expected: Expr = parse("false");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_ne_opti_1() {
        let expr: Expr = parse("3 != 3");
        let expected: Expr = parse("false");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_ne_opti_2() {
        let expr: Expr = parse("3 != 4");
        let expected: Expr = parse("true");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_comp_opti_1() {
        let expr: Expr = parse("3 < 4");
        let expected: Expr = parse("true");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_comp_opti_2() {
        let expr: Expr = parse("3 + 2 < 4");
        let expected: Expr = parse("false");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_comp_opti_3() {
        let expr: Expr = parse("3 + 2 < a");
        let expected: Expr = parse("5 < a");
        assert_optimize(&expr, expected);
    }

    // arrays get

    #[test]
    fn test_array_get_opti_1() {
        let expr: Expr = parse("[1, 2, 3][1]");
        let expected: Expr = parse("2");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_array_get_opti_2() {
        let expr: Expr = parse("[1, 2, 3][0]");
        let expected: Expr = parse("1");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_array_get_opti_3() {
        let expr: Expr = parse("[1, 2, 3][2]");
        let expected: Expr = parse("3");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_array_get_opti_4() {
        let expr: Expr = parse("a[3]");
        let expected: Expr = parse("a[3]");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_array_get_with_expr_1() {
        let expr: Expr = parse("[1, 2, 3][1 + 1]");
        let expected: Expr = parse("3");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_array_get_with_expr_2() {
        let expr: Expr = parse("[1, 2, 3][1 + a]");
        let expected: Expr = parse("[1, 2, 3][1 + a]");
        assert_optimize(&expr, expected);
    }

    #[test]
    #[ignore = "Not implemented yet (can't parse expr in arrays)"]
    fn test_array_get_with_expr_3() {
        let expr: Expr = parse("[1, 2 * 5 + 7, 3][1]");
        let expected: Expr = parse("17");
        assert_optimize(&expr, expected);
    }

    #[test]
    #[ignore = "Not implemented yet (can't parse expr in arrays)"]
    fn test_array_get_with_expr_4() {
        let expr: Expr = parse("[a, b, c][0]");
        let expected: Expr = parse("a");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_array_of_arrays_get_1() {
        let expr: Expr = parse("[[1, 2], [3, 4]][1]");
        let expected: Expr = parse("[3, 4]");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_array_of_arrays_get_2() {
        let expr: Expr = parse("[[1, 2], [3, 4]][1][0]");
        let expected: Expr = parse("3");
        assert_optimize(&expr, expected);
    }

    //? Testing parenthesis

    #[test]
    fn test_paren_opti_1() {
        let expr: Expr = parse("(3)");
        let expected: Expr = parse("3");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_paren_opti_2() {
        let expr: Expr = parse("(3 + 4)");
        let expected: Expr = parse("7");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_paren_opti_3() {
        let expr: Expr = parse("(3 + 4) * 5");
        let expected: Expr = parse("35");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_paren_opti_4() {
        let expr: Expr = parse("(5 * (3 + 4))");
        let expected: Expr = parse("35");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_paren_opti_5() {
        let expr: Expr = parse("((5))");
        let expected: Expr = parse("5");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_paren_opti_6() {
        let expr: Expr = parse("((3 + 4) * a)");
        let expected: Expr = parse("7 * a");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_paren_opti_7() {
        let expr: Expr = parse("(a * (3 + 4))");
        let expected: Expr = parse("a * 7");
        assert_optimize(&expr, expected);
    }

    //? Function calls

    #[test]
    fn test_fn_call_opti_1() {
        let expr: Expr = parse("my_fn(3 + 7)");
        let expected: Expr = parse("my_fn(10)");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_fn_call_opti_2() {
        let expr: Expr = parse("my_fn(3 + 7, true || a)");
        let expected: Expr = parse("my_fn(10, true)");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_fn_call_opti_3() {
        let expr: Expr = parse("my_fn(g(true && b), false || a)");
        let expected: Expr = parse("my_fn(g(b), a)");
        assert_optimize(&expr, expected);
    }

    //? If then else

    //TODO: Combine with block optim to replace blocks by their return value
    //TODO: Blocks that do not return a value, and do not do any peculiar things can be removed
    #[test]
    fn test_if_then_else_opti_1() {
        let expr: Expr = parse("if true { 3 } else { 4 }");
        let expected: Expr = parse("3");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_if_then_else_opti_2() {
        let expr: Expr = parse("if false { 3 } else { 4 }");
        let expected: Expr = parse("4");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_if_then_else_opti_3() {
        let expr: Expr = parse("if false { 3 }");
        let expected: Expr = parse("()");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_if_then_else_opti_4() {
        let expr: Expr = parse("if true || false { 3 } else { 4 }");
        let expected: Expr = parse("3");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_if_then_else_opti_5() {
        let expr: Expr = parse("if true && false { 3 } else { 4 }");
        let expected: Expr = parse("4");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_if_then_else_opti_6() {
        let expr: Expr = parse("if false || a { 3 } else { 4 }");
        let expected: Expr = parse("if a { 3 } else { 4 }");
        assert_optimize(&expr, expected);
    }

    #[test]
    fn test_if_then_else_opti_7() {
        let expr: Expr = parse("if true && a { 3 } else { 4 }");
        let expected: Expr = parse("if a { 3 } else { 4 }");
        assert_optimize(&expr, expected);
    }

    //? Optimizing blocks

    #[test]
    fn test_block_opti() {
        let block: Block = parse(
            "
            {
                let a = 3;
                1 + 3; // Should be optimized out
                f(2); // Should be optimized out
                a
            }
            ",
        );
        let expected: Block = parse(
            "
            {
                let a = 3;
                a
            }
            ",
        );
        assert_optimize(&block, expected);
    }

    #[test]
    fn test_larger_block_opti() {
        let block: Block = parse(
            "
            {
                fn f(a: i32, b: i32) -> i32 {
                    a + b
                }

                fn g(a: bool) -> bool { // Since d is not used and optimized out, this should be optimized out
                    !a
                }

                let a = 3;
                let b = f(2 + a * a, 3);
                let c = b - 2 * a + f(a * b, -4); // Since d is not used and optimized out, this should be optimized out as well
                let d = 2 * c + if g(false) { 1 } else { 2 }; // Not used, should be optimized out
                a + b
            }
            "
        );
        let expected: Block = parse(
            "
            {
                fn f(a: i32, b: i32) -> i32 {
                    a + b
                }

                let a = 3;
                let b = f(2 + a * a, 3);
                a + b
            }
            ",
        );
        assert_optimize(&block, expected);
    }

    #[test]
    fn test_block_reduction() {
        let block: Block = parse(
            "
            {
                fn f(a: i32, b: i32) -> i32 {
                    a + b
                }

                let a = 3;
                let b = {
                    1 + 3; // Should be optimized out
                    f(2 + a * a, 3)
                };
                a + b
            }
            "
        );
        let expected: Block = parse(
            "
            {
                fn f(a: i32, b: i32) -> i32 {
                    a + b
                }

                let a = 3;
                let b = f(2 + a * a, 3);
                a + b
            }
            ",
        );
        assert_optimize(&block, expected);
    }

    #[test]
    fn test_useless_blocks_removal() {
        let block: Block = parse(
            "
            {
                let a = 1;

                {
                    let b = 2;
                    a + b;
                } // Useless block -> unit type + does not affect the outer block

                let b = {
                    let c = {
                        1 + 2
                    }; // Should be optimized out
                    let d = c + a; // Should be optimized out
                    a + 3
                };

                a + b
            }
            "
        );
        let expected: Block = parse(
            "
            {
                let a = 1;

                let b = a + 3;

                a + b
            }
            ",
        );
        assert_optimize(&block, expected);
    }

    #[test]
    fn test_useless_functions_removal() {
        let block: Block = parse(
            "
            {
                fn foo() -> i32 {
                    1 + 2
                } // Careful, this is used by dummy which is used!

                fn dummy(a: i32) -> i32 {
                    a + foo()
                }

                fn not_used() -> i32 {
                    1 + 2
                } // This is not used and should be optimized out

                let a = dummy(3);
                a
            }
            "
        );
        let expected: Block = parse(
            "
            {
                fn foo() -> i32 {
                    3
                }

                fn dummy(a: i32) -> i32 {
                    a + foo()
                }

                let a = dummy(3);
                a
            }
            ",
        );
        assert_optimize(&block, expected);
    }

    #[test]
    fn test_useless_var_def_1() {
        let block: Block = parse(
            "
            {
                let a = 1; // Should be optimized out because it is never used
                let a = 3;
                let b;
                1 + 3; // Should be optimized out
                f(2); // Should be optimized out
                b = a + 4;
                b
            }
            "
        );
        let expected: Block = parse(
            "
            {
                let a = 3;
                let b;
                b = a + 4;
                b
            }
            ",
        );
        assert_optimize(&block, expected);
    }

    // TODO
    #[test]
    fn test_useless_var_def_2() {
        let block: Block = parse(
            "
            {
                let a = 1; // Should be optimized out because it is never used
                let a = 3;
                let b; // Should be optimized out because it is never used
                b = 2; // Should be optimized out
                let b = a + 4;
                b
            }
            "
        );
        let expected: Block = parse(
            "
            {
                let a = 3;
                let b = a + 4;
                b
            }
            ",
        );
        assert_optimize(&block, expected);

        // Here, we should not forget to remove the assignation of b = 2 since it is never used.
        // Therefore, assignation should be considered variable definition (be careful about assigning to outer scope variables).
        //TODO: test case
        // {
        //     let a;
        //     if true {
        //         a = 2;
        //     } else {
        //         a = 3;
        //     }
        //     a
        // }
    }

    // TODO
    #[test]
    fn test_useless_var_def_3() {
        let block: Block = parse(
            "
            {
                let a = 1;
                let b = a + 2;
                let a = 3; // Should be optimized out because it is never used
                let a = 4;
                a + b
            }
            "
        );
        let expected: Block = parse(
            "
            {
                let a = 1;
                let b = a + 2;
                let a = 4;
                a + b
            }
            ",
        );
        assert_optimize(&block, expected);

        // Here, the trick is to be able to correctly locate where the useless variable is since it is not necessarily the first time 'a' is defined.
    }

    #[test]
    fn test_useless_var_def_4() {
        let block: Block = parse(
            "
            {
                let c = 2; // Should be optimized out because it is never used in the end
                let a = 1 - c; // Should be optimized out because it is never used in the end
                let b = a + c; // Should be optimized out because never used before definition of second b
                let a = 3;
                let b; // Should be optimized out because it is never used
                let b = a + 4;
                b
            }
            "
        );
        let expected: Block = parse(
            "
            {
                let a = 3;
                let b = a + 4;
                b
            }
            ",
        );
        assert_optimize(&block, expected);

        // Comment on this example:
        // It is difficult to detect the first definition of a as useless since it is redefined before we detect b as useless
        // For instance, even the rust compiler does not detect it as useless at the first iteration
        // This is why I have implemented a recursive part of optimization inside the block optimization

        // Uncomment this block to see the behavior of the rust compiler on this example
        // let var = {
        //     let c = 2; // Not detected as useless (Needs 3 iterations)
        //     let a = 1 - c; // Not detected as useless (Needs 2 iterations)
        //     let b = a + c; // Detected as useless (From the 1st iteration)
        //     let a = 3;
        //     let b: i32; // Detected as useless (From the 1st iteration)
        //     let b = a + 4;
        //     b
        // };
    }

    //? Optimizing programs

    #[test]
    fn test_prog_opti_1() {
        let prog: Prog = parse(
            "
            fn main() {
                let a = 1; // Should be optimized out because it is never used
                let a = 3;
                let b;
                1 + 3; // Should be optimized out
                f(2); // Should be optimized out
                b = a + 4;
            }
            ",
        );
        let expected: Prog = parse(
            "
            fn main() {
                let a = 3;
                let b;
                b = a + 4;
            }
            ",
        );
        assert_optimize(&prog, expected);
    }

    #[test]
    fn test_prog_opti_2() {
        let prog: Prog = parse(
            "
            fn main() {
                let a = if false {
                    3
                } else {
                    4
                };
                let b = {
                    1 + a; // Should be optimized out
                    a + 3
                };
            }
            ",
        );
        let expected: Prog = parse(
            "
            fn main() {
                let a = 4;
                let b = a + 3;
            }
            ",
        );
        assert_optimize(&prog, expected);
    }

    // TODO
    #[test]
    fn test_prog_not_used_functions() {
        let prog: Prog = parse(
            "
            fn foo() -> i32 {
                1 + 2
            } // Careful, this is used by dummy which is used!

            fn dummy(a: i32) -> i32 {
                a + foo()
            }

            fn not_used() -> i32 {
                1 + 2
            } // This is not used and should be optimized out

            // main can never be optimized out
            fn main() {
                let a = dummy(3);
            }
            ",
        );
        let expected: Prog = parse(
            "
            fn foo() -> i32 {
                3
            }

            fn dummy(a: i32) -> i32 {
                a + foo()
            }

            // main can never be optimized out
            fn main() {
                let a = dummy(3);
            }
            ",
        );
        assert_optimize(&prog, expected);
    }
}
