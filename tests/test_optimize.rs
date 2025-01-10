use rnr::ast::{Block, Expr, Prog};
use rnr::test_util::assert_optimize;
use rnr::common::parse;

#[cfg(test)]
pub mod test_optimize {
    use super::*;

    #[cfg(test)]
    pub mod testing_expressions {
        use super::*;

        // Warning: Expressions can't use literals because the optimizer will try to find their definition
        // Therefore, tests with literals should be done in a proper block with proper definitions

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
            let block: Block = parse(
                "
                {
                    let a = 3;
                    a
                }
                ",
            );
            let expected = block.clone();
            assert_optimize(&block, expected);
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
            let block: Block = parse(
                "
                {
                    let a = true;
                    !!a
                }
                ",
            );
            let expected: Block = parse(
                "
                {
                    let a = true;
                    a
                }
                ",
            );
            assert_optimize(&block, expected);
        }

        #[test]
        fn test_bang_opti_lit_2() {
            let block: Block = parse(
                "
                {
                    let a = false;
                    !!!a
                }
                ",
            );
            let expected: Block = parse(
                "
                {
                    let a = false;
                    !a
                }
                ",
            );
            assert_optimize(&block, expected);
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
            let expected: Expr = parse("-3");
            assert_optimize(&expr, expected);
        }

        #[test]
        fn test_neg_opti_lit_1() {
            let block: Block = parse(
                "
                {
                    let a = 3;
                    --a
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
        fn test_neg_opti_lit_2() {
            let block: Block = parse(
                "
                {
                    let a = -3;
                    ---a
                }
                ",
            );
            let expected: Block = parse(
                "
                {
                    let a = -3;
                    -a
                }
                ",
            );
            assert_optimize(&block, expected);
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
            let block: Block = parse(
                "
                {
                    let a = 3;
                    3 + 4 + 5 + a
                }
                ",
            );
            let expected: Block = parse(
                "
                {
                    let a = 3;
                    12 + a
                }
                ",
            );
            assert_optimize(&block, expected);
        }

        //TODO: Add support for this kind of optimization
        #[test]
        #[ignore = "Not implemented yet (optimization of binops with literals)"]
        fn test_add_opti_with_literals_2() {
            let block: Block = parse(
                "
                {
                    let a = 3;
                    3 + 4 + 5 + a + 6
                }
                ",
            );
            let expected: Block = parse(
                "
                {
                    let a = 3;
                    18 + a
                }
                ",
            );
            assert_optimize(&block, expected);
        }

        #[test]
        fn test_mul_short_circuit_1() {
            let expr: Expr = parse("0 * a"); // Thanks to the short-circuit, a is never read
            let expected: Expr = parse("0");
            assert_optimize(&expr, expected);
        }

        #[test]
        fn test_mul_short_circuit_2() {
            let block: Block = parse(
                "
                {
                    let a = 6;
                    a * 0
                }
                ",
            );
            let expected: Block = parse(
                "
                {
                    0
                }
                ",
            );
            assert_optimize(&block, expected);
        }

        #[test]
        fn test_mul_short_circuit_3() {
            let block: Block = parse(
                "
                {
                    let a = 6;
                    let b = -1;
                    1 * 2 * a * 0 * b
                }
                ",
            );
            let expected: Block = parse(
                "
                {
                    0
                }
                ",
            );
            assert_optimize(&block, expected);
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
            let expr: Expr = parse("false && a"); // Thanks to the short-circuit, a is never read
            let expected: Expr = parse("false");
            assert_optimize(&expr, expected);
        }

        #[test]
        fn test_and_short_circuit_2() {
            let block: Block = parse(
                "
                {
                    let a = false;
                    true && a
                }
                ",
            );
            let expected: Block = parse(
                "
                {
                    let a = false;
                    a
                }
                ",
            );
            assert_optimize(&block, expected);
        }

        #[test]
        fn test_or_opti_1() {
            let expr: Expr = parse("true || false");
            let expected: Expr = parse("true");
            assert_optimize(&expr, expected);
        }

        #[test]
        fn test_or_short_circuit_1() {
            let expr: Expr = parse("true || a"); // Thanks to the short-circuit, a is never read
            let expected: Expr = parse("true");
            assert_optimize(&expr, expected);
        }

        #[test]
        fn test_or_short_circuit_2() {
            let block: Block = parse(
                "
                {
                    let a = true;
                    false || a
                }
                ",
            );
            let expected: Block = parse(
                "
                {
                    let a = true;
                    a
                }
                ",
            );
            assert_optimize(&block, expected);
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
            let block: Block = parse(
                "
                {
                    let a = 10;
                    3 + 2 < a
                }
                ",
            );
            let expected: Block = parse(
                "
                {
                    let a = 10;
                    5 < a
                }
                ",
            );
            assert_optimize(&block, expected);
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
            let block: Block = parse(
                "
                {
                    let a = 1;
                    [1, 2, 3][a]
                }
                ",
            );
            let expected = block.clone();
            assert_optimize(&block, expected);
        }

        #[test]
        fn test_array_get_with_expr_1() {
            let expr: Expr = parse("[1, 2, 3][1 + 1]");
            let expected: Expr = parse("3");
            assert_optimize(&expr, expected);
        }

        #[test]
        fn test_array_get_with_expr_2() {
            let block: Block = parse(
                "
                {
                    let a = 1;
                    [1, 2, 3][1 + a]
                }
                ",
            );
            let expected: Block = block.clone();
            assert_optimize(&block, expected);
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
            let block: Block = parse(
                "
                {
                    let a = 1;
                    let b = 2;
                    let c = 3;
                    [a, b, c][0]
                }
                ",
            );
            let expected: Block = parse(
                "
                {
                    let a = 1;
                    a
                }
                ",
            );
            assert_optimize(&block, expected);
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
            let block: Block = parse(
                "
                {
                    let a = 10;
                    ((3 + 4) * a)
                }
                ",
            );
            let expected: Block = parse(
                "
                {
                    let a = 10;
                    7 * a
                }
                ",
            );
            assert_optimize(&block, expected);
        }

        #[test]
        fn test_paren_opti_7() {
            let block: Block = parse(
                "
                {
                    let a = 10;
                    a * (3 + 4)
                }
                ",
            );
            let expected: Block = parse(
                "
                {
                    let a = 10;
                    a * 7
                }
                ",
            );
            assert_optimize(&block, expected);
        }

        //? Function calls

        #[test]
        fn test_fn_call_opti_1() {
            let block: Block = parse(
                "
                {
                    fn my_fun(a: i32) -> i32 {
                        a
                    }

                    my_fun(3 + 7)
                }
                ",
            );
            let expected: Block = parse(
                "
                {
                    fn my_fun(a: i32) -> i32 {
                        a
                    }

                    my_fun(10)
                }
                ",
            );
            assert_optimize(&block, expected);
        }

        #[test]
        fn test_fn_call_opti_2() {
            let block: Block = parse(
                "
                {
                    fn my_fun(a: i32, b: bool) -> i32 {
                        if b {
                            a
                        } else {
                            0
                        }
                    }

                    let a = false;
                    my_fun(3 + 7, true || a)
                }
                ",
            );
            let expected: Block = parse(
                "
                {
                    fn my_fun(a: i32, b: bool) -> i32 {
                        if b {
                            a
                        } else {
                            0
                        }
                    }

                    my_fun(10, true)
                }
                ",
            );
            assert_optimize(&block, expected);
        }

        #[test]
        fn test_fn_call_opti_3() {
            let block: Block = parse(
                "
                {
                    fn my_fun(a: i32, b: bool) -> i32 {
                        if b {
                            a
                        } else {
                            0
                        }
                    }

                    fn g(a: bool) -> i32 {
                        if a {
                            1
                        } else {
                            2
                        }
                    }

                    let a = false;
                    let b = false;
                    my_fun(g(true && b), false || a)
                }
                ",
            );
            let expected: Block = parse(
                "
                {
                    fn my_fun(a: i32, b: bool) -> i32 {
                        if b {
                            a
                        } else {
                            0
                        }
                    }

                    fn g(a: bool) -> i32 {
                        if a {
                            1
                        } else {
                            2
                        }
                    }

                    let a = false;
                    let b = false;
                    my_fun(g(b), a)
                }
                ",
            );
            assert_optimize(&block, expected);
        }

        //? If then else

        #[test]
        fn test_if_then_else_opti_1() {
            let expr: Expr = parse("if true { 3 } else { 4 }");
            let expected: Expr = parse("3");
            assert_optimize(&expr, expected);
        }
        #[test]
        fn test_if_then_else_opti_1_bis() {
            let expr: Expr = parse("if true || a { 3 } else { 4 }"); // with short-circuit in condition
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
            let block: Block = parse(
                "
                {
                    let a = true;

                    if false || a {
                        3
                    } else {
                        4
                    }
                }
                ",
            );
            let expected: Block = parse(
                "
                {
                    let a = true;

                    if a {
                        3
                    } else {
                        4
                    }
                }
                ",
            );
            assert_optimize(&block, expected);
        }

        #[test]
        fn test_if_then_else_opti_7() {
            let block: Block = parse(
                "
                {
                    let a = true;

                    if true && a {
                        3
                    } else {
                        4
                    }
                }
                ",
            );
            let expected: Block = parse(
                "
                {
                    let a = true;

                    if a {
                        3
                    } else {
                        4
                    }
                }
                ",
            );
            assert_optimize(&block, expected);
        }
    }

    //? Optimizing blocks

    #[cfg(test)]
    pub mod testing_blocks {
        use super::*;

        #[test]
        fn test_block_opti() {
            let block: Block = parse(
                "
                {
                    fn f(a: i32) -> i32 { // Not used, should be optimized out
                        a
                    }

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
        fn test_while_opti() {
            let block: Block = parse(
                "
                {
                    let mut a = 1;
                    while a < 10 {
                        5 + 1; // Should be optimized out
                        a = a + 1;
                        3 + 5; // Should be optimized out
                    }
                    a
                }
                "
            );
            let expected: Block = parse(
                "
                {
                    let mut a = 1;
                    while a < 10 {
                        a = a + 1;
                    }
                    a
                }
                ",
            );
            assert_optimize(&block, expected);
        }

        #[test]
        fn test_while_opti_in_block() {
            let block: Block = parse(
                "
                {
                    let mut a = 1;
                    {
                        while a < 10 {
                            a = a + 1;
                        }
                    }
                    a
                }
                "
            );
            let expected: Block = block.clone();
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
        }

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
                    let a = 1;
                    let b = a + 2;
                    let a = 3; // Should be optimized out because it is never used
                    b
                }
                "
            );
            let expected: Block = parse(
                "
                {
                    let a = 1;
                    let b = a + 2;
                    b
                }
                ",
            );
            assert_optimize(&block, expected);

            // Here, the trick is to be able to correctly locate where the useless variable is since it is not necessarily the first time 'a' is defined.
        }

        #[test]
        fn test_useless_var_def_5() {
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

        #[test]
        fn test_useless_var_def_inner_block_1() {
            let block: Block = parse(
                "
                {
                    let a;
                    let mut b = 2; // Useless var, should be removed
                    let c = {
                        a = 6;
                        b = -3; // Useless var, should be removed
                        a + 3
                    };
                    a + c
                }
                "
            );
            let expected: Block = parse(
                "
                {
                    let a;
                    let c = {
                        a = 6;
                        a + 3
                    };
                    a + c
                }
                ",
            );
            assert_optimize(&block, expected);
        }

        #[test]
        fn test_useless_var_def_inner_block_2() {
            let block: Block = parse(
                "
                {
                    fn f(a: i32) -> i32 { // In the end, this function is useless
                        a + 1
                    }

                    fn g() -> i32 {
                        1
                    }

                    let a;
                    let mut b = 2; // Useless var, should be removed
                    let c = {
                        a = g();
                        b = f(1); // Useless var, should be removed
                        a + 3
                    };
                    a + c
                }
                "
            );
            let expected: Block = parse(
                "
                {
                    fn g() -> i32 {
                        1
                    }

                    let a;
                    let c = {
                        a = g();
                        a + 3
                    };
                    a + c
                }
                ",
            );
            assert_optimize(&block, expected);
        }

        #[test]
        fn test_useless_var_def_with_if_1() {
            let block: Block = parse(
                "
                {
                    let a;
                    let b; // Useless var, should be removed
                    let c = true;
                    if c {
                        b = 1; // Useless var, should be removed
                        a = 2;
                    } else {
                        b = 4; // Useless var, should be removed
                        a = 3;
                    }
                    a
                }
                "
            );
            let expected: Block = parse(
                "
                {
                    let a;
                    let c = true;
                    if c {
                        a = 2;
                    } else {
                        a = 3;
                    }
                    a
                }
                ",
            );
            assert_optimize(&block, expected);
        }

        #[test]
        fn test_useless_var_def_with_if_2() {
            let block: Block = parse(
                "
                {
                    fn f(a: i32) -> i32 { // In the end, this function is useless
                        a + 1
                    }

                    fn g() -> i32 {
                        1
                    }

                    let a;
                    let b; // Useless var, should be removed
                    let c = true;
                    if c {
                        b = 1; // Useless var, should be removed
                        a = g();
                    } else {
                        b = f(2); // Useless var, should be removed
                        a = 3;
                    }
                    a
                }
                "
            );
            let expected: Block = parse(
                "
                {
                    fn g() -> i32 {
                        1
                    }

                    let a;
                    let c = true;
                    if c {
                        a = g();
                    } else {
                        a = 3;
                    }
                    a
                }
                ",
            );
            assert_optimize(&block, expected);
        }

        #[test]
        fn test_useless_var_def_with_while_1() {
            let block: Block = parse(
                "
                {
                    let mut a = 3;
                    let mut b = 0; // Useless var, should be removed
                    while a < 10 {
                        a = a + 1;
                        b = 3 * a; // Useless var, should be removed
                    }
                    a
                }
                "
            );
            let expected: Block = parse(
                "
                {
                    let mut a = 3;
                    while a < 10 {
                        a = a + 1;
                    }
                    a
                }
                ",
            );
            assert_optimize(&block, expected);
        }

        #[test]
        fn test_useless_var_def_with_while_2() {
            let block: Block = parse(
                "
                {
                    fn f(a: i32) -> i32 { // In the end, this function is useless
                        a + 1
                    }

                    fn g() -> i32 {
                        1
                    }

                    let mut a = 3;
                    let mut b = 0; // Useless var, should be removed
                    while a < 10 {
                        a = a + g(); // a + 1
                        b = f(a); // Useless var, should be removed
                    }
                    a
                }
                "
            );
            let expected: Block = parse(
                "
                {
                    fn g() -> i32 {
                        1
                    }

                    let mut a = 3;
                    while a < 10 {
                        a = a + g(); // a + 1
                    }
                    a
                }
                ",
            );
            assert_optimize(&block, expected);
        }

        #[test]
        #[ignore = "Recursion not yet supported -> will result in an infinite loop"]
        fn test_useless_recursive_function() {
            let block: Block = parse(
                "
                {
                    fn fact(n: i32) -> i32 { // Useless, should be removed
                        if n == 0 {
                            1
                        } else {
                            n * fact(n - 1)
                        }
                    }

                    let a = 1;
                    a
                }
                "
            );
            let expected: Block = parse(
                "
                {
                    let a = 1;
                    a
                }
                ",
            );
            assert_optimize(&block, expected);

            // The issue here is that calling function `fact` inside its own definition should not be considered a use of the function
        }

        #[test]
        #[ignore = "Recursion not yet supported -> will result in an infinite loop"]
        fn test_useless_mutually_recursive_functions() {
            let block: Block = parse(
                "
                {
                    fn is_odd(n: i32) -> bool { // Useless, should be removed
                        if n == 0 {
                            false
                        } else {
                            is_even(n - 1)
                        }
                    }

                    fn is_even(n: i32) -> bool { // Useless, should be removed
                        if n == 0 {
                            true
                        } else {
                            is_odd(n - 1)
                        }
                    }

                    let a = 1;
                    a
                }
                "
            );
            let expected: Block = parse(
                "
                {
                    let a = 1;
                    a
                }
                ",
            );
            assert_optimize(&block, expected);

            // The issue here is that mutually recursive functions should not be considered as used as long as they are not for real
        }

        #[test]
        #[ignore = "Recursion not yet supported -> will result in an infinite loop"]
        fn test_useful_recursive_function() {
            let block: Block = parse(
                "
                {
                    fn fact(n: i32) -> i32 {
                        if n == 0 {
                            1
                        } else {
                            n * fact(n - 1)
                        }
                    }

                    let a = fact(5);
                    a
                }
                "
            );
            let expected: Block = block.clone();
            assert_optimize(&block, expected);

            // This time, the function is really of use
        }

        #[test]
        #[ignore = "Recursion not yet supported -> will result in an infinite loop"]
        fn test_useful_mutually_recursive_functions() {
            let block: Block = parse(
                "
                {
                    fn is_odd(n: i32) -> bool {
                        if n == 0 {
                            false
                        } else {
                            is_even(n - 1)
                        }
                    }

                    fn is_even(n: i32) -> bool {
                        if n == 0 {
                            true
                        } else {
                            is_odd(n - 1)
                        }
                    }

                    let a = is_even(5);
                    a
                }
                "
            );
            let expected: Block = block.clone();
            assert_optimize(&block, expected);

            // This time, the two functions are really of use
        }
    }

    //? Optimizing programs

    #[cfg(test)]
    pub mod testing_progs {
        use super::*;

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
}
