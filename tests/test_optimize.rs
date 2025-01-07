use rnr::ast::{Block, Expr, Literal, Prog};
use rnr::common::Optimize;

#[cfg(test)]
pub mod test_optimize {
    use rnr::common::parse;

    use super::*;

    //? Testing things that can't be optimized

    #[test]
    fn test_optimize_literal_1() {
        let expr: Expr = parse("true");
        let expected = expr.clone();
        let res = expr.optimize();
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_optimize_literal_2() {
        let expr: Expr = parse("3");
        let expected = expr.clone();
        let res = expr.optimize();
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_optimize_identifier() {
        let expr: Expr = parse("my_var");
        let expected = expr.clone();
        let res = expr.optimize();
        assert_eq!(res, Ok(expected));
    }

    //? Testing simple unops

    #[test]
    fn test_bang_opti_1() {
        let expr: Expr = parse("!true");
        let res = expr.optimize();
        let expected: Expr = parse("false");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_bang_opti_2() {
        let expr: Expr = parse("!!true");
        let res = expr.optimize();
        let expected: Expr = parse("true");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_bang_opti_3() {
        let expr: Expr = parse("!!!false");
        let res = expr.optimize();
        let expected: Expr = parse("true");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_bang_opti_lit_1() {
        let expr: Expr = parse("!!a");
        let res = expr.optimize();
        let expected: Expr = parse("a");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_bang_opti_lit_2() {
        let expr: Expr = parse("!!!a");
        let res = expr.optimize();
        let expected: Expr = parse("!a");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_neg_opti_1() {
        let expr: Expr = parse("--3");
        let res = expr.optimize();
        let expected: Expr = parse("3");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_neg_opti_2() {
        let expr: Expr = parse("---3");
        let res = expr.optimize();
        // let expected: Expr = parse("-3"); // This is parsed as Neg(3) and not Lit(-3)
        let expected = Expr::Lit(Literal::Int(-3));
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_neg_opti_lit_1() {
        let expr: Expr = parse("--a");
        let res = expr.optimize();
        let expected: Expr = parse("a");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_neg_opti_lit_2() {
        let expr: Expr = parse("---a");
        let res = expr.optimize();
        let expected: Expr = parse("-a");
        assert_eq!(res, Ok(expected));
    }

    //? Testing simple binops

    #[test]
    fn test_add_opti_1() {
        let expr: Expr = parse("3 + 4");
        let res = expr.optimize();
        let expected: Expr = parse("7");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_add_opti_2() {
        let expr: Expr = parse("3 + 4 + 5");
        let res = expr.optimize();
        let expected: Expr = parse("12");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_add_opti_3() {
        let expr: Expr = parse("3 + 4 + 5 + 6");
        let res = expr.optimize();
        let expected: Expr = parse("18");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_add_opti_with_literals_1() {
        let expr: Expr = parse("3 + 5 + 6 + a");
        let res = expr.optimize();
        let expected: Expr = parse("14 + a");
        assert_eq!(res, Ok(expected));
    }

    //TODO: Add support for this kind of optimization
    #[test]
    #[ignore = "Not implemented yet (optimization of binops with literals)"]
    fn test_add_opti_with_literals_2() {
        let expr: Expr = parse("3 + a + 5 + 6");
        let res = expr.optimize();
        let expected: Expr = parse("14 + a");
        assert_eq!(res, Ok(expected));
    }

    // booleans

    #[test]
    fn test_and_opti_1() {
        let expr: Expr = parse("true && false");
        let res = expr.optimize();
        let expected: Expr = parse("false");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_and_opti_2() {
        let expr: Expr = parse("true && false && true");
        let res = expr.optimize();
        let expected: Expr = parse("false");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_and_short_circuit_1() {
        let expr: Expr = parse("false && a");
        let res = expr.optimize();
        let expected: Expr = parse("false");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_and_short_circuit_2() {
        let expr: Expr = parse("true && a");
        let res = expr.optimize();
        let expected: Expr = parse("a");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_or_opti_1() {
        let expr: Expr = parse("true || false");
        let res = expr.optimize();
        let expected: Expr = parse("true");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_or_short_circuit_1() {
        let expr: Expr = parse("true || a");
        let res = expr.optimize();
        let expected: Expr = parse("true");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_or_short_circuit_2() {
        let expr: Expr = parse("false || a");
        let res = expr.optimize();
        let expected: Expr = parse("a");
        assert_eq!(res, Ok(expected));
    }

    // comparison

    #[test]
    fn test_eq_opti_1() {
        let expr: Expr = parse("3 == 3");
        let res = expr.optimize();
        let expected: Expr = parse("true");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_eq_opti_2() {
        let expr: Expr = parse("3 == 4");
        let res = expr.optimize();
        let expected: Expr = parse("false");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_ne_opti_1() {
        let expr: Expr = parse("3 != 3");
        let res = expr.optimize();
        let expected: Expr = parse("false");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_ne_opti_2() {
        let expr: Expr = parse("3 != 4");
        let res = expr.optimize();
        let expected: Expr = parse("true");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_comp_opti_1() {
        let expr: Expr = parse("3 < 4");
        let res = expr.optimize();
        let expected: Expr = parse("true");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_comp_opti_2() {
        let expr: Expr = parse("3 + 2 < 4");
        let res = expr.optimize();
        let expected: Expr = parse("false");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_comp_opti_3() {
        let expr: Expr = parse("3 + 2 < a");
        let res = expr.optimize();
        let expected: Expr = parse("5 < a");
        assert_eq!(res, Ok(expected));
    }

    // arrays get

    #[test]
    fn test_array_get_opti_1() {
        let expr: Expr = parse("[1, 2, 3][1]");
        let res = expr.optimize();
        let expected: Expr = parse("2");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_array_get_opti_2() {
        let expr: Expr = parse("[1, 2, 3][0]");
        let res = expr.optimize();
        let expected: Expr = parse("1");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_array_get_opti_3() {
        let expr: Expr = parse("[1, 2, 3][2]");
        let res = expr.optimize();
        let expected: Expr = parse("3");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_array_get_opti_4() {
        let expr: Expr = parse("a[3]");
        let res = expr.optimize();
        let expected: Expr = parse("a[3]");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_array_get_with_expr_1() {
        let expr: Expr = parse("[1, 2, 3][1 + 1]");
        let res = expr.optimize();
        let expected: Expr = parse("3");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_array_get_with_expr_2() {
        let expr: Expr = parse("[1, 2, 3][1 + a]");
        let res = expr.optimize();
        let expected: Expr = parse("[1, 2, 3][1 + a]");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    #[ignore = "Not implemented yet (can't parse expr in arrays)"]
    fn test_array_get_with_expr_3() {
        let expr: Expr = parse("[1, 2 * 5 + 7, 3][1]");
        let res = expr.optimize();
        let expected: Expr = parse("17");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    #[ignore = "Not implemented yet (can't parse expr in arrays)"]
    fn test_array_get_with_expr_4() {
        let expr: Expr = parse("[a, b, c][0]");
        let res = expr.optimize();
        let expected: Expr = parse("a");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_array_of_arrays_get_1() {
        let expr: Expr = parse("[[1, 2], [3, 4]][1]");
        let res = expr.optimize();
        let expected: Expr = parse("[3, 4]");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_array_of_arrays_get_2() {
        let expr: Expr = parse("[[1, 2], [3, 4]][1][0]");
        let res = expr.optimize();
        let expected: Expr = parse("3");
        assert_eq!(res, Ok(expected));
    }

    //? Testing parenthesis

    #[test]
    fn test_paren_opti_1() {
        let expr: Expr = parse("(3)");
        let res = expr.optimize();
        let expected: Expr = parse("3");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_paren_opti_2() {
        let expr: Expr = parse("(3 + 4)");
        let res = expr.optimize();
        let expected: Expr = parse("7");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_paren_opti_3() {
        let expr: Expr = parse("(3 + 4) * 5");
        let res = expr.optimize();
        let expected: Expr = parse("35");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_paren_opti_4() {
        let expr: Expr = parse("(5 * (3 + 4))");
        let res = expr.optimize();
        let expected: Expr = parse("35");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_paren_opti_5() {
        let expr: Expr = parse("((5))");
        let res = expr.optimize();
        let expected: Expr = parse("5");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_paren_opti_6() {
        let expr: Expr = parse("((3 + 4) * a)");
        let res = expr.optimize();
        let expected: Expr = parse("7 * a");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_paren_opti_7() {
        let expr: Expr = parse("(a * (3 + 4))");
        let res = expr.optimize();
        let expected: Expr = parse("a * 7");
        assert_eq!(res, Ok(expected));
    }

    //? Function calls

    #[test]
    fn test_fn_call_opti_1() {
        let expr: Expr = parse("my_fn(3 + 7)");
        let res = expr.optimize();
        let expected: Expr = parse("my_fn(10)");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_fn_call_opti_2() {
        let expr: Expr = parse("my_fn(3 + 7, true || a)");
        let res = expr.optimize();
        let expected: Expr = parse("my_fn(10, true)");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_fn_call_opti_3() {
        let expr: Expr = parse("my_fn(g(true && b), false || a)");
        let res = expr.optimize();
        let expected: Expr = parse("my_fn(g(b), a)");
        assert_eq!(res, Ok(expected));
    }

    //? If then else

    //TODO: Combine with block optim to replace blocks by their return value
    //TODO: Blocks that do not return a value, and do not do any peculiar things can be removed
    #[test]
    fn test_if_then_else_opti_1() {
        let expr: Expr = parse("if true { 3 } else { 4 }");
        let res = expr.optimize();
        let expected: Expr = parse("{ 3 }");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_if_then_else_opti_2() {
        let expr: Expr = parse("if false { 3 } else { 4 }");
        let res = expr.optimize();
        let expected: Expr = parse("{ 4 }");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_if_then_else_opti_3() {
        let expr: Expr = parse("if false { 3 }");
        let res = expr.optimize();
        let expected: Expr = parse("{ }");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_if_then_else_opti_4() {
        let expr: Expr = parse("if true || false { 3 } else { 4 }");
        let res = expr.optimize();
        let expected: Expr = parse("{ 3 }");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_if_then_else_opti_5() {
        let expr: Expr = parse("if true && false { 3 } else { 4 }");
        let res = expr.optimize();
        let expected: Expr = parse("{ 4 }");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_if_then_else_opti_6() {
        let expr: Expr = parse("if false || a { 3 } else { 4 }");
        let res = expr.optimize();
        let expected: Expr = parse("if a { 3 } else { 4 }");
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_if_then_else_opti_7() {
        let expr: Expr = parse("if true && a { 3 } else { 4 }");
        let res = expr.optimize();
        let expected: Expr = parse("if a { 3 } else { 4 }");
        assert_eq!(res, Ok(expected));
    }

    //? Optimizing blocks
    
    #[test]
    fn test_block_opti_1() {
        let block: Block = parse(
            "
            {
                let a = 3;
                1 + 3; // Should be optimized out
                f(2); // Should be optimized out
                a
            }
            "
        );
        let res = block.optimize();
        let expected: Block = parse(
            "
            {
                let a = 3;
                a
            }
            "
        );
        assert_eq!(res, Ok(expected));
    }

    #[test]
    fn test_larger_block_opti_1() {
        let block: Block = parse(
            "
            {
                fn f(a: i32, b: i32) -> i32 {
                    a + b
                }

                fn g(a: bool) -> bool { // Not used, should be optimized out
                    !a
                }

                let a = 3;
                let b = f(2 + a * a, 3);
                let c = b - 2 * a + f(a * b, -4); // Not used, should be optimized out
                a + b
            }
            "
        );
        let res = block.optimize();
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
            "
        );
        assert_eq!(res, Ok(expected));
    }

    //? Optimizing programs

    #[test]
    #[ignore = "Not implemented yet"]
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
            "
        );
        let res = prog.optimize();
        let expected: Prog = parse(
            "
            fn main() {
                let a = 3;
                let b;
                b = a + 4;
            }
            "
        );
        assert_eq!(res, Ok(expected));
    }

    #[test]
    #[ignore = "Not implemented yet"]
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
            "
        );
        let res = prog.optimize();
        let expected: Prog = parse(
            "
            fn main() {
                let a = 4;
                let b = a + 3;
            }
            "
        );
        assert_eq!(res, Ok(expected));
    }
}
