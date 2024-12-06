//use syn::parse::Parse;
use rnr::ast::*;
//use rnr::util::*;
use rnr::test_util::*;
//use rnr::parse::Parse;

#[cfg(test)]
mod expr {
    use super::*;

    // These are very simple and easy to implement. The others require support for unary operators,
    // parentheses, and/or correct associativity and precedence.
    #[test]
    #[allow(unused_parens)]
    fn arithmetic_expr_simple() {
        test_expr!(1);
        test_expr!(1 + 2);
        test_expr!(1 + 2 + 3);
        test_expr!(2 * 3);
    }

    #[allow(unused_parens)]
    #[test]
    fn arithmetic_expr() {
        test_expr!(1);
        test_expr!(123);
        test_expr!(-1);
        test_expr!(1 + 2);
        test_expr!(1 + 2 + 3);
        test_expr!(2 * 3);
        test_expr!(2 - 3);
        test_expr!(2 - 4 - 5);
        test_expr!(2 * 4 - 5);
        test_expr!(1 + 2 + 3 + 4 + 5);
        test_expr!(1 - 2 - 3 - 4 - 5);
        test_expr!(1 + 2 * 3 + 4);
        test_expr!(1 + 2 * 3 + 4 * 5 + 6);
        test_expr!(1 + 2 * 3 + 4 * 5 + 6 * 7);
        test_expr!(1 * 2 + 3 * 4 + 5 * 6);
        test_expr!(1 * 2 + 3 * 4 + 5 * 6 + 7);
        test_expr!(1 + 2 + 3 + 4 * 5 * 6 * 7);
        test_expr!(1 * 2 * 3 * 4 + 5 + 6 + 7);
        // More operators
        test_expr!(3 / 2);
        test_expr!(1 + 2 - 3 + 4 - 5 + 6);
        test_expr!(1 * 2 + 3 * 4 - 5 * 6);
        test_expr!(2 / 1 + 3 * 4 / 2 - 5 / 2 * 6);
        // With parens...
        test_expr!((1));
        test_expr!((-1));
        test_expr!((1 + 2));
        test_expr!((1 + 2) * 3);
        test_expr!((2 - 4) - 5);
        test_expr!((2 * 4) - 5);
        test_expr!(3 * (1 + 2));
        test_expr!(3 + (2 * 3) + 4);
        test_expr!(3 * (2 + 3) * 4);
        test_expr!(((1 + 2) * (3 + 4)));
    }

    #[test]
    fn logic_expr() {
        test_expr!(true);
        test_expr!(false);
        test_expr!(true || true);
        test_expr!(true || false);
        test_expr!(false || true);
        test_expr!(false || false);
        test_expr!(true || true || true);
        test_expr!(true || true || false);
        test_expr!(true || false || true);
        test_expr!(true || false || false);
        test_expr!(false || true || true);
        test_expr!(false || true || false);
        test_expr!(false || false || true);
        test_expr!(false || false || false);
        test_expr!(true && true);
        test_expr!(true && false);
        test_expr!(false && true);
        test_expr!(false && false);
        test_expr!(true && true && true);
        test_expr!(true && true && false);
        test_expr!(true && false && true);
        test_expr!(true && false && false);
        test_expr!(false && true && true);
        test_expr!(false && true && false);
        test_expr!(false && false && true);
        test_expr!(false && false && false);
        // Mixed
        test_expr!(true || true && true || true);
        test_expr!(true || true && true || false);
        test_expr!(true || true && false || true);
        test_expr!(true || true && false || false);
        test_expr!(true || false && true || true);
        test_expr!(true || false && true || false);
        test_expr!(true || false && false || true);
        test_expr!(true || false && false || false);
        test_expr!(false || true && true || true);
        test_expr!(false || true && true || false);
        test_expr!(false || true && false || true);
        test_expr!(false || true && false || false);
        test_expr!(false || false && true || true);
        test_expr!(false || false && true || false);
        test_expr!(false || false && false || true);
        test_expr!(false || false && false || false);
        // With parens...
        test_expr!((true || true) && (true || true));
        test_expr!((true || true) && (true || false));
        test_expr!((true || true) && (false || true));
        test_expr!((true || true) && (false || false));
        test_expr!((true || false) && (true || true));
        test_expr!((true || false) && (true || false));
        test_expr!((true || false) && (false || true));
        test_expr!((true || false) && (false || false));
        test_expr!((false || true) && (true || true));
        test_expr!((false || true) && (true || false));
        test_expr!((false || true) && (false || true));
        test_expr!((false || true) && (false || false));
        test_expr!((false || false) && (true || true));
        test_expr!((false || false) && (true || false));
        test_expr!((false || false) && (false || true));
        test_expr!((false || false) && (false || false));
    }

    #[allow(unused_parens)]
    #[test]
    fn arithmetic_expr_with_unary_ops() {
        test_expr!(--1);
        test_expr!(-(-1));
        test_expr!(-(1 + 2));
        test_expr!(2 + -3);
        test_expr!((-1 + -2));
        test_expr!(-(-1 + -2) * -3);
        test_expr!(-3 * -(-2 + -3) * -4);
        test_expr!(-2 / 1 + -3 * -4 / 2 - 5 / -2 * -6);
    }

    // NOTE: Remember that when we give `test_expr!` a boolean expression, the point is not to
    // assert that the expression is true, but rather that parsing and evaluating the expression
    // gives *the same* result is the Rust compiler, whether true or false.
    #[test]
    fn logic_expr_with_unary_bang() {
        test_expr!(!true);
        test_expr!(!false);
        test_expr!(!!true);
        test_expr!(!!false);
        test_expr!(!!!true);
        test_expr!(!!!false);
        test_expr!(!true || !true);
        test_expr!(!true || !false);
        test_expr!(!false || !true);
        test_expr!(!false || !false);
        test_expr!(!(!true || !true) && !(!true || !true));
        test_expr!(!(!true || !true) && !(!true || !false));
        test_expr!(!(!true || !true) && !(!false || !true));
        test_expr!(!(!true || !true) && !(!false || !false));
        test_expr!(!(!true || !false) && !(!true || !true));
        test_expr!(!(!true || !false) && !(!true || !false));
        test_expr!(!(!true || !false) && !(!false || !true));
        test_expr!(!(!true || !false) && !(!false || !false));
        test_expr!(!(!false || !true) && !(!true || !true));
        test_expr!(!(!false || !true) && !(!true || !false));
        test_expr!(!(!false || !true) && !(!false || !true));
        test_expr!(!(!false || !true) && !(!false || !false));
        test_expr!(!(!false || !false) && !(!true || !true));
        test_expr!(!(!false || !false) && !(!true || !false));
        test_expr!(!(!false || !false) && !(!false || !true));
        test_expr!(!(!false || !false) && !(!false || !false));
    }

    /// Only uses `==`, `<`, and `>`.
    #[test]
    fn comparison_expr_eq_lt_gt() {
        test_expr!(1 == 1);
        test_expr!(1 == 2);
        test_expr!(0 + 1 == 1 + 0);
        test_expr!(0 + 1 == 2 * 1);
        test_expr!(0 + 1 * 2 == 4);
        test_expr!(3 == 0 + 1 * 2);
        test_expr!(0 + 1 + 2 == 3 * 4 * 5);
        test_expr!(1 * 2 * 3 == 4 + 5 + 6);
        test_expr!(0 + 1 * 2 + 3 * 4 == 5 * 6 + 7 * 8);
        test_expr!(0 + 1 + 2 * 3 * 4 == 5 * 6 * 7 + 8 + 9);
        test_expr!(1 + 2 * 3 + 4 == 3 * (1 + 2) + 2 * 1);
        test_expr!(true == true);
        test_expr!(true == false);
        test_expr!(
            (0 + 1 + 2 * 3 * 4 < 5 * 6 * 7 + 8 + 9)
                == (true && (3 * (1 + 2) < 1 + 2 * 3 + 4) || false)
        );
    }

    #[test]
    fn arithmetic_expr_with_if() {
        test_expr!(if true { 1 } else { 2 });
        test_expr!(if false { 1 } else { 2 });
        test_expr!(if 1 + 2 < 3 * 4 { 1 + 10 / 5 } else { 2 * -3 });
        test_expr!(if !(1 > 0 && !(0 == -3)) || !true {
            1 / 0
        } else {
            5 - 2 - 1
        });
        test_expr!(1 + if true { 1 } else { 2 } * 2);
        test_expr!(2 * if false { 1 } else { 2 } - 1);
        test_expr!(if false { 1 } else { 2 } * 2 + if true { 1 } else { 2 } / 2);
        test_expr!(if true { 1 } else { 2 } == if false { 1 } else { 2 });
        test_expr!(if true || !false {
            1 - -3 * -2
        } else {
            if !false && (3 > 2 || 2 < 3) {
                2 * (1 + 2)
            } else {
                3 / 1 - 1
            }
        });
        test_expr!(!!!if !!true {
            true
        } else {
            if true && (!true || false) {
                !(true && false)
            } else {
                true && !false
            }
        });
        test_expr!(
            if 2 > 1 { 2 * 3 + 1 } else { -4 / 2 }
                < if !false || !true { 3 - 1 * 2 } else { 0 * -1 }
        );
        test_expr!(
            if 1 == 3 / 2 { 2 > 1 } else { 3 < 1 + 2 }
                && !if !(3 / 2 == -1 + 2) {
                    true == false || true
                } else {
                    !true
                }
        );
    }

    #[test]
    fn arithmetic_expr_with_if_nested() {
        test_expr!(if false {
            1
        } else {
            if true {
                2
            } else {
                3
            }
        });
        test_expr!(if true {
            1
        } else {
            if true {
                2
            } else {
                3
            }
        });
        test_expr!(if true {
            1
        } else {
            if false {
                2
            } else {
                3
            }
        });
        test_expr!(if false {
            1
        } else {
            if true {
                2
            } else {
                3
            }
        });
        test_expr!(if false {
            1
        } else {
            if false {
                2
            } else {
                3
            }
        });
        test_expr!(if true {
            1
        } else if true {
            2
        } else {
            3
        });
        test_expr!(if true {
            1
        } else if false {
            2
        } else {
            3
        });
        test_expr!(if false {
            1
        } else if true {
            2
        } else {
            3
        });
        test_expr!(if false {
            1
        } else if false {
            2
        } else {
            3
        });
    }

    // #[ignore = "tricky"]
    #[test]
    fn crazy() {
        test_expr!(
            if false {
                1
            } else {
                if false {
                    2
                } else {
                    3
                }
            } * 2
                + if true {
                    1
                } else if true {
                    2
                } else {
                    3
                } / 2
        );
        test_expr!(
            if true {
                1
            } else if false {
                2
            } else {
                3
            } == if false {
                1
            } else if true {
                2
            } else {
                3
            }
        );
    }

    /// Also uses `!=`, `<=`, and `>=`.
    // #[ignore = "Implementing !=, <=, >= is not mandatory."]
    #[test]
    fn comparison_expr_neq_leq_geq() {
        test_expr!(1 != 1);
        test_expr!(1 != 2);
        test_expr!(0 + 1 != 1 + 0);
        test_expr!(0 + 1 != 2 * 1);
        test_expr!(0 + 1 * 2 != 4);
        test_expr!(3 != 0 + 1 * 2);
        test_expr!(0 + 1 + 2 != 3 * 4 * 5);
        test_expr!(1 * 2 * 3 != 4 + 5 + 6);
        test_expr!(0 + 1 * 2 + 3 * 4 != 5 * 6 + 7 * 8);
        test_expr!(0 + 1 + 2 * 3 * 4 != 5 * 6 * 7 + 8 + 9);
        test_expr!(1 + 2 * 3 + 4 != 3 * (1 + 2) + 2 * 1);
        test_expr!(true != true);
        test_expr!(true != false);
        test_expr!(
            !(0 + -1 + 2 * 3 / 4 >= 5 * 6 / 7 + 8 + 9)
                != !(true && (3 * -(-1 + 2) <= -1 + 2 / 3 - 4) || false)
        );
    }

    // #[ignore = "Requires that short-circuiting of logical operations are implemented."]
    #[test]
    fn short_circuit() {
        test_expr!(1 < 0 && 1 < 1 / 0);
        test_expr!(1 > 0 || 1 < 1 / 0);
    }
}

#[cfg(test)]
mod more_tests {
    use super::*;

    #[test]
    fn block_with_typed_immutable_variables_1() {
        test_block!({
            let x: i32 = 1;
            let y: i32 = 2;
            x + y
        });
    }

    #[test]
    fn block_with_late_init() {
        test_block!({
            let x: i32;
            let y: i32;
            x = 1;
            y = 2;
            x + y
        });
    }

    #[test]
    fn block_with_typed_immutable_variables_2() {
        test_block!({
            let x: i32 = 1;
            let y = 2;
            x + y
        });
    }

    #[test]
    #[allow(unused)]
    fn block_with_shadowing() {
        test_block!({
            let a: i32 = 1;
            let b: i32 = 2;
            let a: i32 = 3;
            let b: i32 = 4;
            a + b
        });
    }

    #[test]
    #[allow(unused)]
    fn block_assign_to_mutable() {
        test_block!({
            let mut a: i32 = 1;
            a = 1 + 2;
            a
        });
    }

    #[test]
    fn block_assign_to_mutable_with_if_then_else() {
        test_block!({
            let mut a: i32 = 1;
            a = if a > 0 { a + 1 } else { a - 2 };
            a
        });
    }

    #[test]
    fn block_assign_to_mutable_array() {
        test_block!({
            let mut a: [i32; 3] = [1, 2, 3];
            a[0] = 4;
            a[1] = 5;
            a[2] = 6;
            a[0] + a[1] + a[2]
        });
    }

    #[test]
    fn block_return_on_array_assignment() {
        test_block!({
            let mut a: [i32; 3] = [1, 2, 3];
            a[0] = 4;
            a[1] = 5;
            a[2] = 6
        });
    }

    #[test]
    fn block_array_alternative_def() {
        test_block!({
            let mut a = [6; 2]; // a == [6, 6]
            a[0] = 4;
            a[1]
        });
    }

    #[test]
    #[ignore = "Not yet implemented. Must use literals not expr for now."]
    fn block_array_alternative_def_with_expr() {
        test_block!({
            // It can only use constants, not variables during expression.
            let mut a = [5+1; 10/4]; // This should be [6; 2], so a == [6, 6]
            a[0] = 4;
            a[1]
        });
    }

    #[test]
    #[allow(unused)]
    fn block_if_then_else_shadowing() {
        test_block!({
            let a: i32 = 1 + 2; // a == 3
            let mut a: i32 = 2 + a; // a == 5
            if true {
                a = a - 1; // outer a == 4
                let mut a: i32 = 0; // inner a == 0
                a = a + 1 // inner a == 1
            } else {
                a = a - 1
            };
            a // a == 4
        });
    }

    #[test]
    fn block_defining_and_calling_foo() {
        test_block!({
            fn foo() {}
            foo()
        });
    }

    #[test]
    fn block_defining_and_calling_bar() {
        test_block!({
            fn bar() -> i32 {
                47
            }
            bar()
        });
    }

    #[test]
    fn block_defining_and_calling_add_with_literals() {
        test_block!({
            fn add(a: i32, b: i32) -> i32 {
                a + b
            }
            add(1, 2)
        });
    }

    #[test]
    fn block_defining_and_calling_add_with_vars() {
        test_block!({
                assert_eq!(x, 1);
                assert_eq!(y, 2);
                assert_eq!(add(x, y), 3);
            },
            fn add(a: i32, b: i32) -> i32 {
                a + b
            },
            let x: i32 = 1,
            let y: i32 = 2;
            add(x, y)
        );
    }

    #[test]
    fn block_calling_foo_before_definition() {
        test_block!({
            foo();
            fn foo() {}
        });
    }

    #[test]
    fn block_calling_bar_before_definition() {
        test_block!({
            let x = bar();
            fn bar() -> i32 {
                47
            }
            x
        });
    }

    #[test]
    #[allow(unused)]
    fn late_initialization() {
        test_block!({
            let x;
            let y;
            if true {
                x = 1;
                y = true;
            } else {
                x = 0;
                y = false;
            }
        });
    }

    /// This one is a bit surprising. Seems to be very similar to `late_init_in_ifelse_fail()` in
    /// `type_check_fail.rs`, but Rust is happy with this one.
    //#[ignore = "weird"]
    #[test]
    #[allow(unused)]
    fn late_init_in_ifelse_ok() {
        test_block!({
            let x;
            let y;
            if true {
                x = 1;
                if true {
                    y = true;
                } else {
                    y = false;
                }
            } else {
                x = 0;
                if true {
                    y = true;
                } else {
                    y = false;
                }
            }
        });
    }

    /// Uses if / else if / else shorthand.
    #[test]
    fn signum() {
        test_block!({
            // Here's a block of tests that test the code below.
            assert_eq!(a, 1);
            assert_eq!(b, -1);
            assert_eq!(c, 0);
            },
            // Here is one statement.
            fn signum(x: i32) -> i32 {
                if x < 0 {
                    -1
                } else if x > 0 {
                    1
                } else {
                    0
                }
            },
            // Another statement.
            let a = signum(4),
            let b = signum(-5),
            // Last statement. (Note the ;)
            let c = signum(0);
            // Finally, the last expression of the block.
            a + b + c
        );
    }

    /// Simplest version of the GCD function.
    /// - Functions defined before they are used.
    /// - No references.
    /// - No recursion.
    /// - No shadowing (neither variables nor functions).
    #[test]
    fn gcd_easy() {
        test_block!({
                // Tests
                assert_eq!(min(3, 7), 3);
                assert_eq!(max(3, 7), 7);
                assert_eq!(abs(3), 3);
                assert_eq!(abs(-3), 3);
                assert_eq!(gcd(5, 7), 1);
                assert_eq!(gcd(12, 18), 6);
            },
            fn min(x: i32, y: i32) -> i32 {
                if x < y {
                    x
                } else {
                    y
                }
            },
            fn max(x: i32, y: i32) -> i32 {
                if x > y {
                    x
                } else {
                    y
                }
            },
            fn abs(x: i32) -> i32 {
                if x < 0 {
                    0 - x   // no -x unary op
                } else {
                    x
                }
            },
            fn gcd(mut x: i32, mut y: i32) -> i32 {
                x = abs(x);
                y = abs(y);
                let mut cont = true;
                while cont {
                    //(x, y) = (min(x, y), max(x, y));
                    let tmp_x = x;
                    let tmp_y = y;
                    x = min(tmp_x, tmp_y);
                    y = max(tmp_x, tmp_y);
                    if (x == y) || (x == 1) {
                        cont = false; // return x
                    };
                    y = y - x;
                }
                x
            };
            gcd(12, 18)
        );
    }

    /// Slightly more difficult version of the GCD function.
    /// - Calls functions before they are defined.
    /// - Defines nested functions.
    /// - Does not rely on parsing with proper precedence.
    #[test]
    fn gcd_medium() {
        test_block!({
                assert_eq!(gcd(-5, 7), 1);
                assert_eq!(gcd(12, -18), 6);
            },
            fn gcd(mut x: i32, mut y: i32) -> i32 {
                x = abs(x);
                y = abs(y);
                let mut cont = true;
                while cont {
                    //(x, y) = (min(x, y), max(x, y));
                    let tmp_x = x;
                    let tmp_y = y;
                    x = min(tmp_x, tmp_y);
                    y = max(tmp_x, tmp_y);
                    if (x == y) || (x == 1) {
                        cont = false; // return x
                    };
                    y = y - x;
                }
                fn min(x: i32, y: i32) -> i32 {
                    if x < y {
                        x
                    } else {
                        y
                    }
                }
                fn max(x: i32, y: i32) -> i32 {
                    if x > y {
                        x
                    } else {
                        y
                    }
                }
                fn abs(x: i32) -> i32 {
                    if x < 0 {
                        0 - x   // no -x unary op
                    } else {
                        x
                    }
                }
                x
            };
            gcd(12, 18)
        );
    }

    /// Even more difficult version of the GCD function.
    /// - Calls functions before they are defined.
    /// - Defines nested functions.
    /// - Shadows an outer function.
    /// - Calls the shadowing function recursively.
    /// - Uses unary operator `-x`.
    /// - Shadows variables.
    /// - Relies on expressions being parsed properly (with correct precedence).
    #[test]
    fn gcd_hard() {
        test_block!({
                assert_eq!(gcd(-5, 7), 1);
                assert_eq!(gcd(36, 18), 18);
                assert_eq!(gcd(12, -18), 6);
            },
            fn gcd(x: i32, y: i32) -> i32 {
                let result = gcd(abs(x), abs(y));
                fn gcd(x: i32, y: i32) -> i32 {
                    if x == y || x == 1 {
                        x
                    } else {
                        let tmp_x = x;
                        let tmp_y = y;
                        let x = min(tmp_x, tmp_y);
                        let y = max(tmp_x, tmp_y);
                        gcd(x, y - x)
                    }
                }
                fn min(x: i32, y: i32) -> i32 {
                    if x < y {
                        x
                    } else {
                        y
                    }
                }
                fn max(x: i32, y: i32) -> i32 {
                    if x > y {
                        x
                    } else {
                        y
                    }
                }
                fn abs(x: i32) -> i32 {
                    if x < 0 {
                        -x
                    } else {
                        x
                    }
                }
                result
            };
            gcd(12, 18)
        );
    }
}

//TODO: Implement references?
/// These tests all require that references are implemented.
#[cfg(test)]
mod ref_deref {
    use super::*;

    #[ignore = "Requires references to be implemented"]
    #[test]
    fn init_with_ref_to_lit_infer_type() {
        test_block!(
            {
                let b = &3;
                *b
            },
            3
        );
    }

    #[ignore = "Requires references to be implemented"]
    #[test]
    fn init_with_ref_to_lit_with_type() {
        test_block!(
            {
                let b: &i32 = &3;
                *b
            },
            3
        );
    }

    #[ignore = "Requires references to be implemented"]
    #[test]
    fn init_with_ref_to_expr_and_deref() {
        test_block!(
            {
                let b = &(1 + 2);
                *b
            },
            3
        );
    }

    #[ignore = "Requires references to be implemented"]
    #[test]
    fn init_ref_late_with_type() {
        test_block!(
            {
                let b: &i32;
                b = &3;
                *b
            },
            3
        );
    }

    #[ignore = "Requires references to be implemented"]
    #[test]
    fn init_ref_late_infer_type() {
        test_block!(
            {
                let b;
                b = &3;
                *b
            },
            3
        );
    }

    #[ignore = "Requires references to be implemented"]
    #[test]
    fn init_with_ref_to_var() {
        test_block!(
            {
                let a = 3;
                let b = &a;
                *b
            },
            3
        );
    }

    #[ignore = "Requires references to be implemented"]
    #[test]
    fn init_with_deref_ref_to_lit() {
        test_block!(
            {
                let b = *&3;
                b
            },
            3
        );
    }

    #[ignore = "Requires references to be implemented"]
    #[test]
    fn init_with_deref_ref_to_var() {
        test_block!(
            {
                let a = 3;
                let b = *&a;
                *&b
            },
            3
        );
    }

    #[ignore = "Requires references to be implemented"]
    #[test]
    fn reassign_mut_ref_lit() {
        test_block!(
            {
                let b = &mut 7;
                *b = 3;
                *b
            },
            3
        );
    }

    #[ignore = "Requires references to be implemented"]
    #[test]
    fn reassign_mut_ref_var() {
        test_block!(
            {
                let mut a = 7;
                let b = &mut a;
                *b = 3;
                *b
            },
            3
        );
    }

    /// Most difficult version of the GCD function.
    /// - Calls functions before they are defined.
    /// - Defines nested functions.
    /// - Extensive use of mutable references.
    /// - Uses unary operator `-x`.
    /// - Calls function with (relatively) complex expressions.
    /// - Relies on expressions being parsed properly (with correct precedence).
    /// NOTE: This requires full support for mutable references.
    #[ignore = "Requires references to be implemented"]
    #[test]
    fn gcd_harder() {
        test_block!({
                assert_eq!(gcd(-5, 7), 1);
                assert_eq!(gcd(12, -18), 6);
                assert_eq!(gcd(36, 18), 18);
            },
            fn gcd(mut x: i32, mut y: i32) -> i32 {
                abs(&mut x);
                abs(&mut y);
                let mut cont = true;
                while cont {
                    //(x, y) = (min(x, y), max(x, y));
                    min_max_swap(&mut x, &mut y);
                    cont = !(x == y || x == 1); // break
                    subtract(&mut y, x);
                }
                /// Subtract `x` from `y` *in place*.
                fn subtract(y: &mut i32, x: i32) {
                    *y = *y - x;
                }
                fn swap(x: &mut i32, y: &mut i32) {
                    let tmp = *x;
                    *x = *y;
                    *y = tmp;
                }
                /// After the function returns, x will contain the smaller value,
                /// and y will contain the larger value.
                /// Roughly equivalent to `(x, y) = (min(x, y), max(x, y))`.
                fn min_max_swap(x: &mut i32, y: &mut i32) {
                    if *x > *y {
                        swap(x, y);
                    }
                }
                /// After abs returns, the value x references will be nonnegative.
                fn abs(x: &mut i32) {
                    if *x < 0 {
                        *x = -*x;
                    }
                }
                x   // return x
            },
            let x = 12,
            let y = 18;
            gcd(-x, (y+y)/-2) == gcd(x, y) && gcd(2*x, -2*y) == 2 * gcd(x, y)
        );
    }

    #[ignore = "Requires references to be implemented"]
    #[test]
    fn crazy_ref_deref() {
        test_expr!(*&if !*&!true { 1 } else { 2 });
        test_expr!(*&if !*&!true { *&1 + 2 } else { *&(1 + *&2) });
        test_expr!(**&if !*&!true { &1 } else { &2 });
        test_expr!(*&if *&true { *&1 } else { *&2 });
        test_expr!(***&&&if **&&!!true && !**&&!true || !!**&&false {
            *&!(!!(false || (!false)))
        } else {
            !*(*&(&!((true || false) && true)))
        });
        test_expr!(*&if !**(&&!(3 * (1 + 2) < *&(3 * 1 + 2))) {
            *&-2 * ((*&1 + **&&2) * *&3 + 4)
        } else {
            *&-2 * ((*&1 + **&&2) * *&3 + -4)
        });
    }

    #[ignore = "Requires references to be implemented"]
    #[test]
    fn if_ref_deref() {
        test_expr!(*if true { &1 } else { &2 });
        test_expr!(*&if true { 1 } else { 2 });
    }
}
