use rnr::ast::{Block, Expr, Literal, Prog};
use rnr::common::parse_test;
use rnr::vm::Val;

#[cfg(test)]
mod test_vm {
    use super::*;

    #[test]
    fn test_expr() {
        let v = parse_test::<Expr, Val>(
            "        
            1 + 1
            ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 2);
    }

    #[test]
    fn test_bool() {
        let v = parse_test::<Block, Val>(
            "
            {
                let a = true && false;
                a
            }
            ",
        );

        assert!(!v.unwrap().get_bool().unwrap());
    }

    #[test]
    fn test_bool_bang2() {
        let v = parse_test::<Block, Val>(
            "
            {
                let a = (!true) && false;
                a
            }
            ",
        );

        assert!(!v.unwrap().get_bool().unwrap());
    }

    #[test]
    fn test_bool_bang() {
        let v = parse_test::<Block, Val>(
            "
            {
                let a = true && !false;
                a
            }
            ",
        );

        assert!(v.unwrap().get_bool().unwrap());
    }

    #[test]
    fn test_block_let() {
        let v = parse_test::<Block, Val>(
            "
            {
                let a: i32 = 1;
                let b: i32 = 2;

                a + b
            }",
        );
        assert_eq!(v.unwrap().get_int().unwrap(), 3);
    }

    #[test]
    fn test_block_let_shadow() {
        let v = parse_test::<Block, Val>(
            "
            {
                let a: i32 = 1;
                let b: i32 = 2;
                let a: i32 = 3;
                let b: i32 = 4;

                a + b
            }",
        );
        assert_eq!(v.unwrap().get_int().unwrap(), 7);
    }

    #[test]
    fn test_local_block() {
        let v = parse_test::<Block, Val>(
            "
            {
                let a = 1;
                let b = {
                    let b = a;
                    b * 2
                };

                b
            }
            ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 2);
    }

    #[test]
    fn test_block_assign() {
        let v = parse_test::<Block, Val>(
            "
            {
                let mut a: i32 = 1;
                a = a + 2;
                a
            }",
        );
        assert_eq!(v.unwrap().get_int().unwrap(), 3);
    }

    #[test]
    fn test_expr_if_then_else() {
        let v = parse_test::<Block, Val>(
            "
            {
                let mut a: i32 = 1;
                a = if a > 0 { a + 1 } else { a - 2 };
                a
            }",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 2);
    }

    #[test]
    fn test_while() {
        let v = parse_test::<Block, Val>(
            "
            {
                let a = 2;
                let b = 0;
                while a > 0 {
                    a = a - 1;
                    b = b + 1;
                }
                b
            }
            ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 2);
    }

    #[test]
    fn test_array() {
        let v = parse_test::<Block, Val>(
            "
            {
                let a = [1, 2, 3];
                let b: [i32; 3] = [4, 5, 6]; //TODO: Add support for length check (will be done by type checker)
                let c = [7; 10];
                a[1] + b[2] + c[5] // 2 + 6 + 10
            }
            ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 18);
    }

    #[test]
    fn test_array_assignment() {
        let v = parse_test::<Block, Val>(
            "
            {
                let mut a = [1, 2, 3];
                a[1] = 4;
                a[1]
            }
            ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 4);
    }

    #[test]
    fn test_prog1() {
        let v = parse_test::<Prog, Val>(
            "
            fn main() {
                let a = 1;
                a
            }
            ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 1);
    }

    #[test]
    fn test_prog2() {
        let v = parse_test::<Prog, Val>(
            "
            fn dummy() -> i32 {
                1
            }

            fn main() {
                let a = 1;
                a + dummy()
            }
            ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 2);
    }

    #[test]
    fn test_local_fn1() {
        let v = parse_test::<Prog, Val>(
            "
            fn main() {

                fn f(i: i32, j: i32) -> i32 {
                    i + j
                }

                fn g() -> i32 {
                    f(3, 4) // 7
                }

                let a = f(1, 2); // a == 3
                a + g() // 3 + 7 == 10
            }
            ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 10);
    }

    #[test]
    fn test_println() {
        let v = parse_test::<Prog, Val>(
            "
            fn main() {
                fn f(i: i32, j: i32) -> i32 {
                    i + j
                }
                let a = f(1, 2);
                println!(\"a = {} and another a = {}\", a, a);
                println!(\"But also some random test to be sure it is correctly implemented...\");
            }
            ",
        );

        assert_eq!(v.unwrap(), Val::Lit(Literal::Unit));
    }

    #[test]
    fn test_fn_shadowing() {
        let v = parse_test::<Prog, Val>(
            "
            fn main() {

                fn f(i: i32, j: i32) -> i32 {
                    i + j
                }

                let a = f(1, 2); // a == 3

                fn g() -> i32 {
                    fn f(i: i32, j: i32) -> i32 {
                        i - j
                    }
                    f(3, 4) // -1
                }
                a + g() // 3 + -1 == 2
            }
            ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 2);
    }

    #[test]
    fn test_call_before_def() {
        let v = parse_test::<Prog, Val>(
            "
            fn main() {

                let a = f(1, 2); // a == 3

                fn f(i: i32, j: i32) -> i32 {
                    i + j
                }

                fn g() -> i32 {
                    fn f(i: i32, j: i32) -> i32 {
                        i - j
                    }
                    f(3, 4) // -1
                }
                a + g() // 3 + -1 == 2
            }
            ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 2);
    }

    #[test]
    fn test_recursion() {
        let v = parse_test::<Prog, Val>(
            "
            fn main() {
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
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 120);
    }

    #[test]
    fn test_check_if_then_else_shadowing() {
        let v = parse_test::<Block, Val>(
            "
            {
                let a: i32 = 1 + 2; // a == 3
                let a: i32 = 2 + a; // a == 5
                if true {
                    a = a - 1;      // outer a == 4
                    let a: i32 = 0; // inner a == 0
                    a = a + 1       // inner a == 1
                } else {
                    a = a - 1
                };
                a   // a == 4
            }
            ",
        );

        assert_eq!(v.unwrap().get_int().unwrap(), 4);
    }

    #[test]
    #[should_panic(expected = "variable 'a' not found")]
    fn test_local_scope() {
        let v = parse_test::<Block, Val>(
            "
            {
                let b = {
                    let mut a: i32 = 0; // a == 0
                    a = a + 1       // a == 1
                };
                a   // Should crash (a not defined here)
            }
            ",
        );

        println!(
            "Did not panic... v = {:?}",
            v.unwrap().get_literal().unwrap()
        );
    }

    #[test]
    #[should_panic(expected = "function 'add' not found")]
    fn test_local_scope_func() {
        let v = parse_test::<Prog, Val>(
            "
            fn testing() -> i32 {
                fn add(a: i32, b: i32) -> i32 {
                    a + b
                }

                fn plus_one(mut a: i32) -> i32 {
                    a = add(a, 1);
                    a
                }

                let b = {
                    let mut a: i32 = 0; // a == 0
                    a = plus_one(a)     // a == 1
                };
                add(b, 3) // b == 4
            }

            fn main() {
                fn dummy() -> i32 {
                    fn bool_to_int(b: bool) -> i32 {
                        if b {
                            1
                        } else {
                            0
                        }
                    }
                    bool_to_int(true)
                }

                let a = testing();
                let b = add(a, 1); // Should crash (add not defined here)
                b
            }
            ",
        );

        println!(
            "Did not panic... val = {:?}",
            v.unwrap().get_literal().unwrap()
        );
    }
}
