use rnr::ast::{Block, Expr, Prog, Type};
use rnr::common::parse_type;
use rnr::type_check::TypeVal;

#[cfg(test)]
mod test_tvm {
    use super::*;

    #[test]
    fn test_expr() {
        let v = parse_type::<Expr, TypeVal>(
            "        
            1 + 1
            ",
        );

        assert_eq!(v.unwrap().get_initialized_type(), Type::I32);
    }

    #[test]
    fn test_later_init() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                let a: i32;
                a = 1;
                a
            }
            ",
        );

        assert_eq!(v.unwrap().get_initialized_type(), Type::I32);
    }

    #[test]
    fn test_bool() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                let a = true && false;
                a
            }
            ",
        );

        assert_eq!(v.unwrap().get_initialized_type(), Type::Bool);
    }

    #[test]
    fn test_bool_bang2() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                let a = (!true) && false;
                a
            }
            ",
        );

        assert_eq!(v.unwrap().get_initialized_type(), Type::Bool);
    }

    #[test]
    fn test_bool_bang() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                let a = true && false;
                !a
            }
            ",
        );

        assert_eq!(v.unwrap().get_initialized_type(), Type::Bool);
    }

    #[test]
    fn test_block_let() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                let a: i32 = 1;
                let b: i32 = 2;

                a + b
            }",
        );
        assert_eq!(v.unwrap().get_initialized_type(), Type::I32);
    }

    #[test]
    fn test_block_let_shadow() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                let a: i32 = 1;
                let b: i32 = 2;
                let a: i32 = 3;
                let b: i32 = 4;

                a + b
            }",
        );
        assert_eq!(v.unwrap().get_initialized_type(), Type::I32);
    }

    #[test]
    fn test_local_block() {
        let v = parse_type::<Block, TypeVal>(
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

        assert_eq!(v.unwrap().get_initialized_type(), Type::I32);
    }

    #[test]
    fn test_block_assign() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                let mut a: i32 = 1;
                a = a + 2;
                a
            }",
        );
        assert_eq!(v.unwrap().get_initialized_type(), Type::I32);
    }

    #[test]
    fn test_expr_if_then_else() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                let mut a: i32 = 1;
                a = if a > 0 { a + 1 } else { a - 2 };
                a
            }",
        );

        assert_eq!(v.unwrap().get_initialized_type(), Type::I32);
    }

    #[test]
    fn test_while() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                let mut a = 2;
                let mut b = 0;
                while a > 0 {
                    a = a - 1;
                    b = b + 1;
                }
                b
            }
            ",
        );

        assert_eq!(v.unwrap().get_initialized_type(), Type::I32);
    }

    #[test]
    fn test_array() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                let a = [1, 2, 3];
                let b: [i32; 3] = [4, 5, 6];
                let c = [7; 10];
                a[1] + b[2] + c[5] // 2 + 6 + 10
            }
            ",
        );

        assert_eq!(v.unwrap().get_initialized_type(), Type::I32);
    }

    #[test]
    fn test_array_assignment() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                let mut a = [1, 2, 3];
                a[1] = 4;
                a[1]
            }
            ",
        );

        assert_eq!(v.unwrap().get_initialized_type(), Type::I32);
    }

    #[test]
    fn test_prog1() {
        let v = parse_type::<Prog, TypeVal>(
            "
            fn main() {
                let a = 1;
                a;
            }
            ",
        );

        assert_eq!(v.unwrap().get_initialized_type(), Type::Unit);
    }

    #[test]
    fn test_prog2() {
        let v = parse_type::<Prog, TypeVal>(
            "
            fn dummy() -> i32 {
                1
            }

            fn main() {
                let a = 1;
                a + dummy();
            }
            ",
        );

        assert_eq!(v.unwrap().get_initialized_type(), Type::Unit);
    }

    #[test]
    fn test_local_fn1() {
        let v = parse_type::<Prog, TypeVal>(
            "
            fn main() {

                fn f(i: i32, j: i32) -> i32 {
                    i + j
                }

                fn g() -> i32 {
                    f(3, 4) // 7
                }

                let a = f(1, 2); // a == 3
                a + g(); // 3 + 7 == 10
            }
            ",
        );

        assert_eq!(v.unwrap().get_initialized_type(), Type::Unit);
    }

    #[test]
    fn test_println() {
        let v = parse_type::<Prog, TypeVal>(
            "
            fn main() {
                fn f(i: i32, j: i32) -> i32 {
                    i + j
                }
                let a = f(1, 2);
                println!(\"a = {} and another a = {}\", a, a);
                println!(\"But also some random test to be sure it is {{correctly}} implemented...\"); // escaped brackets
            }
            ",
        );

        assert_eq!(v.unwrap().get_initialized_type(), Type::Unit);
    }

    #[test]
    fn test_fn_shadowing() {
        let v = parse_type::<Prog, TypeVal>(
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
                a + g(); // 3 + -1 == 2
            }
            ",
        );

        assert_eq!(v.unwrap().get_initialized_type(), Type::Unit);
    }

    #[test]
    fn test_call_before_def() {
        let v = parse_type::<Prog, TypeVal>(
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
                a + g(); // 3 + -1 == 2
            }
            ",
        );

        assert_eq!(v.unwrap().get_initialized_type(), Type::Unit);
    }

    #[test]
    fn test_recursion() {
        let v = parse_type::<Prog, TypeVal>(
            "
            fn main() {
                fn fact(n: i32) -> i32 {
                    if n == 0 {
                        1
                    } else {
                        n * fact(n - 1)
                    }
                }

                let a = fact(5);
            }
            ",
        );

        assert_eq!(v.unwrap().get_initialized_type(), Type::Unit);
    }

    #[test]
    fn test_check_if_then_else_shadowing() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                let a: i32 = 1 + 2; // a == 3
                let mut a: i32 = 2 + a; // a == 5
                if true {
                    a = a - 1;          // outer a == 4
                    let mut a: i32 = 0; // inner a == 0
                    a = a + 1           // inner a == 1
                } else {
                    a = a - 1
                };
                a;   // a == 4
            }
            ",
        );

        assert_eq!(v.unwrap().get_initialized_type(), Type::Unit);
    }

    //? --------------------------- Failing tests ---------------------------

    #[test]
    #[should_panic]
    fn test_var_late_init_wrong_type() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                let a: i32;
                a = true; // Should crash (wrong type)
            }
            ",
        );

        println!(
            "Did not panic... v = {:?}",
            v.unwrap().get_initialized_type()
        );
    }

    #[test]
    #[should_panic]
    fn test_immutable_var() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                let a = 3;
                a = 2; // Should crash (a is immutable)
                a
            }
            ",
        );

        println!(
            "Did not panic... v = {:?}",
            v.unwrap().get_initialized_type()
        );
    }

    #[test]
    #[should_panic]
    fn test_invalid_binop() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                let a = 3;
                let b = true;
                a + b
            }
            ",
        );

        println!(
            "Did not panic... v = {:?}",
            v.unwrap().get_initialized_type()
        );
    }

    #[test]
    #[should_panic]
    fn test_invalid_unop() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                let a = 3;
                !a
            }
            ",
        );

        println!(
            "Did not panic... v = {:?}",
            v.unwrap().get_initialized_type()
        );
    }

    #[test]
    #[should_panic]
    fn test_immutable_array() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                let a = [1, 2, 3];
                a[1] = 4;  // Should crash (a is immutable)
            }
            ",
        );

        println!(
            "Did not panic... v = {:?}",
            v.unwrap().get_initialized_type()
        );
    }

    #[test]
    #[should_panic]
    fn test_invalid_size_array() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                let a: [i32; 2] = [1, 2, 3]; // Should crash (wrong size)
            }
            ",
        );

        println!(
            "Did not panic... v = {:?}",
            v.unwrap().get_initialized_type()
        );
    }

    #[test]
    // #[should_panic(expected = "variable 'a' not found")] // Specifying a panic message is too restrictive and makes the test fragile
    #[should_panic]
    fn test_local_scope() {
        let v = parse_type::<Block, TypeVal>(
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
            v.unwrap().get_initialized_type()
        );
    }

    #[test]
    #[should_panic]
    fn test_invalid_function_def1() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                fn f(i: i32, j: i32) -> bool { // Should crash (return type mismatch)
                    i + j
                }
            }
            ",
        );

        println!(
            "Did not panic... v = {:?}",
            v.unwrap().get_initialized_type()
        );
    }

    #[test]
    #[should_panic]
    fn test_invalid_function_def2() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                fn f(i: i32, j: i32) { // Should crash (return type mismatch -> should be unit)
                    i + j
                }
            }
            ",
        );

        println!(
            "Did not panic... v = {:?}",
            v.unwrap().get_initialized_type()
        );
    }

    #[test]
    #[should_panic]
    fn test_function_missing_args() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                fn f(i: i32, j: i32) -> i32 {
                    i + j
                }

                f(1)   // Should crash (missing argument)
            }
            ",
        );

        println!(
            "Did not panic... v = {:?}",
            v.unwrap().get_initialized_type()
        );
    }

    #[test]
    #[should_panic]
    fn test_function_too_many_args() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                fn f(i: i32, j: i32) -> i32 {
                    i + j
                }

                f(1, 2, 3)   // Should crash (too many arguments)
            }
            ",
        );

        println!(
            "Did not panic... v = {:?}",
            v.unwrap().get_initialized_type()
        );
    }

    #[test]
    #[should_panic]
    fn test_function_invalid_arg_types() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                fn f(i: i32, j: i32) -> i32 {
                    i + j
                }

                f(1, true)   // Should crash (invalid argument types)
            }
            ",
        );

        println!(
            "Did not panic... v = {:?}",
            v.unwrap().get_initialized_type()
        );
    }

    #[test]
    #[should_panic]
    fn test_invalid_println_call1() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                println!(\"{} {:?}\", 1, 2, 3); // Should crash (too many arguments)
            }
            ",
        );

        assert_eq!(v.unwrap().get_initialized_type(), Type::Unit);
    }

    #[test]
    #[should_panic]
    fn test_invalid_println_call2() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                println!(\"{}\"); // Should crash (not enough arguments)
            }
            ",
        );

        assert_eq!(v.unwrap().get_initialized_type(), Type::Unit);
    }

    #[test]
    #[should_panic]
    fn test_invalid_println_call3() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                println!(); // Should crash (not enough arguments)
            }
            ",
        );

        assert_eq!(v.unwrap().get_initialized_type(), Type::Unit);
    }

    #[test]
    #[should_panic]
    fn test_invalid_println_call4() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                let a: i32;
                println!(\"a is {}\", a); // Should crash (uninitialized variable)
            }
            ",
        );

        assert_eq!(v.unwrap().get_initialized_type(), Type::Unit);
    }

    #[test]
    #[should_panic]
    fn test_invalid_println_call5() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                println!(\"{{{}}}\"); // Should crash (not enough arguments: need 1 more than the string)
            }
            ",
        );

        assert_eq!(v.unwrap().get_initialized_type(), Type::Unit);
    }

    #[test]
    // #[should_panic(expected = "function 'add' not found")] // Specifying a panic message is too restrictive and makes the test fragile
    #[should_panic]
    fn test_local_scope_func() {
        let v = parse_type::<Prog, TypeVal>(
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
            v.unwrap().get_initialized_type()
        );
    }

    #[test]
    #[should_panic]
    fn test_while_loop_with_invalid_condition() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                while 1 {
                    1;
                }
            }
            ",
        );

        println!(
            "Did not panic... v = {:?}",
            v.unwrap().get_initialized_type()
        );
    }

    #[test]
    #[should_panic]
    fn test_while_loop_with_return_type() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                while true {
                    1
                }
            }
            ",
        );

        println!(
            "Did not panic... v = {:?}",
            v.unwrap().get_initialized_type()
        );
    }

    #[test]
    #[should_panic]
    fn test_if_with_invalid_condition() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                if 1 {
                    1;
                } else {
                    2;
                }
            }
            ",
        );

        println!(
            "Did not panic... v = {:?}",
            v.unwrap().get_initialized_type()
        );
    }

    #[test]
    #[should_panic]
    fn test_if_with_mismatching_blocks_types() {
        let v = parse_type::<Block, TypeVal>(
            "
            {
                if true {
                    1
                } else {
                    true
                }
            }
            ",
        );

        println!(
            "Did not panic... v = {:?}",
            v.unwrap().get_initialized_type()
        );
    }

    #[test]
    #[should_panic]
    fn test_invalid_main_type() {
        let v = parse_type::<Prog, TypeVal>(
            "
            fn main() -> i32 {
                1
            }
            ",
        );

        println!(
            "Did not panic... v = {:?}",
            v.unwrap().get_initialized_type()
        );
    }

    #[test]
    #[should_panic]
    fn test_invalid_main_args() {
        let v = parse_type::<Prog, TypeVal>(
            "
            fn main(x: i32) {
                let y = x + 1;
            }
            ",
        );

        println!(
            "Did not panic... v = {:?}",
            v.unwrap().get_initialized_type()
        );
    }
}
