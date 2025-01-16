use rnr::ast::{Block, Expr, Prog, Type};
use rnr::common::{parse, parse_check_type, parse_type};
use rnr::test_util::assert_type_check;
use rnr::type_check::TypeVal;

#[cfg(test)]
mod test_tvm {
    use super::*;

    #[cfg(test)]
    mod successful_tests {
        use super::*;

        #[cfg(test)]
        mod testing_return_types {
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
            fn simple_definition() {
                let v = parse_type::<Block, TypeVal>(
                    "
                    {
                        let a = 1;
                        a
                    }
                    ",
                );

                assert_eq!(v.unwrap().get_initialized_type(), Type::I32);
            }

            #[test]
            fn simple_definition_any() {
                let v = parse_type::<Block, TypeVal>(
                    "
                    {
                        let a: _ = 1;
                        a
                    }
                    ",
                );

                assert_eq!(v.unwrap().get_initialized_type(), Type::I32);
            }

            #[test]
            fn test_late_init() {
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
            fn test_late_init_any() {
                let v = parse_type::<Block, TypeVal>(
                    "
                    {
                        let a: _;
                        a = 1;
                        a
                    }
                    ",
                );

                assert_eq!(v.unwrap().get_initialized_type(), Type::I32);
            }

            #[test]
            fn test_late_init_no_type() {
                let v = parse_type::<Block, TypeVal>(
                    "
                    {
                        let a;
                        a = true;
                        a
                    }
                    ",
                );

                assert_eq!(v.unwrap().get_initialized_type(), Type::Bool);
            }

            #[test]
            fn test_if_late_init() {
                let v = parse_type::<Block, TypeVal>(
                    "
                    {
                        let a: i32;
                        if true {
                            a = 1;
                        } else {
                            a = 2;
                        }
                        a
                    }
                    ",
                );

                assert_eq!(v.unwrap().get_initialized_type(), Type::I32);
            }

            #[test]
            fn test_if_late_init_any() {
                let v = parse_type::<Block, TypeVal>(
                    "
                    {
                        let a: _;
                        if true {
                            a = 1;
                        } else {
                            a = 2;
                        }
                        a
                    }
                    ",
                );

                assert_eq!(v.unwrap().get_initialized_type(), Type::I32);
            }

            #[test]
            fn test_if_late_init_more_complex() {
                let v = parse_type::<Block, TypeVal>(
                    "
                    {
                        let a;
                        let b;
                        if true {
                            a = 1;
                            if true {
                                b = 2;
                            } else {
                                b = 3;
                            }
                        } else {
                            a = 2;
                            if true {
                                b = 4;
                            } else {
                                b = 5;
                            }
                        }
                        b
                    }
                    ",
                );

                assert_eq!(v.unwrap().get_initialized_type(), Type::I32);
            }

            #[test]
            fn test_if_late_init_more_complex_any() {
                let v = parse_type::<Block, TypeVal>(
                    "
                    {
                        let a: _;
                        let b: _;
                        if true {
                            a = 1;
                            if true {
                                b = 2;
                            } else {
                                b = 3;
                            }
                        } else {
                            a = 2;
                            if true {
                                b = 4;
                            } else {
                                b = 5;
                            }
                        }
                        b
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
            fn test_empty_array_type() {
                let v = parse_type::<Block, TypeVal>(
                    "
                    {
                        let a: [i32; 0] = [];
                        a
                    }
                    ",
                );

                let arr_type = Type::Array(Box::new(Type::I32), 0);
                assert_eq!(v.unwrap().get_initialized_type(), arr_type);
            }

            #[test]
            fn test_empty_array_type_late_init() {
                let v = parse_type::<Block, TypeVal>(
                    "
                    {
                        let a: [i32; 0];
                        a = [];
                        a
                    }
                    ",
                );

                let arr_type = Type::Array(Box::new(Type::I32), 0);
                assert_eq!(v.unwrap().get_initialized_type(), arr_type);
            }

            #[test]
            fn test_array_inner_type_is_any() {
                let v = parse_type::<Block, TypeVal>(
                    "
                    {
                        let a: [_; 3] = [1, 2, 3];
                        a
                    }
                    ",
                );

                let arr_type = Type::Array(Box::new(Type::I32), 3);
                assert_eq!(v.unwrap().get_initialized_type(), arr_type);
            }

            #[test]
            fn test_array_inner_type_is_any_late_init() {
                let v = parse_type::<Block, TypeVal>(
                    "
                    {
                        let a: [_; 3];
                        a = [1, 2, 3];
                        a
                    }
                    ",
                );

                let arr_type = Type::Array(Box::new(Type::I32), 3);
                assert_eq!(v.unwrap().get_initialized_type(), arr_type);
            }

            #[test]
            fn test_array_type_1() {
                let v = parse_type::<Block, TypeVal>(
                    "
                    {
                        let a = [1, 2, 3];
                        a
                    }
                    ",
                );

                let arr_type = Type::Array(Box::new(Type::I32), 3);
                assert_eq!(v.unwrap().get_initialized_type(), arr_type);
            }

            #[test]
            fn test_array_type_2() {
                let v = parse_type::<Block, TypeVal>(
                    "
                    {
                        let a = [[1, 2], [3, 4], [5, 6]];
                        a
                    }
                    ",
                );

                let arr_type = Type::Array(Box::new(Type::Array(Box::new(Type::I32), 2)), 3);
                assert_eq!(v.unwrap().get_initialized_type(), arr_type);
            }

            #[test]
            fn test_array_type_3() {
                let v = parse_type::<Block, TypeVal>(
                    "
                    {
                        let a = [
                            [
                                [true, false, true],
                                [false, false, false],
                            ],
                            [
                                [true, true, true],
                                [false, false, true],
                            ],
                        ];
                        a
                    }
                    ",
                );

                let arr_type = Type::Array(
                    Box::new(Type::Array(
                        Box::new(Type::Array(Box::new(Type::Bool), 3)),
                        2,
                    )),
                    2,
                );
                assert_eq!(v.unwrap().get_initialized_type(), arr_type);
            }

            #[test]
            fn test_array_get_1() {
                let v = parse_type::<Block, TypeVal>(
                    "
                    {
                        let a = [1, 2, 3];
                        a[0]
                    }
                    ",
                );

                let get_type = Type::I32;
                assert_eq!(v.unwrap().get_initialized_type(), get_type);
            }

            #[test]
            fn test_array_get_2() {
                let v = parse_type::<Block, TypeVal>(
                    "
                    {
                        let a = [[true, false, false], [false, false, false]];
                        a[0]
                    }
                    ",
                );

                let get_type = Type::Array(Box::new(Type::Bool), 3);
                assert_eq!(v.unwrap().get_initialized_type(), get_type);
            }

            #[test]
            fn test_array_get_3() {
                let v = parse_type::<Block, TypeVal>(
                    "
                    {
                        let a = [[true, false, false], [false, false, false]];
                        a[0][1]
                    }
                    ",
                );

                let get_type = Type::Bool;
                assert_eq!(v.unwrap().get_initialized_type(), get_type);
            }

            #[test]
            fn test_array_assignment_1() {
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
            fn test_array_assignment_2() {
                let v = parse_type::<Block, TypeVal>(
                    "
                    {
                        let mut a = [[1, 2], [3, 4], [5, 6]];
                        a[1][1] = 7;
                        a[2] = [8, 9];
                        a[2]
                    }
                    ",
                );

                assert_eq!(
                    v.unwrap().get_initialized_type(),
                    Type::Array(Box::new(Type::I32), 2)
                );
            }

            #[test]
            fn test_array_with_expr() {
                let v = parse_type::<Block, TypeVal>(
                    "
                    {
                        fn f(x: i32) -> i32 {
                            x * 2
                        }

                        let b = 2;
                        let c = f(b);
                        let a = [1, b, c];
                        a
                    }
                    ",
                );

                assert_eq!(
                    v.unwrap().get_initialized_type(),
                    Type::Array(Box::new(Type::I32), 3)
                );
            }

            #[test]
            fn test_array_with_expr_alt() {
                let v = parse_type::<Block, TypeVal>(
                    "
                    {
                        let b = 2;
                        let a = [b; 4];
                        a
                    }
                    ",
                );

                assert_eq!(
                    v.unwrap().get_initialized_type(),
                    Type::Array(Box::new(Type::I32), 4)
                );
            }
        }

        #[cfg(test)]
        mod testing_returned_ast {
            use rnr::common::CheckType;

            use super::*;

            #[test]
            fn test_simple_let() {
                let res: Block = parse(
                    "
                    {
                        let a = 1;
                    }
                    ",
                );
                let expected: Block = parse(
                    "
                    {
                        let a: i32 = 1;
                    }
                    ",
                );

                assert_type_check(&res, expected);
            }

            #[test]
            fn test_complex_let() {
                let res: Block = parse(
                    "
                    {
                        fn f(i: i32) -> bool {
                            i > 0
                        }

                        let a = if f(-1) {
                            f(1)
                        } else {
                            f(3)
                        };
                    }
                    ",
                );
                let expected: Block = parse(
                    "
                    {
                        fn f(i: i32) -> bool {
                            i > 0
                        }

                        let a: bool = if f(-1) {
                            f(1)
                        } else {
                            f(3)
                        };
                    }
                    ",
                );

                assert_type_check(&res, expected);
            }

            #[test]
            fn test_simple_late_assign() {
                let res: Block = parse(
                    "
                    {
                        let a;
                        a = 1;
                    }
                    ",
                );
                let expected: Block = parse(
                    "
                    {
                        let a: i32;
                        a = 1;
                    }
                    ",
                );

                assert_type_check(&res, expected);
            }

            #[test]
            fn test_complex_late_assign() {
                let res: Block = parse(
                    "
                    {
                        let a;
                        a = 1;

                        let a;
                        a = if f(-1) {
                            f(1)
                        } else {
                            false
                        };

                        fn f(i: i32) -> bool {
                            i > 0
                        }
                    }
                    ",
                );
                let expected: Block = parse(
                    "
                    {
                        let a: i32;
                        a = 1;

                        let a: bool;
                        a = if f(-1) {
                            f(1)
                        } else {
                            false
                        };

                        fn f(i: i32) -> bool {
                            i > 0
                        }
                    }
                    ",
                );

                assert_type_check(&res, expected);
            }

            #[test]
            fn test_prog1() {
                let res: Prog = parse(
                    "
                    fn main() {
                        let a = 1;
                        a;
                    }
                    ",
                );
                let expected: Prog = parse(
                    "
                    fn main() -> () {
                        let a: i32 = 1;
                        a;
                    }
                    ",
                );

                assert_type_check(&res, expected);
            }

            #[test]
            fn test_prog2() {
                let res: Prog = parse(
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
                let expected: Prog = parse(
                    "
                    fn dummy() -> i32 {
                        1
                    }

                    fn main() -> () {
                        let a: i32 = 1;
                        a + dummy();
                    }
                    ",
                );

                assert_type_check(&res, expected);
            }

            #[test]
            fn test_local_fn1() {
                let res: Prog = parse(
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
                let expected: Prog = parse(
                    "
                    fn main() -> () {

                        fn f(i: i32, j: i32) -> i32 {
                            i + j
                        }

                        fn g() -> i32 {
                            f(3, 4) // 7
                        }

                        let a: i32 = f(1, 2);
                        a + g();
                    }
                    ",
                );

                assert_type_check(&res, expected);
            }

            #[test]
            fn test_println() {
                let res: Prog = parse(
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
                let expected: Prog = parse(
                    "
                    fn main() -> () {
                        fn f(i: i32, j: i32) -> i32 {
                            i + j
                        }
                        let a: i32 = f(1, 2);
                        println!(\"a = {} and another a = {}\", a, a);
                        println!(\"But also some random test to be sure it is {{correctly}} implemented...\");
                    }
                    ",
                );

                assert_type_check(&res, expected);
            }

            #[test]
            fn test_fn_shadowing() {
                let res: Prog = parse(
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
                let expected: Prog = parse(
                    "
                    fn main() -> () {

                        fn f(i: i32, j: i32) -> i32 {
                            i + j
                        }

                        let a: i32 = f(1, 2);

                        fn g() -> i32 {
                            fn f(i: i32, j: i32) -> i32 {
                                i - j
                            }
                            f(3, 4)
                        }
                        a + g();
                    }
                    ",
                );

                assert_type_check(&res, expected);
            }

            #[test]
            fn test_call_before_def() {
                let res: Prog = parse(
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
                let expected: Prog = parse(
                    "
                    fn main() -> () {

                        let a: i32 = f(1, 2);

                        fn f(i: i32, j: i32) -> i32 {
                            i + j
                        }

                        fn g() -> i32 {
                            fn f(i: i32, j: i32) -> i32 {
                                i - j
                            }
                            f(3, 4)
                        }
                        a + g();
                    }
                    ",
                );

                assert_type_check(&res, expected);
            }

            #[test]
            fn test_recursion() {
                let res: Prog = parse(
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
                let expected: Prog = parse(
                    "
                    fn main() -> () {
                        fn fact(n: i32) -> i32 {
                            if n == 0 {
                                1
                            } else {
                                n * fact(n - 1)
                            }
                        }

                        let a: i32 = fact(5);
                    }
                    ",
                );

                assert_type_check(&res, expected);
            }

            #[test]
            fn test_correct_block_return_type_1() {
                let res: Block = parse(
                    "
                    {
                        let a = 1;
                        a
                    }
                    ",
                );
                let res = res.check_type().unwrap();

                let mut expected: Block = parse(
                    "
                    {
                        let a: i32 = 1;
                        a
                    }
                    ",
                );
                expected.return_type = Some(Type::I32);

                assert!(expected.fully_eq(&res));
            }

            #[test]
            fn test_correct_block_return_type_2() {
                let res: Block = parse(
                    "
                    {
                        let a = 1;
                        let b = 2;
                        a + b
                    }
                    ",
                );
                let res = res.check_type().unwrap();

                let mut expected: Block = parse(
                    "
                    {
                        let a: i32 = 1;
                        let b: i32 = 2;
                        a + b
                    }
                    ",
                );
                expected.return_type = Some(Type::I32);

                assert!(expected.fully_eq(&res));
            }

            #[test]
            fn test_correct_block_return_type_3() {
                let res: Block = parse(
                    "
                    {
                        let a = \"Hey!\";
                        a
                    }
                    ",
                );
                let res = res.check_type().unwrap();

                let mut expected: Block = parse(
                    "
                    {
                        let a: String = \"Hey!\";
                        a
                    }
                    ",
                );
                expected.return_type = Some(Type::String);

                assert!(expected.fully_eq(&res));
            }

            #[test]
            fn test_correct_block_return_type_4() {
                let res: Block = parse(
                    "
                    {
                        let a = [true, false];
                        a
                    }
                    ",
                );
                let res = res.check_type().unwrap();

                let mut expected: Block = parse(
                    "
                    {
                        let a: [bool; 2] = [true, false];
                        a
                    }
                    ",
                );
                expected.return_type = Some(Type::Array(Box::new(Type::Bool), 2));

                assert!(expected.fully_eq(&res));
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
        }
    }

    //? --------------------------- Failing tests ---------------------------

    #[cfg(test)]
    mod failing_tests {
        use super::*;

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
        fn test_if_missing_late_init() {
            let v = parse_type::<Block, TypeVal>(
                "
                {
                    let a: i32;
                    if true {
                        a = 1;
                    } else {
                        let b = 2;
                        // no initialization for a: Should panic even if not reachable
                    }
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
        fn test_if_different_late_init() {
            let v = parse_type::<Block, TypeVal>(
                "
                {
                    let a;
                    if true {
                        a = 1;
                    } else {
                        a = true; // Should crash (different types)
                    }
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
        fn test_if_different_late_init_complex() {
            let v = parse_type::<Block, TypeVal>(
                "
                {
                    let a;
                    let b;
                    if true {
                        a = 1;
                        if true {
                            b = 2;
                        } else {
                            b = 3;
                        }
                    } else {
                        a = 2;
                        if true {
                            b = true;
                        } else {
                            b = false;
                        }
                    }
                    b
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
        fn test_never_initialized_var_1() {
            let v = parse_type::<Block, TypeVal>(
                "
                {
                    let a: i32;
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
        fn test_never_initialized_var_2() {
            let v = parse_type::<Block, TypeVal>(
                "
                {
                    let a: i32;
                    let b = {
                        let a = 2;
                        a + 3
                    };
                    b
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
        fn test_never_initialized_var_3() {
            let v = parse_type::<Block, TypeVal>(
                "
                {
                    let a: i32;
                    let a = 3; // previous a is never initialized on its removal
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
        fn test_empty_array_type_missing_annotation() {
            let v = parse_type::<Block, TypeVal>(
                "
                {
                    let a = [];
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
        fn test_empty_array_type_missing_annotation_late_init() {
            let v = parse_type::<Block, TypeVal>(
                "
                {
                    let a;
                    a = [];
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
        fn test_immutable_large_array_1() {
            let v = parse_type::<Block, TypeVal>(
                "
                {
                    let a = [[1, 2], [3; 2]];
                    a[1][0] = 4;  // Should crash (a is immutable)
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
        fn test_immutable_large_array_2() {
            let v = parse_type::<Block, TypeVal>(
                "
                {
                    let a = [[1, 2], [3; 2]];
                    a[1] = [-1, 3]; // Should crash (a is immutable)
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
        fn test_array_assignment_wrong_dim_0() {
            let v = parse_type::<Block, TypeVal>(
                "
                {
                    let mut a = [[1, 2], [3, 4], [5, 6]];
                    a[1][1][0] = 2; // Should crash (out of dimension)
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
        fn test_array_assignment_wrong_dim_1() {
            let v = parse_type::<Block, TypeVal>(
                "
                {
                    let mut a = [[1, 2], [3, 4], [5, 6]];
                    a[1][1] = [7, 2]; // Should crash (wrong dimension)
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
        fn test_array_assignment_wrong_dim_2() {
            let v = parse_type::<Block, TypeVal>(
                "
                {
                    let mut a = [[1, 2], [3, 4], [5, 6]];
                    a[1] = [7, 2, 3]; // Should crash (wrong dimension)
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
        fn test_array_assignment_wrong_dim_3() {
            let v = parse_type::<Block, TypeVal>(
                "
                {
                    let mut a = [[1, 2], [3, 4], [5, 6]];
                    a = [7, 2, 3]; // Should crash (wrong dimensions)
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
        fn test_invalid_size_array_1() {
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
        #[should_panic]
        fn test_invalid_definition_array_1() {
            let v = parse_type::<Block, TypeVal>(
                "
                {
                    let s = 2;
                    let a = [1; s]; // Should crash (the expression used in the size is not a constant)
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
        fn test_invalid_definition_array_2() {
            let v = parse_type::<Block, TypeVal>(
                "
                {
                    let a = [1, true]; // Should crash (inconsistent types)
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
        fn test_invalid_definition_array_3() {
            let v = parse_type::<Block, TypeVal>(
                "
                {
                    let a = [1, 2, [3, 4]]; // Should crash (inconsistent types)
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
        fn test_invalid_definition_array_4() {
            let v = parse_type::<Block, TypeVal>(
                "
                {
                    let a = [[1, 2], [3; 4]]; // Should crash (inconsistent types)
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
            let res = parse_check_type::<Prog>(
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

            println!("Did not panic... resulting program = {}", res.unwrap());
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
            let res = parse_check_type::<Prog>(
                "
                fn main() -> i32 {
                    1
                }
                ",
            );

            println!("Did not panic... resulting program = {}", res.unwrap());
        }

        #[test]
        #[should_panic]
        fn test_invalid_main_args() {
            let res = parse_check_type::<Prog>(
                "
                fn main(x: i32) {
                    let y = x + 1;
                }
                ",
            );

            println!("Did not panic... resulting program = {}", res.unwrap());
        }
    }
}
