use rnr::ast::*;
use rnr::vm::VM;

// helper eval method to make it easier to evaluate very basic expressions (no statements, no mutable, no function calls, etc)
fn eval_basic_expr(e: Expr) -> Literal {
    let mut vm = VM::new();
    let result = vm.eval_expr(&e);
    let result_literal = result.unwrap().into();
    result_literal
}

//? References in rust

#[test]
fn test_references_in_real_rust() {
    let a = 1;
    let b = &a;
    println!("a={} and b={}", a, b);
    let a = true;
    let c = *b;
    println!("a={} and b={} and c={}", a, b, c);

    //? Invalid:
    // let a = {
    //     let b = 1;
    //     &b
    // };
}

//?#################################################################################################
//?#                                                                                               #
//?#                                          Expr Type                                            #
//?#                                                                                               #
//?#################################################################################################

#[cfg(test)]
mod test_expr {
    use super::*;

    #[test]
    fn test_display_literal() {
        assert_eq!(format!("{}", Expr::from(5)), "5");
        assert_eq!(format!("{}", Expr::from(true)), "true");
        assert_eq!(format!("{}", Expr::from(false)), "false");
        println!("{}", Literal::Int(3));
        println!("{}", Literal::Bool(false));
        println!("{}", Literal::Unit);
        assert_eq!(format!("{}", Literal::Int(3)), "3");
        assert_eq!(format!("{}", Literal::Bool(false)), "false");
        assert_eq!(format!("{}", Literal::Unit), "()");
    }

    #[test]
    fn test_display_binop() {
        let a = 5;
        let b = 3;
        let o = BinOp::Add;
        let e = Expr::bin_op(o, Expr::from(a), Expr::from(b));
        assert_eq!(format!("{}", e), format!("{} {} {}", a, o, b));
    }

    #[test]
    fn test_display_unop() {
        let a = 5;
        let o = UnOp::Neg;
        let e = Expr::un_op(o, Expr::from(a));
        assert_eq!(format!("{}", e), format!("{}{}", o, a));
    }

    #[test]
    fn test_display_parentheses() {
        let a = 5;
        let b = 3;
        let o = BinOp::Add;
        let e = Expr::bin_op(o, Expr::from(a), Expr::from(b));
        let p = Expr::Par(Box::new(e));
        assert_eq!(format!("{}", p), format!("({} {} {})", a, o, b));
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                         Literal Type                                          #
//?#                                                                                               #
//?#################################################################################################

#[cfg(test)]
mod test_literal {
    use super::*;

    #[test]
    fn test_int() {
        let a = 5;
        let l = Literal::Int(a);
        assert_eq!(l.get_int(), a);
    }

    #[test]
    fn test_bool() {
        let b = true;
        let l = Literal::Bool(b);
        assert_eq!(l.get_bool(), b);
    }

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", Literal::Int(5)), "5");
        assert_eq!(format!("{}", Literal::Bool(true)), "true");
        assert_eq!(format!("{}", Literal::Bool(false)), "false");
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                          BinOp Type                                           #
//?#                                                                                               #
//?#################################################################################################

#[cfg(test)]
mod test_binop {
    use super::*;

    // Valid operations

    #[test]
    fn test_add() {
        let a = 5;
        let b = 3;
        let e = Expr::bin_op(BinOp::Add, Expr::from(a), Expr::from(b));
        assert_eq!(eval_basic_expr(e), Literal::Int(a + b));
    }

    #[test]
    fn test_sub() {
        let a = 5;
        let b = 3;
        let e = Expr::bin_op(BinOp::Sub, Expr::from(a), Expr::from(b));
        assert_eq!(eval_basic_expr(e), Literal::Int(a - b));
    }

    #[test]
    fn test_mul() {
        let a = 5;
        let b = 3;
        let e = Expr::bin_op(BinOp::Mul, Expr::from(a), Expr::from(b));
        assert_eq!(eval_basic_expr(e), Literal::Int(a * b));
    }

    #[test]
    fn test_div() {
        let a = 6;
        let b = 3;
        let e = Expr::bin_op(BinOp::Div, Expr::from(a), Expr::from(b));
        assert_eq!(eval_basic_expr(e), Literal::Int(a / b));
    }

    #[test]
    fn test_and() {
        let a = true;
        let b = false;
        let e = Expr::bin_op(BinOp::And, Expr::from(a), Expr::from(b));
        assert_eq!(eval_basic_expr(e), Literal::Bool(a && b));
    }

    #[test]
    fn test_or() {
        let a = true;
        let b = false;
        let e = Expr::bin_op(BinOp::Or, Expr::from(a), Expr::from(b));
        assert_eq!(eval_basic_expr(e), Literal::Bool(a || b));
    }

    #[test]
    fn test_eq() {
        let a = 5;
        let b = 3;
        let e = Expr::bin_op(BinOp::Eq, Expr::from(a), Expr::from(b));
        assert_eq!(eval_basic_expr(e), Literal::Bool(a == b));
    }

    #[test]
    fn test_ne() {
        let a = 5;
        let b = 3;
        let e = Expr::bin_op(BinOp::Ne, Expr::from(a), Expr::from(b));
        assert_eq!(eval_basic_expr(e), Literal::Bool(a != b));
    }

    #[test]
    fn test_gt() {
        let a = 5;
        let b = 3;
        let e = Expr::bin_op(BinOp::Gt, Expr::from(a), Expr::from(b));
        assert_eq!(eval_basic_expr(e), Literal::Bool(a > b));
    }

    #[test]
    fn test_lt() {
        let a = 5;
        let b = 3;
        let e = Expr::bin_op(BinOp::Lt, Expr::from(a), Expr::from(b));
        assert_eq!(eval_basic_expr(e), Literal::Bool(a < b));
    }

    #[test]
    fn test_ge() {
        let a = 5;
        let b = 3;
        let e = Expr::bin_op(BinOp::Ge, Expr::from(a), Expr::from(b));
        assert_eq!(eval_basic_expr(e), Literal::Bool(a >= b));
    }

    #[test]
    fn test_le() {
        let a = 5;
        let b = 3;
        let e = Expr::bin_op(BinOp::Le, Expr::from(a), Expr::from(b));
        assert_eq!(eval_basic_expr(e), Literal::Bool(a <= b));
    }

    // Invalid operations

    #[test]
    #[should_panic]
    fn test_invalid_add_int_bool() {
        let a = 1;
        let b = false;
        let e = Expr::bin_op(BinOp::Add, Expr::from(a), Expr::from(b));
        eval_basic_expr(e);
    }

    #[test]
    #[should_panic]
    fn test_invalid_add_bool_bool() {
        let a = true;
        let b = false;
        let e = Expr::bin_op(BinOp::Add, Expr::from(a), Expr::from(b));
        eval_basic_expr(e);
    }

    #[test]
    #[should_panic]
    fn test_invalid_sub_int_bool() {
        let a = 1;
        let b = false;
        let e = Expr::bin_op(BinOp::Sub, Expr::from(a), Expr::from(b));
        eval_basic_expr(e);
    }

    #[test]
    #[should_panic]
    fn test_invalid_sub_bool_bool() {
        let a = true;
        let b = false;
        let e = Expr::bin_op(BinOp::Sub, Expr::from(a), Expr::from(b));
        eval_basic_expr(e);
    }

    #[test]
    #[should_panic]
    fn test_invalid_mul_int_bool() {
        let a = 1;
        let b = false;
        let e = Expr::bin_op(BinOp::Mul, Expr::from(a), Expr::from(b));
        eval_basic_expr(e);
    }

    #[test]
    #[should_panic]
    fn test_invalid_mul_bool_bool() {
        let a = true;
        let b = false;
        let e = Expr::bin_op(BinOp::Mul, Expr::from(a), Expr::from(b));
        eval_basic_expr(e);
    }

    #[test]
    #[should_panic]
    fn test_div_by_zero() {
        let e = Expr::bin_op(BinOp::Div, Expr::from(6), Expr::from(0));
        eval_basic_expr(e);
    }

    #[test]
    #[should_panic]
    fn test_invalid_div_int_bool() {
        let a = 1;
        let b = false;
        let e = Expr::bin_op(BinOp::Div, Expr::from(a), Expr::from(b));
        eval_basic_expr(e);
    }

    #[test]
    #[should_panic]
    fn test_invalid_div_bool_bool() {
        let a = true;
        let b = false;
        let e = Expr::bin_op(BinOp::Div, Expr::from(a), Expr::from(b));
        eval_basic_expr(e);
    }

    #[test]
    #[should_panic]
    fn test_invalid_and() {
        let a = 1;
        let b = 2;
        let e = Expr::bin_op(BinOp::And, Expr::from(a), Expr::from(b));
        eval_basic_expr(e);
    }

    #[test]
    #[should_panic]
    fn test_invalid_or() {
        let a = 1;
        let b = 2;
        let e = Expr::bin_op(BinOp::Or, Expr::from(a), Expr::from(b));
        eval_basic_expr(e);
    }

    // Display

    #[test]
    fn test_display() {
        // Integer operations
        assert_eq!(format!("{}", BinOp::Add), "+");
        assert_eq!(format!("{}", BinOp::Sub), "-");
        assert_eq!(format!("{}", BinOp::Mul), "*");
        assert_eq!(format!("{}", BinOp::Div), "/");
        // Boolean operations
        assert_eq!(format!("{}", BinOp::And), "&&");
        assert_eq!(format!("{}", BinOp::Or), "||");
        // Comparison operations
        assert_eq!(format!("{}", BinOp::Eq), "==");
        assert_eq!(format!("{}", BinOp::Ne), "!=");
        assert_eq!(format!("{}", BinOp::Gt), ">");
        assert_eq!(format!("{}", BinOp::Lt), "<");
        assert_eq!(format!("{}", BinOp::Ge), ">=");
        assert_eq!(format!("{}", BinOp::Le), "<=");
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                          UnOp Type                                            #
//?#                                                                                               #
//?#################################################################################################

#[cfg(test)]
mod test_unop {
    use super::*;

    // Valid operations

    #[test]
    fn test_neg() {
        let a = 5;
        let e = Expr::un_op(UnOp::Neg, Expr::from(a));
        assert_eq!(eval_basic_expr(e), Literal::Int(-a));
    }

    #[test]
    fn test_bang() {
        let b = true;
        let e = Expr::un_op(UnOp::Bang, Expr::from(b));
        assert_eq!(eval_basic_expr(e), Literal::Bool(!b));
    }

    // Invalid operations

    #[test]
    #[should_panic]
    fn test_invalid_neg() {
        let b = true;
        let e = Expr::un_op(UnOp::Neg, Expr::from(b));
        eval_basic_expr(e);
    }

    #[test]
    #[should_panic]
    fn test_invalid_bang() {
        let a = 1;
        let e = Expr::un_op(UnOp::Bang, Expr::from(a));
        eval_basic_expr(e);
    }

    // Display

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", UnOp::Neg), "-");
        assert_eq!(format!("{}", UnOp::Bang), "!");
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                         'Type' Type                                           #
//?#                                                                                               #
//?#################################################################################################

#[cfg(test)]
mod test_type {
    use super::*;

    #[test]
    fn test_display_type() {
        assert_eq!(format!("{}", Type::I32), "i32");
        assert_eq!(format!("{}", Type::Bool), "bool");
        assert_eq!(format!("{}", Type::Unit), "()");
        assert_eq!(format!("{}", Type::String), "String");
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                         Block Type                                            #
//?#                                                                                               #
//?#################################################################################################

//?#################################################################################################
//?#                                                                                               #
//?#                                         Mutable Type                                          #
//?#                                                                                               #
//?#################################################################################################

//?#################################################################################################
//?#                                                                                               #
//?#                                       Parameter Type                                          #
//?#                                                                                               #
//?#################################################################################################

//?#################################################################################################
//?#                                                                                               #
//?#                                       Parameters Type                                         #
//?#                                                                                               #
//?#################################################################################################

//?#################################################################################################
//?#                                                                                               #
//?#                                       Arguments Type                                          #
//?#                                                                                               #
//?#################################################################################################

//?#################################################################################################
//?#                                                                                               #
//?#                                     FnDeclaration Type                                        #
//?#                                                                                               #
//?#################################################################################################

//?#################################################################################################
//?#                                                                                               #
//?#                                         Prog Type                                             #
//?#                                                                                               #
//?#################################################################################################

//?#################################################################################################
//?#                                                                                               #
//?#                                       Statement Type                                          #
//?#                                                                                               #
//?#################################################################################################

//? ------------------------------ Initial Tests from the ast_traits file, and some new ones as well ------------------------------

#[cfg(test)]
mod initial_tests {
    use super::*;

    #[test]
    fn display_simple_block() {
        let ts: proc_macro2::TokenStream = "
        {
            let a = 3;
            let b = 4;
            let c = {
                a + b
            };
            let d = {
                let e = 5;
                e
            };
            c + d
        }
        "
        .parse()
        .unwrap();
        let e: Block = syn::parse2(ts).unwrap();
        println!("ast:\n{:?}", e);

        println!("pretty:\n{}", e);
    }

    #[test]
    fn display_simple_block_with_func() {
        let ts: proc_macro2::TokenStream = "
        {
            fn add(a: i32, b: i32) -> i32 {
                a + b
            }

            let a = 3;
            let b = 4;
            let c = add(a, b);
            c
        }
        "
        .parse()
        .unwrap();
        let e: Block = syn::parse2(ts).unwrap();
        println!("ast:\n{:?}", e);

        println!("pretty:\n{}", e);
    }

    #[test]
    fn display_if_then_else_1() {
        let ts: proc_macro2::TokenStream = "
        {
            let a = if b {
                3
            } else {
                4
            };

            let b = if c { let d = 3; d } else { 4 };

            a + b
        }
        "
        .parse()
        .unwrap();
        let e: Block = syn::parse2(ts).unwrap();
        println!("ast:\n{:?}", e);

        println!("pretty:\n{}", e);
    }

    #[test]
    fn display_if_then_else_2() {
        let ts: proc_macro2::TokenStream = "
        if a {
            let a : i32 = false;
            0
        } else {
            if a == 5 { b = 8 };
            while b {
                e;
            }
            b
        }
        "
        .parse()
        .unwrap();
        let e: Expr = syn::parse2(ts).unwrap();
        println!("ast:\n{:?}", e);

        println!("pretty:\n{}", e);
    }

    #[test]
    fn display_if_then_else_if_1() {
        let ts: proc_macro2::TokenStream = "
        if a {1} else if b {2} else {3}
        "
        .parse()
        .unwrap();
        let e: Expr = syn::parse2(ts).unwrap();
        println!("ast:\n{:?}", e);

        println!("pretty:\n{}", e);
    }

    #[test]
    fn display_if_then_else_if_2() {
        let ts: proc_macro2::TokenStream = "
        if a {
            let d = 3;
            f(d)
        } else if b {
            if c > 10 { f(8) } else { f(11) }
        } else if c {
            if c < 4 { f(5) }
        } else {
            f(2)
        }
        "
        .parse()
        .unwrap();
        let e: Expr = syn::parse2(ts).unwrap();
        println!("ast:\n{:?}", e);

        println!("pretty:\n{}", e);
    }

    #[test]
    fn display_while() {
        let ts: proc_macro2::TokenStream = "
        while a == 9 {
            let b : i32 = 7;
        }
        "
        .parse()
        .unwrap();
        let e: Statement = syn::parse2(ts).unwrap();
        println!("ast:\n{:?}", e);

        println!("pretty:\n{}", e);
    }

    #[test]
    fn display_while_in_block() {
        let ts: proc_macro2::TokenStream = "
        {
            let mut a = 3;
            while a != 9 {
                if a > 9 {
                    a = 9;
                } else {
                    a = a + 1;
                }
            }
            a
        }
        "
        .parse()
        .unwrap();
        let e: Block = syn::parse2(ts).unwrap();
        println!("ast:\n{:?}", e);

        println!("pretty:\n{}", e);
    }

    #[test]
    fn display_expr() {
        println!("{}", Expr::Ident("a".to_string()));
        println!("{}", Expr::Lit(Literal::Int(7)));
        println!("{}", Expr::Lit(Literal::Bool(false)));
        let e = Expr::BinOp(
            BinOp::Add,
            Box::new(Expr::Ident("a".to_string())),
            Box::new(Expr::Lit(Literal::Int(7))),
        );
        println!("{}", e);
        assert_eq!(format!("{}", e), "a + 7");
    }

    // As you see it becomes cumbersome to write tests
    // if you have to construct the Expr by hand.
    //
    // Instead we might use our parser

    #[test]
    fn parse_display_expr() {
        let ts: proc_macro2::TokenStream = "a + 7".parse().unwrap();
        let e: Expr = syn::parse2(ts).unwrap();
        println!("e {}", e);
    }

    // This one will fail (Display for `if` is not yet implemented).
    // Implement it as an optional assignment
    //
    // Hint: You need to implement Display for Statement and Block

    #[test]
    fn parse_display_if() {
        let ts: proc_macro2::TokenStream = "if a > 5 {5}".parse().unwrap();
        let e: Expr = syn::parse2(ts).unwrap();
        println!("e {}", e);
    }
}
