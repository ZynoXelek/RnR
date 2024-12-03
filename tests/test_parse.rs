use rnr::ast::*;
use rnr::parse::*;
use rnr::test_util::assert_parse_fail;
use syn::Result;

//? -------------------------- Testing some parsing token streams --------------------------

#[cfg(test)]
mod test_token_streams {

    fn get_token_stream(input: &str) -> proc_macro2::TokenStream {
        let ts: proc_macro2::TokenStream = input.parse().unwrap();
        println!("Token Stream for input {:?} is {:?}", input, ts);
        input.parse().unwrap()
    }

    #[test]
    fn test_token_types() {
        let inputs = vec!["i32", "bool", "()", "String"];
        for input in inputs {
            get_token_stream(input);
        }
    }

    #[test]
    fn test_tokens_lit() {
        let inputs = vec!["1", "-1", "true", "false", "\"hello\"", ""];
        for input in inputs {
            get_token_stream(input);
        }
    }

    #[test]
    fn test_tokens_binop() {
        let inputs = vec![
            "+", "-", "*", "/", "&&", "||", "==", "!=", "<", ">", "<=", ">=",
        ];
        for input in inputs {
            get_token_stream(input);
        }
    }

    #[test]
    fn test_tokens_unop() {
        let inputs = vec!["!", "-"];
        for input in inputs {
            get_token_stream(input);
        }
    }

    #[test]
    fn test_tokens_identifier() {
        let inputs = vec!["my_variable", "var", "var123", "_var_123_"];
        for input in inputs {
            get_token_stream(input);
        }
    }

    #[test]
    fn test_token_arguments() {
        let inputs = vec![
            "(1)",
            "(a)",
            "(a,)",
            "(a, b)",
            "(a + 1, b * 2)",
            "(1, 2, 3 + 4)",
        ];
        for input in inputs {
            get_token_stream(input);
        }
    }

    #[test]
    fn test_tokens_call() {
        let input = vec![
            "foo()",
            "foo(1)",
            "foo(true)",
            "foo(true, false)",
            "foo(true || false)",
        ];
        for input in input {
            get_token_stream(input);
        }
    }

    #[test]
    fn test_tokens_block() {
        let inputs = vec![
            "{ let a : i32 = 1; a = 5; a + 5 }",
            "{ 
                let b : bool = false;
                b = true
            }",
        ];
        for input in inputs {
            get_token_stream(input);
        }
    }

    #[test]
    fn test_tokens_if_then_else() {
        let inputs = vec!["if a > 0 {1} else {2}", "if false {a}"];
        for input in inputs {
            get_token_stream(input);
        }
    }

    #[test]
    fn test_tokens_parameters() {
        let inputs = vec![
            "(a: i32)",
            "(a: i32,)",
            "(b: bool)",
            "(a: i32, b: bool)",
            "(a: i32, b: bool,)",
        ];
        for input in inputs {
            get_token_stream(input);
        }
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                         Literal Type                                          #
//?#                                                                                               #
//?#################################################################################################

#[cfg(test)]
mod test_parse_lit {
    use super::*;

    #[test]
    fn parse_lit_int() {
        let lit: Literal = parse("1");
        assert_eq!(lit, Literal::Int(1));
    }

    #[test]
    fn parse_lit_neg_int() {
        let lit: Literal = parse("-1");
        assert_eq!(lit, Literal::Int(-1));
    }

    #[test]
    fn parse_lit_bool_false() {
        let lit: Literal = parse("false");
        assert_eq!(lit, Literal::Bool(false));
    }

    #[test]
    fn parse_lit_string() {
        let lit: Literal = parse("\"abba\"");
        assert_eq!(lit, Literal::String("abba".to_string()));
    }

    #[test]
    fn parse_lit_array() {
        let lit: Literal = parse("[]");
        assert_eq!(lit, Literal::Array(vec![], 0));

        let lit: Literal = parse("[1, 2, 3]");
        assert_eq!(
            lit,
            Literal::Array(vec![Literal::Int(1), Literal::Int(2), Literal::Int(3)], 3)
        );

        let lit: Literal = parse("[1, 2, 3,]");
        assert_eq!(
            lit,
            Literal::Array(vec![Literal::Int(1), Literal::Int(2), Literal::Int(3)], 3)
        );

        let lit: Literal = parse("[1; 2]");
        assert_eq!(
            lit,
            Literal::Array(vec![Literal::Int(1), Literal::Int(1)], 2)
        );

        let lit: Literal = parse("[true, false, true]");
        assert_eq!(
            lit,
            Literal::Array(
                vec![
                    Literal::Bool(true),
                    Literal::Bool(false),
                    Literal::Bool(true)
                ],
                3
            )
        );

        let lit: Literal = parse("[\"hello\", \"world\"]");
        assert_eq!(
            lit,
            Literal::Array(
                vec![
                    Literal::String("hello".to_string()),
                    Literal::String("world".to_string())
                ],
                2
            )
        );

        // recursive
        let lit: Literal = parse("[[1, 2], [3, 4]]");
        assert_eq!(
            lit,
            Literal::Array(
                vec![
                    Literal::Array(vec![Literal::Int(1), Literal::Int(2)], 2),
                    Literal::Array(vec![Literal::Int(3), Literal::Int(4)], 2)
                ],
                2
            )
        );
    }

    #[test]
    fn parse_lit_fail() {
        assert_parse_fail::<Literal>("a");
        assert_parse_fail::<Literal>("-");
        assert_parse_fail::<Literal>("'hello'");
        assert_parse_fail::<Literal>("[1, 2, 3");
        assert_parse_fail::<Literal>("[, 2, 3]");
    }

    #[test]
    #[should_panic]
    fn test_parsing_errors() {
        let _: Literal = parse("[1, 2 3]");
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                          BinOp Type                                           #
//?#                                                                                               #
//?#################################################################################################

#[cfg(test)]
mod test_parse_binop {
    use super::*;

    #[test]
    fn parse_op_add() {
        let op: BinOp = parse("+");
        assert_eq!(op, BinOp::Add);
    }

    #[test]
    fn parse_op_sub() {
        let op: BinOp = parse("-");
        assert_eq!(op, BinOp::Sub);
    }

    #[test]
    fn parse_op_mul() {
        let op: BinOp = parse("*");
        assert_eq!(op, BinOp::Mul);
    }

    #[test]
    fn parse_op_div() {
        let op: BinOp = parse("/");
        assert_eq!(op, BinOp::Div);
    }

    #[test]
    fn parse_op_and() {
        let op: BinOp = parse("&&");
        assert_eq!(op, BinOp::And);
    }

    #[test]
    fn parse_op_or() {
        let op: BinOp = parse("||");
        assert_eq!(op, BinOp::Or);
    }

    #[test]
    fn parse_op_eq() {
        let op: BinOp = parse("==");
        assert_eq!(op, BinOp::Eq);
    }

    #[test]
    fn parse_op_ne() {
        let op: BinOp = parse("!=");
        assert_eq!(op, BinOp::Ne);
    }

    #[test]
    fn parse_op_lt() {
        let op: BinOp = parse("<");
        assert_eq!(op, BinOp::Lt);
    }

    #[test]
    fn parse_op_gt() {
        let op: BinOp = parse(">");
        assert_eq!(op, BinOp::Gt);
    }

    #[test]
    fn parse_op_le() {
        let op: BinOp = parse("<=");
        assert_eq!(op, BinOp::Le);
    }

    #[test]
    fn parse_op_ge() {
        let op: BinOp = parse(">=");
        assert_eq!(op, BinOp::Ge);
    }

    #[test]
    fn parse_op_fail() {
        assert_parse_fail::<BinOp>("");
        assert_parse_fail::<BinOp>("1");
        assert_parse_fail::<BinOp>("x");
        assert_parse_fail::<BinOp>(".");
        assert_parse_fail::<BinOp>("=");
    }

    #[test]
    #[should_panic]
    fn test_parsing_errors() {
        let _: BinOp = parse("e");
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                          UnOp Type                                            #
//?#                                                                                               #
//?#################################################################################################

#[cfg(test)]
mod test_parse_unop {
    use super::*;

    #[test]
    fn bang() {
        let op: UnOp = parse("!");
        assert_eq!(op, UnOp::Bang);
    }

    #[test]
    fn neg() {
        let op: UnOp = parse("-");
        assert_eq!(op, UnOp::Neg);
    }

    #[test]
    fn parse_unop_fail() {
        assert_parse_fail::<UnOp>(".");
        assert_parse_fail::<UnOp>("/");
        assert_parse_fail::<UnOp>("x");
        assert_parse_fail::<UnOp>("1");
        assert_parse_fail::<UnOp>("i32");
        assert_parse_fail::<UnOp>("true");
        assert_parse_fail::<UnOp>("()");
    }

    #[test]
    #[should_panic]
    fn test_parsing_errors() {
        let _: UnOp = parse("e");
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                          Expr Type                                            #
//?#                                                                                               #
//?#################################################################################################

#[cfg(test)]
mod test_parse_expr {
    use super::*;

    // NOTE: Most tests that involve expressions have been moved to the `expr` module in
    // the `tests/integration_tests.rs` file.
    // Here we just focus on checking that parsing does not panic, and also checking that binary
    // operator expressions are correctly parsed (in terms of associativity and precedence).
    // In particular, these tests do not evaluate expressions!

    #[test]
    fn literal() {
        parse::<Expr>("123");
        parse::<Expr>("true");
        parse::<Expr>("()");
        parse::<Expr>("\"hello world\"");
    }

    #[test]
    fn binary_op() {
        parse::<Expr>("1 + 2");
        parse::<Expr>("1 - 2");
        parse::<Expr>("1 * 2");
        parse::<Expr>("1 / 2");
        parse::<Expr>("1 + 2 + 3");
        parse::<Expr>("1 * 2 - 3 / 1");
        parse::<Expr>("(1) + (2)");
        parse::<Expr>("(1 + 2) + (3 + 4)");
        parse::<Expr>("(1 * 2) - (3 / 4)");
        parse::<Expr>("true || !false");
        parse::<Expr>("!true && false");
        parse::<Expr>("true && (!false || true) && false");
    }

    #[test]
    fn array_access() {
        parse::<Expr>("[1, 2][1]");
        parse::<Expr>("a[2]");
    }

    #[test]
    fn ident_followed_by_diff_operator() {
        parse::<Expr>("a != 2"); //If macros are not correctly implemented, this will fail because it will be parsed as `a!  =  2`
                                 //Which will be incorrectly interpreted as a macro call with lacking arguments.
    }

    #[test]
    fn binary_op_comparisons() {
        parse::<Expr>("1 == 2");
        parse::<Expr>("1 < 2");
        parse::<Expr>("1 > 2");
        parse::<Expr>("1 + (2 * 3) == (2 - 3) / 4");
        parse::<Expr>("1 + 2 < 2 * 3");
        parse::<Expr>("1 + 2 > (2 - 1)");
        parse::<Expr>("true == true");
        parse::<Expr>("true || false == true && (false || true)");
    }

    #[test]
    fn unary_op() {
        parse::<Expr>("-(1)");
        parse::<Expr>("-(1+2)");
        parse::<Expr>("-(2 * 3 / 2)");
        parse::<Expr>("!true");
        parse::<Expr>("!true && true");
        parse::<Expr>("!true || !false");
        parse::<Expr>("!(true && !true)");
    }

    #[test]
    fn identifier() {
        parse::<Expr>("my_variable");
        parse::<Expr>("var");
        parse::<Expr>("var123");
        parse::<Expr>("_var_123_");
    }

    #[test]
    fn operators_and_identifiers() {
        parse::<Expr>("-my_variable");
        parse::<Expr>("!var");
        parse::<Expr>("321 + var123 - 123");
        parse::<Expr>("(1 - _var_123_ * 2) == a && (!b || true) || !a");
    }

    // Trying to parse these expressions should fail.

    #[test]
    fn fail_tests() {
        assert_parse_fail::<Expr>("12 34");
        assert_parse_fail::<Expr>("+");
        assert_parse_fail::<Expr>("1+");
        assert_parse_fail::<Expr>("1++2");
        assert_parse_fail::<Expr>("1+2+3+4+");
        assert_parse_fail::<Expr>("(1+2+3+4");
        assert_parse_fail::<Expr>("1+2+3+4)");
        assert_parse_fail::<Expr>("1)+2+(3+4");
        assert_parse_fail::<Expr>("3(1+2)");
        assert_parse_fail::<Expr>("(1+2)3");
        assert_parse_fail::<Expr>("(1+2)(3+4)");
        assert_parse_fail::<Expr>("12!34");
        assert_parse_fail::<Expr>("true ! false");
        assert_parse_fail::<Expr>("(2 * 4) - ");
    }

    #[test]
    #[should_panic]
    fn test_parsing_errors() {
        let _: Expr = parse(" * ");
    }

    // Some helpers for building Expr ASTs.

    fn add<T1: Into<Expr>, T2: Into<Expr>>(left: T1, right: T2) -> Expr {
        Expr::bin_op(BinOp::Add, left.into(), right.into())
    }

    fn mul<T1: Into<Expr>, T2: Into<Expr>>(left: T1, right: T2) -> Expr {
        Expr::bin_op(BinOp::Mul, left.into(), right.into())
    }

    fn or<T1: Into<Expr>, T2: Into<Expr>>(left: T1, right: T2) -> Expr {
        Expr::bin_op(BinOp::Or, left.into(), right.into())
    }

    fn and<T1: Into<Expr>, T2: Into<Expr>>(left: T1, right: T2) -> Expr {
        Expr::bin_op(BinOp::And, left.into(), right.into())
    }

    fn eq<T1: Into<Expr>, T2: Into<Expr>>(left: T1, right: T2) -> Expr {
        Expr::bin_op(BinOp::Eq, left.into(), right.into())
    }

    pub fn paren(expr: Expr) -> Expr {
        Expr::Par(Box::new(expr))
    }

    // Here are some test cases that directly examine the AST that is built from the expressions to
    // make sure that precedence and associativity are handled correctly.

    #[test]
    fn precedence_and_associativity_1() {
        let expr: Expr = parse("1+2+3");
        let expected = add(add(1, 2), 3);
        assert_eq!(expr, expected);
    }

    #[test]
    fn precedence_and_associativity_2() {
        let expr: Expr = parse("1+2*3");
        let expected = add(1, mul(2, 3));
        assert_eq!(expr, expected);
    }

    #[test]
    fn precedence_and_associativity_3() {
        let expr: Expr = parse("1+2*3+4");
        let expected = add(add(1, mul(2, 3)), 4);
        assert_eq!(expr, expected);
    }

    #[test]
    fn precedence_and_associativity_4() {
        let expr: Expr = parse("1+2*3 == 4");
        let expected = eq(add(1, mul(2, 3)), 4);
        assert_eq!(expr, expected);
    }

    #[test]
    fn precedence_and_associativity_5() {
        let expr: Expr = parse("1+2*3 == 1+2*3");
        let expected = eq(add(1, mul(2, 3)), add(1, mul(2, 3)));
        assert_eq!(expr, expected);
    }

    #[test]
    fn precedence_and_associativity_6() {
        let expr: Expr = parse("1*2+3*4+5*6");
        let expected = add(add(mul(1, 2), mul(3, 4)), mul(5, 6));
        assert_eq!(expr, expected);
    }

    #[test]
    fn precedence_and_associativity_7() {
        let expr: Expr = parse("1+2 * 3+4 == 5*(6+7) + 8*9");
        let left = add(add(1, mul(2, 3)), 4);
        let right = add(mul(5, paren(add(6, 7))), mul(8, 9));
        let expected = eq(left, right);
        assert_eq!(expr, expected);
    }

    // NOTE: priorities: `==` > `&&` > `||`
    // (Comparisons take precedence over and/or!)

    #[test] // 1 2 3
    fn precedence_and_associativity_123() {
        let expr: Expr = parse("true || true && true == true");
        let expected = or(true, and(true, eq(true, true)));
        assert_eq!(expr, expected);
    }

    #[test] // 1 3 2
    fn precedence_and_associativity_132() {
        let expr: Expr = parse("true || true == true && true");
        let expected = or(true, and(eq(true, true), true));
        assert_eq!(expr, expected);
    }

    #[test] // 2 1 3
    fn precedence_and_associativity_213() {
        let expr: Expr = parse("true && true || true == true");
        let expected = or(and(true, true), eq(true, true));
        assert_eq!(expr, expected);
    }

    #[test] // 2 3 1
    fn precedence_and_associativity_231() {
        let expr: Expr = parse("true && true == true || true");
        let expected = or(and(true, eq(true, true)), true);
        assert_eq!(expr, expected);
    }

    #[test] // 3 1 2
    fn precedence_and_associativity_312() {
        let expr: Expr = parse("true == true || true && true");
        let expected = or(eq(true, true), and(true, true));
        assert_eq!(expr, expected);
    }

    #[test] // 3 2 1
    fn precedence_and_associativity_321() {
        let expr: Expr = parse("true == true && true || true");
        let expected = or(and(eq(true, true), true), true);
        assert_eq!(expr, expected);
    }

    #[test]
    fn array_access_expr_with_array() {
        // let a = [true, false][[1, 0][1]]; // This is valid Rust code, which would give true.

        let expr: Expr = parse("[true, false][[1, 0][1]]");
        let expected = {
            let array = Expr::Lit(Literal::Array(
                vec![Literal::Bool(true), Literal::Bool(false)],
                2,
            ));
            let array_idx = Expr::Lit(Literal::Array(
                vec![Literal::Int(1), Literal::Int(0)],
                2,
            ));
            let index = Expr::bin_op(BinOp::Get, array_idx, Expr::Lit(Literal::Int(1)));
            Expr::bin_op(BinOp::Get, array, index)
        };
        assert_eq!(expr, expected);
    }

    #[test]
    fn array_access_expr_with_unary_operation_1() {
        // let a = ![true, false][0]; // This is valid Rust code, which would give false.

        let expr: Expr = parse("![true, false][0]");
        let expected = {
            let array = Expr::Lit(Literal::Array(
                vec![Literal::Bool(true), Literal::Bool(false)],
                2,
            ));
            let index = Expr::Lit(Literal::Int(0));
            Expr::UnOp(UnOp::Bang, Box::new(Expr::bin_op(BinOp::Get, array, index)))
        };
        assert_eq!(expr, expected);
    }

    #[test]
    fn array_access_expr_with_unary_operation_2() {
        // let a = true && ![true, false][0]; // This is valid Rust code, which would give false.

        let expr: Expr = parse("true && ![true, false][0]");
        let expected = {
            let array = Expr::Lit(Literal::Array(
                vec![Literal::Bool(true), Literal::Bool(false)],
                2,
            ));
            let index = Expr::Lit(Literal::Int(0));
            let right_operand = Expr::UnOp(UnOp::Bang, Box::new(Expr::bin_op(BinOp::Get, array, index)));
            Expr::bin_op(BinOp::And, Expr::Lit(Literal::Bool(true)), right_operand)
        };
        assert_eq!(expr, expected);
    }

    #[test]
    fn array_access_expr_with_unary_operation_3() {
        // let a = ![true, false][0] || true; // This is valid Rust code, which would give true.

        let expr: Expr = parse("![true, false][0] || true");
        let expected = {
            let array = Expr::Lit(Literal::Array(
                vec![Literal::Bool(true), Literal::Bool(false)],
                2,
            ));
            let index = Expr::Lit(Literal::Int(0));
            let left_operand = Expr::UnOp(UnOp::Bang, Box::new(Expr::bin_op(BinOp::Get, array, index)));
            Expr::bin_op(BinOp::Or, left_operand, Expr::Lit(Literal::Bool(true)))
        };
        assert_eq!(expr, expected);
    }
}

#[cfg(test)]
mod test_parse_if {
    use super::*;

    // This test is not really a test of our parser
    // Added just a reference to how Rust would treat the nesting.
    #[test]
    #[allow(unused_must_use)]
    fn test_if_then_else_nested_rust() {
        if false {
            2;
        } else {
            if true {
                3 + 5;
            }
        };
    }

    // This test is not really a test of our parser
    // Added just a reference to how Rust would treat the nesting.
    #[test]
    #[allow(unused_must_use)]
    fn test_if_then_else_nested_rust2() {
        if false {
            2;
        } else if true {
            3 + 5;
        };
    }

    // NOTE: These tests just parse some if-expressions and just (implicitly) check that there are
    // no panics.

    #[test]
    fn test_if_then_else_nested2() {
        let src = "
        if false {
            2;
        } else if true {
            3 + 5;
        }";
        let e: Expr = parse(src);
        println!("Source:\n{}", src);
        println!("Parsing result (pretty print):\n{}", e);
    }

    #[test]
    fn test_if_then_else_nested() {
        let src = "
        if false {
            2;
        } else {
            if true {
                3 + 5;
            }
        }";
        let e: Expr = parse(src);
        println!("Source:\n{}", src);
        println!("Parsing result (pretty print):\n{}", e);
    }

    #[test]
    fn test_if_then_else_nested3() {
        let src = "
        if false {
            2;
        } else if true {
            3 + 5;
        } else if false {
            let a : i32 = 0;
            a
        } else {
            5
        }
        ";
        let e: Expr = parse(src);
        println!("Source:\n{}", src);
        println!("Parsing result (pretty print):\n{}", e);
    }

    #[test]
    fn test_expr_if_then_else() {
        let src = "if a > 0 {1} else {2}";
        let e: Expr = parse(src);
        println!("Source:\n{}", src);
        println!("Parsing result (pretty print):\n{}", e);
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                         'Type' Type                                           #
//?#                                                                                               #
//?#################################################################################################

#[cfg(test)]
mod test_parse_type {
    use super::*;

    #[test]
    fn parse_type_i32() {
        let typ: Type = parse("i32");
        assert_eq!(typ, Type::I32);
    }

    #[test]
    fn parse_type_bool() {
        let typ: Type = parse("bool");
        assert_eq!(typ, Type::Bool);
    }

    #[test]
    fn parse_type_unit() {
        let typ: Type = parse("()");
        assert_eq!(typ, Type::Unit);
    }

    #[test]
    fn parse_type_String() {
        let typ: Type = parse("String");
        assert_eq!(typ, Type::String);
    }

    #[test]
    fn parse_type_Arrays() {
        let typ: Type = parse("[i32; 5]");
        assert_eq!(typ, Type::Array(Box::new(Type::I32), 5));

        let typ: Type = parse("[bool; 3]");
        assert_eq!(typ, Type::Array(Box::new(Type::Bool), 3));

        let typ: Type = parse("[String; 4]");
        assert_eq!(typ, Type::Array(Box::new(Type::String), 4));

        let typ: Type = parse("[[i32; 2]; 3]");
        assert_eq!(
            typ,
            Type::Array(Box::new(Type::Array(Box::new(Type::I32), 2)), 3)
        );
    }

    #[test]
    fn parse_type_fail() {
        assert_parse_fail::<Type>("u32");
        assert_parse_fail::<Type>("I32");
        assert_parse_fail::<Type>("123");
        assert_parse_fail::<Type>("boolean");
        assert_parse_fail::<Type>("Bool");
        assert_parse_fail::<Type>("true");
        assert_parse_fail::<Type>("false");
        assert_parse_fail::<Type>("(e)");
        assert_parse_fail::<Type>("(i32)");
        assert_parse_fail::<Type>("(())");
        assert_parse_fail::<Type>("<i32");
        assert_parse_fail::<Type>("<str>");
        assert_parse_fail::<Type>("[i32]"); // missing size
        assert_parse_fail::<Type>("[i32, 2]"); // should use ; instead of ,
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                         Arguments Type                                        #
//?#                                                                                               #
//?#################################################################################################

#[cfg(test)]
mod test_parse_fn_calls {
    use super::*;

    #[test]
    fn args() {
        parse::<Arguments>("(1)");
        parse::<Arguments>("(a)");
        parse::<Arguments>("(a, b)");
        parse::<Arguments>("(a + 1, b * 2)");
        parse::<Arguments>("(1, 2, 3 + 4)");
    }

    #[test]
    fn function_call() {
        parse::<Expr>("foo()");
        parse::<Expr>("foo(1)");
        parse::<Expr>("foo(true)");
        parse::<Expr>("foo(true, false)");
        parse::<Expr>("foo(true || false)");
        parse::<Expr>("foo(1 + 2)");
        parse::<Expr>("foo(1, 2)");
        parse::<Expr>("foo(1, 2 + 2)");
        parse::<Expr>("foo(my_variable)");
        parse::<Expr>("foo(a, b, c)");
        parse::<Expr>("foo(\"passing a string\")");
        parse::<Expr>("ident({1}, {let a = 6; a },)");
    }

    #[test]
    fn function_call_extra_comma() {
        parse::<Expr>("foo(1,)");
        parse::<Expr>("foo(1, 2,)");
        parse::<Expr>("foo(1, 2 + 2,)");
        parse::<Expr>("foo(a,)");
        parse::<Expr>("foo(a, b, c,)");
        parse::<Expr>("foo(true, false,)");
    }

    #[test]
    fn fail_tests() {
        assert_parse_fail::<Expr>("foo(,)");
        assert_parse_fail::<Expr>("foo(+)");
        assert_parse_fail::<Expr>("foo(1+)");
        assert_parse_fail::<Expr>("foo(2 * 4, -)");
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                      Parameter(s) Type                                        #
//?#                                                                                               #
//?#################################################################################################

#[cfg(test)]
mod test_parse_parameters {
    use super::*;

    #[test]
    fn single_param() {
        parse::<Parameter>("a: i32");
        parse::<Parameter>("b : bool");
        parse::<Parameter>("my_var:String");
    }

    #[test]
    fn params() {
        parse::<Parameters>("(a: i32, b: bool)");
        parse::<Parameters>("(a: i32,)");
        parse::<Parameters>("(a:i32,b:bool)");
    }

    #[test]
    fn fail_tests() {
        assert_parse_fail::<Parameter>("a i32");
        assert_parse_fail::<Parameter>("a: Int32");
        // Parameters
        assert_parse_fail::<Parameters>("(a, b)");
        assert_parse_fail::<Parameters>("(, a: i32)");
        assert_parse_fail::<Parameters>("(a: i32, b)");
        assert_parse_fail::<Parameters>("a: int32, b: bool");
        assert_parse_fail::<Parameters>("(a: int32, b: bool");
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                      FnDeclaration Type                                       #
//?#                                                                                               #
//?#################################################################################################

#[cfg(test)]
mod test_parse_fn_declaration {
    use super::*;

    #[test]
    fn param() {
        parse::<Parameter>("a: i32");
        parse::<Parameter>("b: bool");
    }

    #[test]
    fn params() {
        parse::<Parameters>("(a: i32)");
        parse::<Parameters>("(a: i32,)");
        parse::<Parameters>("(b: bool)");
        parse::<Parameters>("(a: i32, b: bool)");
        parse::<Parameters>("(a: i32, b: bool,)");
    }

    #[test]
    fn fn_no_type() {
        parse::<FnDeclaration>("fn foo() {}");
        parse::<FnDeclaration>("fn foo(a: i32, b: bool) {}");
    }

    #[test]
    fn fn_with_type() {
        parse::<FnDeclaration>("fn foo() -> i32 {}");
        parse::<FnDeclaration>("fn foo(a: i32, b: bool) -> i32 {}");
        parse::<FnDeclaration>("fn foo() -> () {}");
        parse::<FnDeclaration>("fn foo(a: i32, b: bool) -> () {}");
        parse::<FnDeclaration>("fn foo() -> bool {}");
        parse::<FnDeclaration>("fn foo(a: i32, b: bool) -> bool {}");
    }

    #[test]
    fn test_println() {
        let src = "println!(\"{}\", 1)";
        let expr: Expr = parse(src);
    }

    // Trying to parse these function declarations should fail.

    #[test]
    fn fail_tests() {
        assert_parse_fail::<Parameter>("123");
        assert_parse_fail::<Parameter>("i32");
        assert_parse_fail::<Parameter>("a = i32");
        assert_parse_fail::<FnDeclaration>("fn 123() {}");
        assert_parse_fail::<FnDeclaration>("fn foo(a, b: i32) {}");
        assert_parse_fail::<FnDeclaration>("fn foo(): i32 {}");
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                         Statement Type                                        #
//?#                                                                                               #
//?#################################################################################################

#[cfg(test)]
mod test_parse_statement {
    use super::*;

    #[test]
    fn test_statement_let_ty_expr() {
        let stmt: Statement = parse("let a: i32 = 2");
        let expected = Statement::Let(
            Mutable(false),
            "a".to_string(),
            Some(Type::I32),
            Some(Expr::Lit(Literal::Int(2))),
        );
        assert_eq!(stmt, expected);
    }

    #[test]
    fn test_statement_let_mut_ty_expr() {
        let stmt: Statement = parse("let mut a: i32 = 2");
        let expected = Statement::Let(
            Mutable(true),
            "a".to_string(),
            Some(Type::I32),
            Some(Expr::Lit(Literal::Int(2))),
        );
        assert_eq!(stmt, expected);
    }

    #[test]
    fn test_statement_let() {
        let stmt: Statement = parse("let a");
        let expected = Statement::Let(Mutable(false), "a".to_string(), None, None);
        assert_eq!(stmt, expected);
    }

    #[test]
    fn test_statement_assign() {
        let stmt: Statement = parse("a = false");
        let expected = Statement::Assign(
            Expr::Ident("a".to_string()),
            Expr::Lit(Literal::Bool(false)),
        );
        assert_eq!(stmt, expected);
    }

    #[test]
    fn test_statement_array_assign() {
        let stmt: Statement = parse("a[2] = false");
        let expected = Statement::Assign(
            Expr::bin_op(
                BinOp::Get,
                Expr::Ident("a".to_string()),
                Expr::Lit(Literal::Int(2)),
            ),
            Expr::Lit(Literal::Bool(false)),
        );
        assert_eq!(stmt, expected);
    }

    #[test]
    fn test_statement_while() {
        let stmt: Statement = parse("while a {}");
        let expected = Statement::While(
            Expr::Ident("a".to_string()),
            Block {
                statements: vec![],
                semi: false,
            },
        );
        assert_eq!(stmt, expected);
    }

    #[test]
    fn test_statement_expr() {
        let stmt: Statement = parse("a");
        println!("stmt {:?}", stmt);
        assert_eq!(stmt, Statement::Expr(Expr::Ident("a".to_string())));
    }

    // Trying to parse these statements should fail.

    #[test]
    fn fail_tests() {
        assert_parse_fail::<Statement>("let a i32;");
        assert_parse_fail::<Statement>("let a: I32;");
        assert_parse_fail::<Statement>("let a: i32 == 3;");
        assert_parse_fail::<Statement>("let 123;");
        assert_parse_fail::<Statement>("let 123: i32;");
        assert_parse_fail::<Statement>("123_var = 3;");
        assert_parse_fail::<Statement>("while true { let x }"); // Should fail since a block cannot end with a let statement (see test_statements_in_block function)
        assert_parse_fail::<Statement>("while {}");
        // NOTE: we could also test something like "123 = 3", but we will want to allow the
        // left-hand side to be an expression (such as `xs[0] = 3`). So checking what kinds of
        // expressions are allowed on the left of an assignment would require a bit more work and
        // is probably best done by the type checker or VM.
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                           Block Type                                          #
//?#                                                                                               #
//?#################################################################################################

#[cfg(test)]
mod test_statements_in_block {

    fn dummy() -> bool {
        true
    }

    //* What this function shows is that the 'let' statement is the only one that can not terminate a block
    //* Moreover, the only possibility for a return value of a block is to use an expression as the last statement (or an assignment)
    fn testing_blocks() -> i32 {
        // Testing what rust compiler's accepts as the last statement of a block
        let c = 3;
        let mut d = false;
        {
            let a = 2; // Can not remove ";" here (Let)
        }
        let z = {
            let mut a = 1;
            a = a + 1 // Can remove ";" here (Assign)
        };
        d = {
            c >= 2 // Can remove ";" here (Expr) - if correct type with what us before the block
        }; // But need semi-colon here then
        {
            d = true // Can remove ";" here (Assign)
        }
        d = {
            let f = d || c > 2; // Can't remove ";" here (Let)
            f // Can remove ";" here (Expr)
        };
        {
            while false {
                // Can never return a value
            } // Can remove ";" here (While)
        }
        {
            let dummy_var = 1;
            fn f() {} // Can remove ";" here (Fn)

            // fn f(a: i32) { a + 1 } // Can't define two functions with the same name at the same scope.
        }
        d = {
            dummy() // Can remove ";" here (Expr)
        }; // Semi-colon is mandatory here since it is not the last statement

        let mut h = {
            let b1 = 1;
            let mut b2 = 4;
            b2 = b1 + b2 // Can remove ";" here (Assign), so can be the last statement
        };

        h = {
            let b1 = 5;
            let mut b2 = 6;
            b2 = b1 - b2 // Can remove ";" here (Assign), so can be the last statement
        };

        let e = {
            if d {
                1
            } else {
                -1
            }
        };

        {
            {
                2
            } // returns 2
        }; // returns 2
           // The block above is an expression, needs ; if not the last statement

        // Defining an function and verifying its scope
        fn test() -> bool {
            let temp = true;
            // No closure makes it impossible to use variables from the previous scope,
            // so functions act like new VMs with correctly defined initial values.
            // (!d && temp)
            !temp
        }

        e + if !d { 1 } else { 3 } // Expression
    }
}

#[cfg(test)]
mod test_parse_block {
    use super::*;

    #[test]
    fn test_block_expr_fail() {
        let ts: proc_macro2::TokenStream = "{ let a = }".parse().unwrap();
        let stmt: Result<Statement> = syn::parse2(ts);
        println!("stmt {:?}", stmt);
        assert!(stmt.is_err());
    }

    #[test]
    fn test_block_semi() {
        let ts: proc_macro2::TokenStream = "
        {
            let a : i32 = 1;
            a = 5;
            a + 5;
        }"
        .parse()
        .unwrap();
        let bl: Block = syn::parse2(ts).unwrap();
        println!("bl {:?}", bl);
        println!("bl (pretty printing)\n{}", bl);
        assert_eq!(bl.statements.len(), 3);
        assert!(bl.semi);
    }

    #[test]
    fn test_block_no_semi() {
        let ts: proc_macro2::TokenStream = "
        {
            let a : i32 = 1;
            a = 5;
            a + 5
        }"
        .parse()
        .unwrap();
        let bl: Block = syn::parse2(ts).unwrap();
        println!("bl {:?}", bl);
        println!("bl (pretty printing)\n{}", bl);
        assert_eq!(bl.statements.len(), 3);
        assert!(!bl.semi);
    }

    #[test]
    fn test_block_fn() {
        let ts: proc_macro2::TokenStream = "
        {
            let a : i32 = 1;
            fn t() {}
            a = 5;
            a + 5
        }"
        .parse()
        .unwrap();
        let bl: Block = syn::parse2(ts).unwrap();
        println!("bl {:?}", bl);
        println!("bl (pretty printing)\n{}", bl);
        assert_eq!(bl.statements.len(), 4);
        assert!(!bl.semi);
    }

    #[test]
    fn test_block_while() {
        let ts: proc_macro2::TokenStream = "
        {
            let a : i32 = 1;
            while true {}
            a = 5;
            a + 5
        }"
        .parse()
        .unwrap();
        let bl: Block = syn::parse2(ts).unwrap();
        println!("bl {:?}", bl);
        println!("bl (pretty printing)\n{}", bl);
        assert_eq!(bl.statements.len(), 4);
        assert!(!bl.semi);
    }

    #[test]
    fn test_block2() {
        let ts: proc_macro2::TokenStream = "{ let b : bool = false; b = true }".parse().unwrap();
        let bl: Block = syn::parse2(ts).unwrap();
        println!("bl {:?}", bl);
        println!("bl (pretty printing)\n{}", bl);
        assert_eq!(bl.statements.len(), 2);
        assert!(!bl.semi);
    }

    #[test]
    fn test_expr_block() {
        let ts: proc_macro2::TokenStream = "
        {
            12
        }
        "
        .parse()
        .unwrap();
        println!("{:?}", ts);
        let e: Expr = syn::parse2(ts).unwrap();
        println!("e {:?}", e);
        println!("e (pretty printing)\n{}", e);
    }

    #[test]
    fn test_block_fail() {
        let ts: proc_macro2::TokenStream = "{ let a = 1 a = 5 }".parse().unwrap();
        let bl: Result<Block> = syn::parse2(ts);
        println!("bl {:?}", bl);

        assert!(bl.is_err());
    }
}

//?#################################################################################################
//?#                                                                                               #
//?#                                           Prog Type                                           #
//?#                                                                                               #
//?#################################################################################################

#[cfg(test)]
mod test_parse_prog {
    use super::*;

    #[test]
    fn test_prog() {
        let ts: proc_macro2::TokenStream = "
        fn a(a: i32) { let b = 5; a + b }
        fn b() -> i32 { 3 }

        fn main() {
            let c : i32 = 1;
            let mut d : i32 = 2;
            d = a(c + b());
            println!(\"{}\", d);
        }
        "
        .parse()
        .unwrap();
        let pr: Result<Prog> = syn::parse2(ts);
        let prog = pr.unwrap();
        println!("prog: {:?}", prog);
        println!("prog (pretty printing)\n{}", prog);
    }

    #[ignore = "Requires references to be implemented"]
    #[test]
    fn test_ref_de_ref() {
        let ts: proc_macro2::TokenStream = "
        fn main() {
            let a = &1;
            let mut a = &mut 1;
            *a = *a + 1;
            println!(\"{}\", *a);
        }
        "
        .parse()
        .unwrap();
        let pr: Result<Prog> = syn::parse2(ts);
        let prog = pr.unwrap();
        println!("prog: {:?}", prog);
        println!("prog (pretty printing)\n{}", prog);
    }
}
