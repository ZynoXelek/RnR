use crate::ast::{Block, Literal, Type};
use crate::common::Eval;
use crate::parse::{parse, try_parse};
use crate::vm::Val;
use syn::parse::Parse;

/// Takes a regular rust expression, turns it into a string, and then parses
/// it as an Expr and evaluates it.
#[macro_export]
macro_rules! test_expr {
    ( $expr:expr ) => {{
        let expected = $expr;
        let src = stringify!($expr);
        let expr: $crate::ast::Expr = $crate::parse::parse(src);
        let value: $crate::vm::Val = $crate::common::Eval::eval(&expr).unwrap();
        let result: $crate::ast::Literal = value.into();
        assert_eq!(result, $crate::ast::Literal::from(expected));
    }};
}
pub use test_expr;

/// Takes a string containing source code and assert that parsing it fails.
pub fn assert_parse_fail<T: Parse + std::fmt::Debug>(src: &str) {
    let result = try_parse::<T>(src);
    assert!(
        result.is_err(),
        "parsing `{}` into `{}` was supposed to fail, but was parsed as {:?}",
        src,
        std::any::type_name::<T>(),
        result.unwrap()
    );
}

/// Assert that evaluation (by VM) results in the expected value.
/// Does not perform type checking.
pub fn assert_value<T1, T2>(p: &T1, expected: T2)
where
    T1: Eval<Val>,
    T2: Into<Val>,
{
    let val = p.eval();
    println!("VM eval result: {:?}", val);
    assert!(val.is_ok());
    let val = val.unwrap();
    assert_eq!(val, expected.into());
}

/// Assert that type results in the expected type.
/// Does not perform value verification.
pub fn assert_type<T1, T2>(p: &T1, expected: T2)
where
    T1: Eval<Type>,
    T2: Into<Type>,
{
    let ty = p.eval();
    println!("Type checking result: {:?}", ty);
    assert!(ty.is_ok());
    let ty = ty.unwrap();
    assert_eq!(ty, expected.into());
}

/// Assert that `p` evaluates to the expected value **and** that the type returned by the type
/// checker matches the type of the expected value.
/// Expected is a primitive value, such as an i32 or bool.
pub fn assert_eval<T1, T2>(p: &T1, expected: T2)
where
    T1: Parse + Eval<Type> + Eval<Val>,
    T2: Into<Val> + Into<Type> + Copy,
{
    // Convert the expected value to a literal and a type.
    let expected_type: Type = expected.into();
    assert_type(p, expected_type); // NOTE: enable later for type checking!
    assert_value(p, expected);
}

/// Check that parsing the given string produces the expected AST object.
/// Takes a string containing some source code, parses it, and checks that it
/// matches the expected result.
/// Note: Does **not** rely on the Display trait implementation for `T` being
/// correct or even existing at all.
pub fn test_parse<T>(src: &str, expected: T)
where
    T: Parse + std::fmt::Debug + PartialEq,
{
    let p: T = parse(src);
    assert_eq!(p, expected)
}

/// Take a parsable and displayable object (some AST element like an `Expr` etc),
/// convert it to a string, and check that parsing the string produces the original object.
/// This function is convenient when manually constructing the AST is simple, such as
/// a literal, an operation, or a simple expression.
/// For more complicated ASTs, `test_parse_display(src)` may be more convenient.
/// Note: **Relies on implementations of both Display and Parse traits being correct!**
pub fn test_display_parse<T>(ast: T)
where
    T: Parse + std::fmt::Display + std::fmt::Debug + PartialEq,
{
    let src = ast.to_string();
    println!("Display string:\n{}", src);
    test_parse(&src, ast);
}

/// Takes raw/literal code and uses it to test parsing, type checking, eval (vm), and display.
/// The code is evaluated as-is and also converted to string form so that it can be parsed.
/// In particular, **the code can be tested, so that we know that we are feeding correct code
/// to the parser and that it evaluates to the expected result!**
/// (No need to have two copies of the code (as actual code and as a string) and keep the two
/// in sync.
///
/// The macro can be used in two different ways.
/// Either a **literal block** is passed in, in which case an expression can optionally be
/// passed as well. If it is, then the value of the block is checked against the expected value.
/// The source will be the direct String version of the code.
///
/// The other option is to **construct a block from statements**.
/// In that case, the first block contains tests (assertions about the statements), followed
/// by a sequence of statements, **terminated with a semicolon**, and finally the last
/// expression.
/// The semicolon separating the statements from the final expression is necessary to
/// resolve "local ambiguity".
#[macro_export]
macro_rules! test_block {
    ( $block:block $(, $expected:expr )? ) => {
        {
            // Evaluate the block.
            let result = $block;
            $(
                // If there is an expected value given, check that they match.
                let expected = $expected;
                assert_eq!(result, expected);
            )?
            // Convert to source String and parse, eval, etc.
            let src = stringify!($block);
            let block: Block = $crate::parse::parse(src);
            // assert_value(&block, result);
            assert_eval(&block, result); // Type checking and evaluation.
            test_display_parse(block);  // NOTE: comment out to disable Display testing
        }
    };
    ( $tests:block, $( $code:stmt ),+; $x:expr ) => {
        {
            // Put all the statements here.
            $( $code )*
            // And then the tests.
            $tests
            // Build the source code string for the statements...
            let mut ss: Vec<String> = Vec::new();
            $(
                ss.push(stringify!($code;).into());
            )*
            // ...putting the final expression last.
            ss.push(stringify!($x).into());
            // And make a block, i.e surround with { }.
            let src = format!("{{\n{}\n}}", ss.join("\n"));
            // Parse the source.
            //println!("Parsing source:\n{}", src); // Print the stringified code.
            let block: Block = $crate::parse::parse(&src);
            // Evaluate the final expression...
            let expected = $x;
            //...and use it to check that type checking and evaluation agrees.
            // assert_value(&block, expected);
            assert_eval(&block, expected); // Type checking and evaluation.
            // Convert the parsed AST to string and parse again, checking that
            // we get the same thing back.
            test_display_parse(block);  // NOTE: comment out to disable Display testing
        }
    };
}
pub use test_block;
