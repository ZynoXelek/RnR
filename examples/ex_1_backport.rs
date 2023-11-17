use d7050e_lab4::ast::{Block, Expr, Statement};
use d7050e_lab4::climb::climb;

fn main() {}

// Test of while
#[test]
fn test_expr_stmt_while() {
    let ts: proc_macro2::TokenStream = "
    while true {
        let a: i32 = 5;
        a = 5;
        a;
    }"
    .parse()
    .unwrap();
    let e: Statement = syn::parse2(ts).unwrap();
    println!("{}", e);
}

// Test of display
#[test]
fn test_expr_stmt_let() {
    let ts: proc_macro2::TokenStream = "let a: i32 = 5 + a".parse().unwrap();
    let e: Statement = syn::parse2(ts).unwrap();
    println!("{}", e);
}

#[test]
fn test_expr_stmt() {
    let ts: proc_macro2::TokenStream = "a + 1 + (5 - 5) * 8".parse().unwrap();
    let e: Statement = syn::parse2(ts).unwrap();
    println!("{}", e);
}

#[test]
fn test_expr_assign() {
    let ts: proc_macro2::TokenStream = "a = 1 + a".parse().unwrap();
    let e: Statement = syn::parse2(ts).unwrap();
    println!("{}", e);
}

#[test]
fn test_expr_assign_fail() {
    let ts: proc_macro2::TokenStream = "a = 1 + false".parse().unwrap();
    let e: Statement = syn::parse2(ts).unwrap();
    println!("{}", e);
}

#[test]
fn test_block() {
    let ts: proc_macro2::TokenStream = "
    {
        let a: i32 = 0;
        let a: bool = false;
        a
    }"
    .parse()
    .unwrap();
    let e: Block = syn::parse2(ts).unwrap();
    println!("{}", e);
}

// And the optional assignments from lab3.
// (If you did not yet do them, you can do them now or later)

// Hint, you should NOT alter the AST
// Instead create a nested `if` using the existing AST.
// The `else if` is just syntactic sugar!
#[test]
fn test_else_if() {
    let ts: proc_macro2::TokenStream = "
    {
        if false {
            2;
        } else if true {
            3 + 5;
        } else if false {
            5;
        }
    }
    "
    .parse()
    .unwrap();
    let bl: Block = syn::parse2(ts).unwrap();
    println!("bl {:?}", bl);
    // assert suitable condition
}

// Optional: Extending AST with unary `!` operator
// (Later you will need unary operators to capture * dereferencing)
#[test]
fn test_not() {
    let ts: proc_macro2::TokenStream = "false || !true".parse().unwrap();
    let e: Expr = syn::parse2(ts).unwrap();
    println!("e {:?}", e);
    let e = climb(e);
    // println!("e {:?}", e);
    // println!("evaluation {:?}", e.eval());
    // assert_eq!(e.eval(), Literal::Bool(false || !true));
}
