use d7050e_lab4::{
    ast::{Block, Type},
    type_check::{check_block, TypeEnv},
};

fn main() {}

// Type checking, optional assignment.
// Extend AST, parser, type checker and EBNF.
//
// Rust can typically infer the type of variables.
#[test]
fn test_inference1() {
    let ts: proc_macro2::TokenStream = "
    {
        let a = 1 + 2; // a:i32 = 3
        a              
    }
    "
    .parse()
    .unwrap();
    let bl: Block = syn::parse2(ts).unwrap();
    println!("bl {}", bl);
    let ty = check_block(bl, TypeEnv::new());

    assert_eq!(ty.unwrap(), Type::I32);
}

// You can even declare without type.
// Rust will infer the type on first use.
#[test]
fn test_inference2() {
    let ts: proc_macro2::TokenStream = "
    {
        let a;         // a declared
        a = 5;         // a must be of i32
        a        
    }
    "
    .parse()
    .unwrap();
    let bl: Block = syn::parse2(ts).unwrap();
    println!("bl {}", bl);
    let ty = check_block(bl, TypeEnv::new());

    assert_eq!(ty.unwrap(), Type::I32);
}

// Rust requires all variables to be assigned.
// before they are used.
#[test]
fn test_inference_fail() {
    let ts: proc_macro2::TokenStream = "
    {
        let a;         // a declared
        if a == 5 {};  // <- error, not assigned
        a        
    }
    "
    .parse()
    .unwrap();
    let bl: Block = syn::parse2(ts).unwrap();
    println!("bl {}", bl);
    let ty = check_block(bl, TypeEnv::new());

    assert_eq!(ty.is_error(), true);
}

// Rust requires all variables to be assigned
// before they are used.
#[test]
fn test_inference_fail() {
    let ts: proc_macro2::TokenStream = "
    {
        let a;         // a declared
        if a == 5 {};  // <- error, not assigned
        a        
    }
    "
    .parse()
    .unwrap();
    let bl: Block = syn::parse2(ts).unwrap();
    println!("bl {}", bl);
    let ty = check_block(bl, TypeEnv::new());

    assert_eq!(ty.is_error(), true);
}
