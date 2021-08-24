use d7050e_lab4::{
    ast::{Block, Type},
    type_check::{check_block, TypeEnv},
};

fn main() {}

// Type check, optional assignment.
// Extend AST, parser, type checker and EBNF.
//
// Require variables to be declared mut for assignments.
#[test]
fn test_mut() {
    let ts: proc_macro2::TokenStream = "
    {
        let mut a: i32 = 1 + 2;     
        a = a - 1;       
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

// Immutable variables should not be assignable.
#[test]
fn test_mut_fail() {
    let ts: proc_macro2::TokenStream = "
    {
        let a: i32 = 1 + 2;     
        a = a - 1; // <- should fail      
        a       
    }
    "
    .parse()
    .unwrap();
    let bl: Block = syn::parse2(ts).unwrap();
    println!("bl {}", bl);
    let ty = check_block(bl, TypeEnv::new());

    assert_eq!(ty.is_err(), true);
}

// Rust allows late assignment of declared variables.
#[test]
fn test_late_assign() {
    let ts: proc_macro2::TokenStream = "
    {
        let a: i32;     
        a = 1;       
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

// Rust allows late assignment of declared variables with type inference.
#[test]
fn test_late_assign_inference() {
    let ts: proc_macro2::TokenStream = "
    {
        let a;     
        a = 1;       
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
