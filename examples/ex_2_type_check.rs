use d7050e_lab4::{
    ast::{Block, Type},
    type_check::{check_block, TypeEnv},
};

fn main() {}

// Type checking
#[test]
fn test_check_if_then_else_shadowing() {
    let ts: proc_macro2::TokenStream = "
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
    "
    .parse()
    .unwrap();
    let bl: Block = syn::parse2(ts).unwrap();
    println!("bl {}", bl);
    let ty = check_block(bl, TypeEnv::new());

    assert_eq!(ty.unwrap(), Type::I32);
}
