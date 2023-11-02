use d7050e_lab4::{
    ast::{Block, Type},
    type_check::{check_block, TypeEnv},
};

fn main() {}

// Type check arrays. Introduction of usize and as T for primitive types.
// Extend AST, parser, type checker and EBNF.
//

// Array [T,n], is polymorphic to T and requires n: usize
// Indexing operator [i], requires i: usize
#[test]
fn test_array1() {
    let ts: proc_macro2::TokenStream = "
    {
        let a: [i32;5] = [1, 2, 3, 4, 5];     
                
        a[0]               
    }
    "
    .parse()
    .unwrap();
    let bl: Block = syn::parse2(ts).unwrap();
    println!("bl {}", bl);
    let ty = check_block(bl, TypeEnv::new());

    assert_eq!(ty.unwrap(), Type::I32);
}

// Array [T,n], T and n, can be inferred
#[test]
fn test_array2() {
    let ts: proc_macro2::TokenStream = "
    {
        let a = [1, 2, 3, 4, 5];     
                
        a[0]               
    }
    "
    .parse()
    .unwrap();
    let bl: Block = syn::parse2(ts).unwrap();
    println!("bl {}", bl);
    let ty = check_block(bl, TypeEnv::new());

    assert_eq!(ty.unwrap(), Type::I32);
}

// Array [T,n] can be initialized, where the n is assigned to all indexes
#[test]
fn test_array2() {
    let ts: proc_macro2::TokenStream = "
    {
        let a = [42; 2];     
                
        a[0]               
    }
    "
    .parse()
    .unwrap();
    let bl: Block = syn::parse2(ts).unwrap();
    println!("bl {}", bl);
    let ty = check_block(bl, TypeEnv::new());

    assert_eq!(ty.unwrap(), Type::I32);
}

// Array [T,n] can be mutated by index
#[test]
fn test_array2() {
    let ts: proc_macro2::TokenStream = "
    {
        let mut a = [42; 2]; 

        a[0] = 1337;    
                
        a[0]               
    }
    "
    .parse()
    .unwrap();
    let bl: Block = syn::parse2(ts).unwrap();
    println!("bl {}", bl);
    let ty = check_block(bl, TypeEnv::new());

    assert_eq!(ty.unwrap(), Type::I32);
}
