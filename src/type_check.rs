use std::collections::HashMap;

use crate::ast::{Block, Expr, Literal, Op, Statement, Type};

// So let's implement a type checker
// Here we go!!!!

type TypeErr = String;

pub type TypeEnv = HashMap<String, Type>;

// unify
// attempts to unify two types (got and expected)
// on success, the unified type is returned
// on failure, an expression type error is returned
#[allow(dead_code)]
fn unify(got: Type, expected: Type) -> Result<Type, TypeErr> {
    match got == expected {
        true => Ok(expected),
        false => Err(format!("expected type {:?}, got type {:?}", expected, got)),
    }
}

#[test]
fn test_unify() {
    let t = unify(Type::I32, Type::I32).unwrap();
    println!("{}", t);
    assert_eq!(t, Type::I32);
}

#[test]
fn test_unify_fail() {
    let t = unify(Type::I32, Type::Bool);
    println!("{:?}", t);
    assert!(t.is_err());
}

// op_types
// returns types as: (expected left, expected right, result)
#[allow(dead_code)]
fn op_type(op: Op) -> (Type, Type, Type) {
    match op {
        Op::Add => (Type::I32, Type::I32, Type::I32),
        Op::Sub => (Type::I32, Type::I32, Type::I32),
        Op::Mul => (Type::I32, Type::I32, Type::I32),
        _ => todo!(),
    }
}

// check_expr
// recursively checks an expression for type correctness
// on success: the expression type is returned
// on failure, an expression type error is returned
pub fn check_expr(e: Expr, env: &TypeEnv) -> Result<Type, TypeErr> {
    match e {
        Expr::Ident(id) => match env.get(&id) {
            Some(t) => Ok(*t),
            None => Err("variable not found".to_string()),
        },
        Expr::Lit(Literal::Int(_)) => Ok(Type::I32),
        Expr::Lit(Literal::Bool(_)) => Ok(Type::Bool),
        Expr::Lit(Literal::Unit) => Ok(Type::Unit),

        #[allow(unused_variables)]
        Expr::BinOp(op, l, r) => {
            todo!()
        }

        #[allow(unused_variables)]
        Expr::Par(e) => todo!(),

        #[allow(unused_variables)]
        Expr::IfThenElse(cond, t, e) => {
            todo!()
        }
    }
}

#[test]
fn test_id() {
    let ts: proc_macro2::TokenStream = "a".parse().unwrap();
    let e: Expr = syn::parse2(ts).unwrap();
    let mut env = TypeEnv::new();
    env.insert("a".to_string(), Type::I32);
    let ty = check_expr(e, &env).unwrap();
    assert_eq!(ty, Type::I32);
}

#[test]
fn test_lit_i32() {
    let ts: proc_macro2::TokenStream = "123".parse().unwrap();
    let e: Expr = syn::parse2(ts).unwrap();
    let env = TypeEnv::new();
    let ty = check_expr(e, &env).unwrap();
    assert_eq!(ty, Type::I32);
}

#[test]
fn test_expr() {
    let ts: proc_macro2::TokenStream = "a + 1 + (5 - 5) * 8".parse().unwrap();
    let e: Expr = syn::parse2(ts).unwrap();
    println!("{}", e);
    let mut env = TypeEnv::new();
    env.insert("a".to_string(), Type::I32);
    let ty = check_expr(e, &env).unwrap();
    assert_eq!(ty, Type::I32);
}

#[test]
fn test_expr_if_then_else() {
    let ts: proc_macro2::TokenStream = "
    if true { false } else { b }
    "
    .parse()
    .unwrap();
    let e: Expr = syn::parse2(ts).unwrap();
    println!("{}", e);
    let mut env = TypeEnv::new();
    env.insert("b".to_string(), Type::Bool);
    let ty = check_expr(e, &env).unwrap();
    assert_eq!(ty, Type::Bool);
}

#[allow(unused_variables)]
pub fn check_stmt(stmt: Statement, env: &mut TypeEnv) -> Result<Type, TypeErr> {
    #[allow(unreachable_code)]
    Ok(match stmt {
        #[allow(unused_variables)]
        Statement::Let(id, t, e) => {
            // let a: i32 = 5 + 2
            // for now just accept an ident
            todo!()
        }
        #[allow(unused_variables)]
        Statement::Expr(e) => {
            // the type of an Expr is returned
            todo!()
        }
        #[allow(unused_variables)]
        Statement::Assign(id, e) => {
            // a = 5
            todo!()
        }
        #[allow(unused_variables)]
        Statement::While(e, b) => {
            todo!()
        }
    })
}

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
    let mut env = TypeEnv::new();
    // env.insert("a".to_string(), Type::I32);
    let ty = check_stmt(e, &mut env).unwrap();
    assert_eq!(ty, Type::Unit);
}

#[test]
fn test_expr_stmt_let() {
    let ts: proc_macro2::TokenStream = "let a: i32 = 5 + a".parse().unwrap();
    let e: Statement = syn::parse2(ts).unwrap();
    println!("{}", e);
    let mut env = TypeEnv::new();
    env.insert("a".to_string(), Type::I32);
    let ty = check_stmt(e, &mut env).unwrap();
    assert_eq!(ty, Type::Unit);
}

#[test]
fn test_expr_stmt() {
    let ts: proc_macro2::TokenStream = "a + 1 + (5 - 5) * 8".parse().unwrap();
    let e: Statement = syn::parse2(ts).unwrap();
    println!("{}", e);
    let mut env = TypeEnv::new();
    env.insert("a".to_string(), Type::I32);
    let ty = check_stmt(e, &mut env).unwrap();
    assert_eq!(ty, Type::I32);
}

#[test]
fn test_expr_assign() {
    let ts: proc_macro2::TokenStream = "a = 1 + a".parse().unwrap();
    let e: Statement = syn::parse2(ts).unwrap();
    println!("{}", e);
    let mut env = TypeEnv::new();
    env.insert("a".to_string(), Type::I32);
    let ty = check_stmt(e, &mut env).unwrap();
    assert_eq!(ty, Type::Unit);
}

#[test]
fn test_expr_assign_fail() {
    let ts: proc_macro2::TokenStream = "a = 1 + false".parse().unwrap();
    let e: Statement = syn::parse2(ts).unwrap();
    println!("{}", e);
    let mut env = TypeEnv::new();
    env.insert("a".to_string(), Type::I32);
    let ty = check_stmt(e, &mut env);
    assert!(ty.is_err());
}

#[allow(unused_assignments)]
pub fn check_block(b: Block, env: TypeEnv) -> Result<Type, TypeErr> {
    // we can extend the environment, but also see outer environment
    let mut env = env;

    #[allow(unused_variables)]
    let mut return_ty = Type::Unit;
    for stmt in b.statements {
        // update the return type for each iteration
        return_ty = check_stmt(stmt, &mut env)?;
    }

    todo!()
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
    let mut env = TypeEnv::new();
    env.insert("a".to_string(), Type::I32);
    let ty = check_block(e, env).unwrap();
    assert_eq!(ty, Type::Bool);
}

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
