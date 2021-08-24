use crate::ast::{Block, Expr, Literal, Op, Statement, Type};

use syn::{
    parse::{Parse, ParseStream},
    Result, Token,
};

impl Parse for Literal {
    fn parse(input: ParseStream) -> Result<Self> {
        // Use the "built in" syn parser for literals
        let l: syn::Lit = input.parse()?;

        Ok(match l {
            syn::Lit::Int(l) => Literal::Int(l.base10_parse().unwrap()),
            syn::Lit::Bool(b) => Literal::Bool(b.value),
            // for now only Int and Bool are covered
            _ => unimplemented!(),
        })
    }
}

#[test]
fn parse_lit_int() {
    let ts: proc_macro2::TokenStream = "1".parse().unwrap();
    let l: Literal = syn::parse2(ts).unwrap();
    assert_eq!(l, Literal::Int(1));
}

#[test]
fn parse_lit_bool_false() {
    let ts: proc_macro2::TokenStream = "false".parse().unwrap();
    let l: Literal = syn::parse2(ts).unwrap();
    assert_eq!(l, Literal::Bool(false));
}

#[test]
fn parse_lit_fail() {
    let ts: proc_macro2::TokenStream = "a".parse().unwrap();
    let l: Result<Literal> = syn::parse2(ts);
    assert!(l.is_err());
}

impl Parse for Op {
    fn parse(input: ParseStream) -> Result<Self> {
        // check if next token is `+`
        if input.peek(Token![+]) {
            // consume the token
            let _: Token![+] = input.parse()?;
            Ok(Op::Add)
        } else if input.peek(Token![-]) {
            let _: Token![-] = input.parse()?;
            Ok(Op::Sub)
        } else if input.peek(Token![*]) {
            let _: Token![*] = input.parse()?;
            Ok(Op::Mul)
        } else if input.peek(Token![/]) {
            let _: Token![/] = input.parse()?;
            Ok(Op::Div)
        } else if input.peek(Token![&&]) {
            let _: Token![&&] = input.parse()?;
            Ok(Op::And)
        } else if input.peek(Token![||]) {
            let _: Token![||] = input.parse()?;
            Ok(Op::Or)
        } else if input.peek(Token![==]) {
            let _: Token![==] = input.parse()?;
            Ok(Op::Eq)
        } else if input.peek(Token![>]) {
            let _: Token![>] = input.parse()?;
            Ok(Op::Gt)
        } else if input.peek(Token![<]) {
            let _: Token![<] = input.parse()?;
            Ok(Op::Lt)
        } else {
            // to explicitly create an error at the current position
            input.step(|cursor| Err(cursor.error("expected operator")))
        }
    }
}

#[test]
fn parse_op_add() {
    let ts: proc_macro2::TokenStream = "+".parse().unwrap();
    let op: Op = syn::parse2(ts).unwrap();
    println!("op {:?}", op);
    assert_eq!(op, Op::Add);
}

#[test]
fn parse_op_mul() {
    let ts: proc_macro2::TokenStream = "*".parse().unwrap();
    let op: Op = syn::parse2(ts).unwrap();
    println!("op {:?}", op);
    assert_eq!(op, Op::Mul);
}

#[test]
fn parse_op_fail() {
    let ts: proc_macro2::TokenStream = "1".parse().unwrap();
    let err = syn::parse2::<Op>(ts);
    println!("err {:?}", err);
    assert!(err.is_err());
}

// Render a "right associative" AST
impl Parse for Expr {
    // Use a custom parser for expressions
    fn parse(input: ParseStream) -> Result<Self> {
        let left = if input.peek(syn::token::Paren) {
            // we have a left (Expr), e.g., "(1 + 2)"
            let content;
            let _ = syn::parenthesized!(content in input);
            let e: Expr = content.parse()?;
            Expr::Par(Box::new(e))
        } else if input.peek(syn::Ident) {
            // we have a left Ident, e.g, "my_best_ident_ever"
            let ident: syn::Ident = input.parse()?;
            Expr::Ident(ident.to_string())
        } else if input.peek(syn::token::If) {
            // we have a left conditional, e.g., "if true {1} else {2}" or
            // if true { 5 }
            let IfThenOptElse(c, t, e) = input.parse()?;
            Expr::IfThenElse(Box::new(c), t, e)
        } else {
            // else we require a left literal
            let left: Literal = input.parse()?;
            left.into()
        };
        // now check if right is an Op Expr
        match input.parse::<Op>() {
            Ok(op) => {
                let right: Expr = input.parse()?;
                Ok(Expr::bin_op(op, left, right))
            }
            // no op, just return the left, no error
            Err(_) => Ok(left),
        }
    }
}

//
// We want to parse strings like
// `if expr { then block }`
// and
// `if expr { then block } else { else block }
//
// The else arm is optional
struct IfThenOptElse(Expr, Block, Option<Block>);

impl Parse for IfThenOptElse {
    fn parse(input: ParseStream) -> Result<IfThenOptElse> {
        let _if: syn::token::If = input.parse()?;
        let cond_expr: Expr = input.parse()?;

        let then_block: Block = input.parse()?;

        if input.peek(syn::token::Else) {
            let _else: syn::token::Else = input.parse()?;

            let else_block: Block = input.parse()?;
            Ok(IfThenOptElse(cond_expr, then_block, Some(else_block)))
        } else {
            Ok(IfThenOptElse(cond_expr, then_block, None))
        }
    }
}

#[test]
fn test_expr_ident() {
    let ts: proc_macro2::TokenStream = "my_best_ident".parse().unwrap();
    println!("{:?}", ts);
    let e: Expr = syn::parse2(ts).unwrap();

    println!("e {:?}", e);

    assert_eq!(e, Expr::Ident("my_best_ident".to_string()));
}

#[test]
fn test_expr_if_then_else() {
    let ts: proc_macro2::TokenStream = "if a > 0 {1} else {2}".parse().unwrap();
    println!("{:?}", ts);
    let e: Expr = syn::parse2(ts).unwrap();

    println!("e {:?}", e);
}

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

#[test]
fn test_if_then_else_nested() {
    let ts: proc_macro2::TokenStream = "
    if false {
        2;
    } else {
        if true {
            3 + 5;
        }
    }"
    .parse()
    .unwrap();
    println!("{:?}", ts);
    let e: Expr = syn::parse2(ts).unwrap();

    println!("e {:?}", e);
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

#[test]
fn test_if_then_else_nested2() {
    let ts: proc_macro2::TokenStream = "
    if false {
        2;
    } else if true {
        3 + 5;
    }"
    .parse()
    .unwrap();
    println!("{:?}", ts);
    let e: Expr = syn::parse2(ts).unwrap();

    println!("e {:?}", e);
}

#[test]
fn test_expr_right() {
    let ts: proc_macro2::TokenStream = "2 - 4 - 5".parse().unwrap();
    let e: Expr = syn::parse2(ts).unwrap();
    println!("e {:?}", e);
}

#[test]
fn test_expr_par() {
    let ts: proc_macro2::TokenStream = "(2 - 4) - 5".parse().unwrap();
    let e: Expr = syn::parse2(ts).unwrap();
    println!("e {:?}", e);
}

#[test]
fn test_expr_mul() {
    let ts: proc_macro2::TokenStream = "2 * 4 - 5".parse().unwrap();
    let e: Expr = syn::parse2(ts).unwrap();
    println!("e {:?}", e);
}

#[test]
fn test_expr_par_mul() {
    let ts: proc_macro2::TokenStream = "(2 * 4) - 5".parse().unwrap();
    let e: Expr = syn::parse2(ts).unwrap();
    println!("e {:?}", e);
}

#[test]
fn test_expr_fail() {
    let ts: proc_macro2::TokenStream = "(2 * 4) - ".parse().unwrap();
    let e: Result<Expr> = syn::parse2(ts);
    assert!(e.is_err());
}

use quote::quote;

impl Parse for Type {
    fn parse(input: ParseStream) -> Result<Type> {
        // The syn::Type is very complex and overkill
        // Types in Rust involve generics, paths
        // etc., etc., etc. ...
        //
        // To make things simple, we just turn the syn::Type
        // to a token stream (`quote`) and turn that into a String
        // and turn that into an &str (`as_str`)
        let t: syn::Type = input.parse()?;
        let ts = quote! {#t}.to_string();
        match ts.as_str() {
            "i32" => Ok(Type::I32),
            "bool" => Ok(Type::Bool),
            "()" => Ok(Type::Unit),
            _ =>
            // to explicitly create an error at the current position
            {
                input.step(|cursor| Err(cursor.error("expected operator")))
            }
        }
    }
}

#[test]
fn test_type_i32() {
    let ts: proc_macro2::TokenStream = "i32".parse().unwrap();
    let e: Type = syn::parse2(ts).unwrap();
    assert_eq!(e, Type::I32);
}

#[test]
fn test_type_bool() {
    let ts: proc_macro2::TokenStream = "bool".parse().unwrap();
    let e: Type = syn::parse2(ts).unwrap();
    assert_eq!(e, Type::Bool);
}

#[test]
fn test_type_unit() {
    let ts: proc_macro2::TokenStream = "()".parse().unwrap();
    let e: Type = syn::parse2(ts).unwrap();
    assert_eq!(e, Type::Unit);
}

#[test]
fn test_type_fail() {
    let ts: proc_macro2::TokenStream = "u32".parse().unwrap();
    let e: Result<Type> = syn::parse2(ts);
    assert_eq!(e.is_err(), true);
}

impl Parse for Statement {
    fn parse(input: ParseStream) -> Result<Statement> {
        if input.peek(syn::token::Let) {
            // let a : u32 = 1 + 2
            let _let: syn::token::Let = input.parse()?;
            let left: Expr = input.parse()?;

            let _colon: syn::token::Colon = input.parse()?;

            let ty: Type = input.parse()?;

            let _eq: syn::token::Eq = input.parse()?;
            let right: Expr = input.parse()?;
            Ok(Statement::Let(left, ty, right))
        } else {
            // a = 1 + 2, or 1 + 5
            let left: Expr = input.parse()?;

            if input.peek(syn::token::Eq) {
                // a = 1 + 2
                let _eq: syn::token::Eq = input.parse()?;
                let right: Expr = input.parse()?;

                Ok(Statement::Assign(left, right))
            } else {
                // 1 + 5
                Ok(Statement::Expr(left))
            }
        }
    }
}

#[test]
fn test_block_expr_let() {
    let ts: proc_macro2::TokenStream = "let a: i32 = 2".parse().unwrap();
    let be: Statement = syn::parse2(ts).unwrap();
    println!("be {:?}", be);

    assert_eq!(
        be,
        Statement::Let(
            Expr::Ident("a".to_string()),
            Type::I32,
            Expr::Lit(Literal::Int(2))
        )
    );
}

#[test]
fn test_block_expr_assign() {
    let ts: proc_macro2::TokenStream = "a = false".parse().unwrap();
    let be: Statement = syn::parse2(ts).unwrap();
    println!("be {:?}", be);

    assert_eq!(
        be,
        Statement::Assign(
            Expr::Ident("a".to_string()),
            Expr::Lit(Literal::Bool(false))
        )
    );
}

#[test]
fn test_block_while() {
    let ts: proc_macro2::TokenStream = "while a {}".parse().unwrap();
    let be: Statement = syn::parse2(ts).unwrap();
    println!("be {:?}", be);

    assert_eq!(
        be,
        Statement::While(
            Expr::Ident("a".to_string()),
            Block {
                statements: vec![],
                semi: false
            }
        )
    );
}

#[test]
fn test_block_expr_expr() {
    let ts: proc_macro2::TokenStream = "a".parse().unwrap();
    let be: Statement = syn::parse2(ts).unwrap();
    println!("be {:?}", be);
    assert_eq!(be, Statement::Expr(Expr::Ident("a".to_string())));
}

use syn::punctuated::Punctuated;

// Here we take advantage of the parser function `parse_terminated`
impl Parse for Block {
    fn parse(input: ParseStream) -> Result<Block> {
        let content;
        let _ = syn::braced!(content in input);

        let bl: Punctuated<Statement, Token![;]> = content.parse_terminated(Statement::parse)?;

        // We need to retrieve the semi before we collect into a vector
        // as into_iter consumes the value.
        let semi = bl.trailing_punct();

        Ok(Block {
            // turn the Punctuated into a vector
            statements: bl.into_iter().collect(),
            semi,
        })
    }
}

#[test]
fn test_block_expr_fail() {
    let ts: proc_macro2::TokenStream = "{ let a = }".parse().unwrap();
    let be: Result<Statement> = syn::parse2(ts);
    println!("be {:?}", be);
    assert_eq!(be.is_err(), true);
}

#[test]
fn test_block1() {
    let ts: proc_macro2::TokenStream = "{ let a : i32 = 1; a = 5; a + 5; }".parse().unwrap();
    let bl: Block = syn::parse2(ts).unwrap();
    println!("bl {:?}", bl);
    assert_eq!(bl.statements.len(), 3);
    assert_eq!(bl.semi, true);
}

#[test]
fn test_block2() {
    let ts: proc_macro2::TokenStream = "{ let b : bool = false; b = true }".parse().unwrap();
    let bl: Block = syn::parse2(ts).unwrap();
    println!("bl {:?}", bl);
    assert_eq!(bl.statements.len(), 2);
    assert_eq!(bl.semi, false);
}

#[test]
fn test_block_fail() {
    let ts: proc_macro2::TokenStream = "{ let a = 1 a = 5 }".parse().unwrap();
    let bl: Result<Block> = syn::parse2(ts);
    println!("bl {:?}", bl);

    assert_eq!(bl.is_err(), true);
}
