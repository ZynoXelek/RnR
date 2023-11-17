use crate::ast::{
    Expr::{self, *},
    Literal, Op,
};

// As we seen, the simple parse implementation for Expr
// renders us a right associative representation of Expr
// and thus its evaluation does not adhere to the
// mathematical intuition of expressions.
//
// There are many ways to fix this problem, among these
// precedence climbing is a common approach, see
// https://en.wikipedia.org/wiki/Operator-precedence_parser
//
// parse_expression_1(lhs, min_precedence)
//     lookahead := peek next token
//     while lookahead is a binary operator whose precedence is >= min_precedence
//         op := lookahead
//         advance to next token
//         rhs := parse_primary ()
//         lookahead := peek next token
//         while lookahead is a binary operator whose precedence is greater
//                  than op's, or a right-associative operator
//                  whose precedence is equal to op's
//             rhs := parse_expression_1 (rhs, min_precedence + 1)
//             lookahead := peek next token
//         lhs := the result of applying op with operands lhs and rhs
//     return lhs
//
// In order to implement the algorithm for our Expr
// we first want to turn the right associated graph to
// to a flat vector of elements ExprItems.
#[derive(Debug)]
enum ExprItems {
    Op(Op),
    Lit(Literal),
}

impl Op {
    // The operator priority
    // Mul/Div has higher priority (binds harder) than Add/Sub
    pub fn priority(&self) -> u8 {
        match self {
            Op::Add => 0,
            Op::Sub => 0,
            Op::Mul => 1,
            Op::Div => 1,
            Op::And => 0,
            Op::Or => 0,
            Op::Eq => 2,
            Op::Lt => 2,
            Op::Gt => 2,
        }
    }
}

impl ExprItems {
    fn get_op(&self) -> Op {
        match self {
            ExprItems::Op(op) => *op,
            _ => panic!(),
        }
    }
    fn get_lit(&self) -> Literal {
        match self {
            ExprItems::Lit(lit) => *lit,
            _ => panic!(),
        }
    }
}

// Flattens an Expr into a vector of ExprItems
fn to_vec(e: Expr) -> Vec<ExprItems> {
    match e {
        Lit(l) => vec![ExprItems::Lit(l)],
        BinOp(op, l, r) => {
            let mut r = to_vec(*r);
            match *l {
                Lit(l) => {
                    r.push(ExprItems::Op(op));
                    r.push(ExprItems::Lit(l));
                    r
                }
                // should never occur due to the Expr structure
                _ => unreachable!(),
            }
        }
        // not yet implemented
        _ => unimplemented!(),
    }
}

use std::iter::{Peekable, Rev};

// A Peekable iterator allows allows to peek next item
// Rev reverses the scanning order
// std::slice::Iter is the iterator implementation for Vec
type Scanner<'a> = Peekable<Rev<std::slice::Iter<'a, ExprItems>>>;

fn peek_precedence<F>(scanner: &mut Scanner, f: F) -> bool
where
    F: Fn(u8) -> bool,
{
    if let Some(ExprItems::Op(op)) = scanner.peek() {
        f(op.priority())
    } else {
        false
    }
}

// A one-to-one implementation of the "wikipedia" algorithm.
fn climb_rec(mut lhs: Expr, min_precedence: u8, scanner: &mut Scanner) -> Expr {
    while peek_precedence(scanner, |op_precedence| op_precedence >= min_precedence) {
        // op := lookahead
        let op: Op = scanner.next().unwrap().get_op();
        // advance to next token
        // rhs := parse_primary ()
        let mut rhs: Expr = scanner.next().unwrap().get_lit().into();
        // while lookahead is a binary operator whose precedence is greater
        //                  than op's, or a right-associative operator
        //                  whose precedence is equal to op's
        while peek_precedence(scanner, |op_precedence| op_precedence > op.priority()) {
            // rhs := parse_expression_1 (rhs, min_precedence + 1)
            rhs = climb_rec(rhs, min_precedence + 1, scanner);
            // lookahead := peek next token
            // scanner will be updated since we passed it recursively
        }
        // lhs := the result of applying op with operands lhs and rhs
        lhs = BinOp(op, Box::new(lhs), Box::new(rhs));
    }
    lhs
}

// A trampoline to start of the precedence climbing
pub fn climb(e: Expr) -> Expr {
    // flatten the Expr into a vector
    let v: Vec<ExprItems> = to_vec(e);
    // turn the vector into a Scanner
    let mut scanner = v.iter().rev().peekable();
    // take the first literal
    let lhs: Expr = scanner.next().unwrap().get_lit().into();
    // now call the climber
    climb_rec(lhs, 0, &mut scanner)
}

#[test]
fn climb_test1() {
    let ts: proc_macro2::TokenStream = "2 - 3 - 4 - 5".parse().unwrap();
    let e: Expr = syn::parse2(ts).unwrap();
    println!("e {:?}", e);
    let e = climb(e);
    println!("e {:?}", e);
    // println!("evaluation {:?}", e.eval());
    // assert_eq!(e.eval(), Literal::Int(2 - 3 - 4 - 5));
}

#[test]
fn climb_test2() {
    let ts: proc_macro2::TokenStream = "2 - 3 * 4 - 5".parse().unwrap();
    let e: Expr = syn::parse2(ts).unwrap();
    println!("e {:?}", e);
    let e = climb(e);
    println!("e {:?}", e);
    // println!("evaluation {:?}", e.eval());
    // assert_eq!(e.eval(), Literal::Int(2 - 3 * 4 - 5));
}

#[test]
fn climb_test3() {
    let ts: proc_macro2::TokenStream = "4 - 5 - 2 * 8 * 3 - 1 - 2 * 5".parse().unwrap();
    let e: Expr = syn::parse2(ts).unwrap();
    println!("e {:?}", e);
    let e = climb(e);
    println!("e {:?}", e);
    // println!("evaluation {:?}", e.eval());
    // assert_eq!(e.eval(), Literal::Int(4 - 5 - 2 * 8 * 3 - 1 - 2 * 5));
}

#[test]
fn climb_test4() {
    let ts: proc_macro2::TokenStream = "8 - 7 - 6 * 5 - 4 * 3".parse().unwrap();
    let e: Expr = syn::parse2(ts).unwrap();
    println!("e {:?}", e);
    let e = climb(e);
    println!("e {:?}", e);
    // println!("evaluation {:?}", e.eval());
    // assert_eq!(e.eval(), Literal::Int(8 - 7 - 6 * 5 - 4 * 3));
}
