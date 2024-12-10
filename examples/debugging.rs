use rnr::ast::{Block, Expr, Prog, Type};
use mips::rf::Reg::*;

use rnr::common::parse_mips;

fn main() {
    // Get the resulting mips
    let mips = parse_mips::<Block>(
    "
    {
        let a = 2;
        let b = {
            let b = a; // This should be 2
            let a = 3;
            a + b // This is 5
        };

        a + b // This is 7
    }
    "
    ).unwrap();
    
    // Check the result of the mips
    assert_eq!(mips.rf.get(t0), 7);

    println!("Result = {}", mips.rf.get(t0))
}
