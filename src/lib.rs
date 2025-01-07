#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_imports)]

// Strive to keep your code free of warnings
// Eventually you should be able to deny unused_variables, dead_code etc.

// common definitions
pub mod common;
pub mod error;
pub mod test_util;

// AST related
pub mod ast;
pub mod ast_traits;

pub mod parse;

pub mod type_check;

pub mod optimize;

pub mod vm;

pub mod backend;
pub mod asm_parse;

// intrinsics
pub mod intrinsics;
