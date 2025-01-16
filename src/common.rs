use mips::{error::Error as MipsError, instrs::Instrs, vm::Mips};

use crate::error::{Error, EvalError, TypeError};

// Made to evaluate the return type of an Expr, Statement, Block, etc...
pub trait EvalType<T: Clone> {
    fn eval_type(&self) -> Result<T, TypeError>
    where
        T: Clone;
}

// Made to verify that the Expr, Statement, Block, etc... is valid (types relative) and
// returns the same object but where let statements have their types annotated
pub trait CheckType<T: Clone> {
    fn check_type(&self) -> Result<T, TypeError>
    where
        T: Clone;
}

pub trait Eval<T: Clone> {
    fn eval(&self) -> Result<T, EvalError>
    where
        T: Clone;
}

pub trait Optimize<T: Clone> {
    fn optimize(&self) -> Result<T, Error>
    //TODO: Use custom error
    where
        T: Clone;
}

pub trait GetInstructions {
    fn get_instructions(&self) -> Result<Instrs, Error>; //TODO: Use custom error
}

pub trait GetMips {
    fn get_mips(&self) -> Result<Mips, Error>; //TODO: Use custom error
}

pub fn parse<T1>(s: &str) -> T1
where
    T1: syn::parse::Parse + std::fmt::Display,
{
    let ts_result = s.parse::<proc_macro2::TokenStream>();
    let ts: proc_macro2::TokenStream;
    match ts_result {
        Ok(t) => ts = t,
        Err(e) => {
            println!("Error in token stream parsing: {}", e);
            panic!("Error parsing the input token stream");
        }
    }

    let res = syn::parse2::<T1>(ts);
    match res {
        Ok(r) => {
            //? To avoid spamming the terminal during tests and final run through main
            // println!(" --- Parsing --- ");
            // println!("{}", r);
            // println!(" --- End Parsing --- ");
            r
        }
        Err(e) => {
            let typename = std::any::type_name::<T1>();
            println!("Error in type '{}' parsing: {}", typename, e);
            panic!("Error parsing from token stream to type '{}'", typename);
        }
    }
}

pub fn parse_type<T1, T2>(s: &str) -> Result<T2, TypeError>
where
    T1: syn::parse::Parse + std::fmt::Display + EvalType<T2>,
    T2: std::fmt::Debug + Clone,
{
    let bl = parse::<T1>(s);
    let v = bl.eval_type()?;
    println!("\nreturn {:?}", v);
    Ok(v)
}

pub fn parse_check_type<T1>(s: &str) -> Result<T1, TypeError>
where
    T1: syn::parse::Parse + std::fmt::Display + Clone + CheckType<T1>,
{
    let bl = parse::<T1>(s);
    let v = bl.check_type()?;
    println!("\nreturn {}", v);
    Ok(v)
}

pub fn parse_test<T1, T2>(s: &str) -> Result<T2, EvalError>
where
    T1: syn::parse::Parse + std::fmt::Display + Eval<T2>,
    T2: std::fmt::Debug + Clone,
{
    let bl = parse::<T1>(s);
    let v = bl.eval()?;
    println!("\nreturn {:?}", v);
    Ok(v)
}

pub fn parse_mips<T1>(s: &str) -> Result<Mips, Error>
where
    T1: syn::parse::Parse + std::fmt::Display + Clone + CheckType<T1> + GetMips,
{
    let bl = parse::<T1>(s);

    // Assert that it is valid code
    let bl = match bl.check_type() {
        Ok(b) => b,
        Err(e) => return Err(format!("Type check failed: {}", e.to_string())),
    };

    let mut mips = bl.get_mips()?;

    // Evaluate it
    match mips.run() {
        Ok(_) => (),
        Err(e) => match e {
            MipsError::Halt => (),
            _ => panic!("Unexpected running error: {:?}", e),
        },
    };

    Ok(mips)
}

// Useful to test the code generation without risking to run into an infinite loop
pub fn parse_mips_no_run<T1>(s: &str) -> Result<Mips, Error>
where
    T1: syn::parse::Parse + std::fmt::Display + Clone + CheckType<T1> + GetMips,
{
    let bl = parse::<T1>(s);

    // Assert that it is valid code
    let bl = match bl.check_type() {
        Ok(b) => b,
        Err(e) => return Err(format!("Type check failed: {}", e.to_string())),
    };

    let mips = bl.get_mips()?;

    // Do not evaluate it yet

    Ok(mips)
}
