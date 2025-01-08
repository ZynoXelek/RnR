use mips::{instrs::Instrs, vm::Mips};

use crate::error::{Error, EvalError, TypeError};

pub trait EvalType<T: Clone> {
    fn eval_type(&self) -> Result<T, TypeError>
    where
        T: Clone;
}

pub trait Eval<T: Clone> {
    fn eval(&self) -> Result<T, EvalError>
    where
        T: Clone;
}

pub trait Optimize<T: Clone> {
    fn optimize(&self) -> Result<T, Error> //TODO: Use custom error
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

    //? Debug
    // eprintln!("Token stream is:\n{}", ts);

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
    T1: syn::parse::Parse + std::fmt::Display + GetMips,
{
    let bl = parse::<T1>(s);
    let mut mips = bl.get_mips()?;

    // Evaluate it
    _ = mips.run();

    Ok(mips)
}

// Useful to test the code generation without risking to run into an infinite loop
pub fn parse_mips_no_run<T1>(s: &str) -> Result<Mips, Error>
where
    T1: syn::parse::Parse + std::fmt::Display + GetMips,
{
    let bl = parse::<T1>(s);
    let mips = bl.get_mips()?;

    // Do not evaluate it yet

    Ok(mips)
}
