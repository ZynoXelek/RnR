use mips::vm::Mips;

use crate::error::{TypeError, EvalError, Error};

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

pub trait GetMips {
    fn get_mips(&self) -> Result<Mips, Error>; //TODO: Use custom error
}

pub fn parse<T1>(s: &str) -> T1
where
    T1: syn::parse::Parse + std::fmt::Display,
{
    let ts: proc_macro2::TokenStream = s.parse().unwrap();
    let r: T1 = syn::parse2(ts).unwrap();
    println!(" --- Parsing --- ");
    println!("{}", r);
    println!(" --- End Parsing --- ");
    r
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
    mips.run();

    Ok(mips)
}
