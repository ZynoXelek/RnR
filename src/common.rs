use crate::error::EvalError;

pub trait Eval<T: Clone> {
    fn eval(&self) -> Result<T, EvalError>
    where
        T: Clone;
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
