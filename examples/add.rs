// An example program to try to parse using RNR

fn add(a: i32, b: i32) -> i32 {
    a + b
}

fn main() {
    let i = 1;
    let j = 2;
    let k = add(i, j);
    println!("i = {}, j = {} --> k = {}", i, j, k);
}
