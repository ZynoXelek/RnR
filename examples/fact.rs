// An example program introducing a simple recursive factorial function

fn fact(n: i32) -> i32 {
    if n == 0 {
        1
    } else {
        n * fact(n - 1)
    }
}

fn main() {
    let n = 5;
    let a = fact(n);
    println!("{}! = {}", n, a);
}
