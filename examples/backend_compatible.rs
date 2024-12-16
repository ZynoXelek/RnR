// An example program to try to parse using RNR
// It is compatible to be used by the backend (no String nor macros)

fn add(a: i32, b: i32) -> i32 {
    a + b
}

fn main() {
    let a = 1;
    let b = 2;
    let _c = add(a, b);
}
