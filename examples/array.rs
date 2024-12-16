// An example program to try to parse using RNR
// It uses an array

fn gen_array() -> [i32; 3] {
    [1, 2, 3]
}

fn f(arr: [i32; 3]) -> i32 {
    arr[0] * 3 + arr[1] / 2 - arr[2]
}

fn main() {
    let arr = gen_array();
    let res = f(arr);
    println!("{}", res);
}
