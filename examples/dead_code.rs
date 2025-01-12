// Dead code: This function is never called
fn unused_function(a: i32, b: i32) -> i32 {
    a + b
}

fn some_used_function(a: i32, b: bool) -> i32 {
    a + if b { 1 } else { 0 }
}

fn main() {
    let x: i32 = 10;
    let y: i32 = 20;
    let flag: bool = true;

    // Dead code: This function is never called
    fn another_unused_function(j: i32) -> bool {
        j <= 0
    }

    // Dead code: This variable is never used
    let unused_variable: i32 = 42;

    let z = some_used_function(x, flag);

    println!("x: {}, y: {}, flag: {} --> z = {}", x, y, flag, z);

    // Dead code: This variable is never used
    let unused_flag: bool = false;
}
