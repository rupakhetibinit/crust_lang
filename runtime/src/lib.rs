#[unsafe(no_mangle)]
pub extern "C" fn print_i64(x: i64) {
    println!("{}", x);
}

#[unsafe(no_mangle)]
pub extern "C" fn print_f64(x: f64) {
    println!("{}", x);
}

#[unsafe(no_mangle)]
pub extern "C" fn print_bool(x: bool) {
    println!("{}", x);
}
