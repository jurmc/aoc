use std::fs;

fn main() {
    let input = fs::read_to_string("test_input.txt").unwrap();

    println!("input: {}", input);
}
