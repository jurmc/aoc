use std::fs;
use std::collections::HashSet;

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    //let input = fs::read_to_string("test_input.txt").unwrap();

    let rows: Vec<Vec<i32>> = input.lines().map(|line| {
        let row: Vec<i32> = line.split_whitespace().map(|i| i.parse::<i32>().unwrap()).collect();
        row
    }).collect();


    let (safe_rows, unsafe_rows): (Vec<_>, Vec<_>) = rows.iter().partition(|row| {
        let mut direction = row[1] - row[0];
        if direction > 0 {
            direction = 1;
        } else if direction < 0 {
            direction = -1;
        }
        let is_row_safe = row.windows(2).all(|win| {
            let mut curr_diff = win[1] - win[0];
            let curr_abs_diff = i32::abs(curr_diff);

            if curr_diff * direction < 0 {
                return false;
            }

            if curr_abs_diff > 3 || curr_abs_diff < 1 {
                return false;
            }

            true
        });
        is_row_safe
    });

    println!("Result1: {}", &safe_rows.len());
    println!("Unsafe rows: {}", &unsafe_rows.len());
}
