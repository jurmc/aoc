use std::fs;
use std::collections::HashSet;

fn is_row_safe(row: &Vec<i32>) -> bool {
    let diff_pattern = HashSet::from([1, 2, 3]);
    let diffs: HashSet<i32> = row.windows(2).map(|i| {i32::abs(i[0]-i[1])}).collect();
    diffs.is_subset(&diff_pattern)
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();

    let rows: Vec<Vec<i32>> = input.lines().map(|line| {
        let row: Vec<i32> = line.split_whitespace().map(|i| i.parse::<i32>().unwrap()).collect();
        row
    }).collect();

    let (only_sorted_rows, not_sorted): (Vec<_>, Vec<_>) = rows.into_iter().partition(|row| { row.is_sorted() || row.iter().rev().is_sorted()});

    let (proper_diff, mut not_safe) : (Vec<Vec<i32>>, Vec<Vec<i32>>) = only_sorted_rows.into_iter().partition(
        |row| {
            is_row_safe(&row)
        });

    let mut drops_at_first = not_sorted;
    drops_at_first.append(&mut not_safe);

    let proper_diff2: Vec<Vec<i32>> = drops_at_first.into_iter().filter(|row| {
        for idx in 0..row.len() {
            let mut new_row = row.clone();
            new_row.remove(idx);
            //print!(" new_row: {:?}", new_row);
            if is_row_safe(&new_row) {
                //println!(" safe");
                return true
            }
            //println!(" not safe");
        }
        false
    }).collect();

    println!("Result1: {}", proper_diff.len());
    println!("Result2: {}", proper_diff.len() + proper_diff2.len());
}
