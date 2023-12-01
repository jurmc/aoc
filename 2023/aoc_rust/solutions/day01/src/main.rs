use std::fs;
use std::collections::HashMap;

fn main() {
    let input = read_file_into_vec_string("input_day01.txt");
    let test_input: Vec<_> = input.iter().collect();
    println!("test_input: {:?}", test_input);

    let first_col = input.clone().into_iter()
        //.map(|s| str_to_digit_after(s))
        .map(|s| get_first_digit(s));
        //.collect();
    let second_col = input.into_iter()
        //.map(|s| str_to_digit_after(s))
        .map(|s| s.chars().rev().collect::<String>())
        .map(|s| get_first_digit(s));
        //.collect();
    let both_cols: Vec<u32> = first_col.zip(second_col)
        .map(|(c1,c2)| c1 + c2.as_str())
        .map(|s| s.parse().unwrap())
        .collect();

    //println!("first_col: {:?}", first_col);
    //println!("second_col: {:?}", second_col);
    println!("both_cols: {:?}", both_cols);
    println!("result: {:?}", both_cols.iter().sum::<u32>());
}

fn get_first_digit(mut s: String) -> String {
    let digits = vec!['1', '2', '3', '4', '5', '6', '7', '8', '9']; 
    let idx = s.find(|c: char| digits.contains(&c)).unwrap();
    String::from(s.get(idx..idx+1).unwrap())
}

fn str_to_digit_after(mut s: String) -> String {
    let keys = vec!["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]; 
    let vals = vec!["1", "2", "3", "4", "5", "6", "7", "8", "9"]; 
    let map: HashMap<_,_> = keys.into_iter().zip(vals.into_iter()).collect();

    map.iter().for_each(|(k,v)| {
        println!("k: {}, v: {}", k, v);
        let seeking_for_len = k.len();
        match s.find(k) {
            Some(idx) => {
                s.insert_str(idx+seeking_for_len, v);
                println!("idx: {}", idx);
            },
            None => println!("Not found"),
        }
    });

    println!("s in: {}", s);
    println!("s out: {}", s);
    s
}

pub fn not_empty(s: &str) -> bool {
    for c in s.chars() {
        match c {
            ' ' => continue,
            '\n' => continue,
            '\t' => continue,
            _ => return true,
        }
    }
    return false;
}

pub fn read_file_into_vec_string(file_name: &str) -> Vec<String> {
    let file_content = fs::read_to_string(file_name).unwrap();
    let mut v: Vec<String> = Vec::new();
    for line in file_content.lines() {
        if not_empty(line) {
            v.push(String::from(line));
        }
    }
    v
}

