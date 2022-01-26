use std::fs;

fn main() {
    let data = read_data("input.dat");
    println!("Solution 1 for day2: {}", solve1_day2(data));

    let data = read_data("input.dat");
    println!("Solution 2 for day2: {}", solve2_day2(data));
}

fn not_empty(s: &str) -> bool {
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

fn read_data(file_name: &str) -> Vec<Vec<String>> {
    let file_content = fs::read_to_string(file_name).unwrap();
    let mut v: Vec<Vec<String>> = Vec::new();
    for line in file_content.lines() {
        if not_empty(line) {
            let mut record: Vec<String> = Vec::new();
            for word in line.split_whitespace() {
                record.push(String::from(word));
            }
            v.push(record);
        }
    }
    v
}

fn solve1_day2(data: Vec<Vec<String>>) -> i64{
    let mut acc_forward: i64 = 0;
    let mut acc_depth: i64 = 0;

    for record in data {
        let mut it = record.iter();
        let direction = it.next().unwrap();
        let value = it.next().unwrap().parse::<i64>().unwrap();

        match direction.as_str() {
            "forward" => acc_forward += value,
            "down" => acc_depth += value,
            "up" => acc_depth -= value,
            _ => panic!("don't know what to do"),
        }
    }

    acc_forward * acc_depth
}

fn solve2_day2(data: Vec<Vec<String>>) -> i64{
    let mut acc_aim: i64 = 0;
    let mut acc_forward: i64 = 0;
    let mut acc_depth: i64 = 0;

    for record in data {
        let mut it = record.iter();
        let direction = it.next().unwrap();
        let value = it.next().unwrap().parse::<i64>().unwrap();

        match direction.as_str() {
            "forward" => {
                acc_forward += value;
                acc_depth += acc_aim * value;
            },
            "down" => acc_aim += value,
            "up" => acc_aim -= value,
            _ => panic!("don't know what to do"),
        }
    }

    println!("acc_aim: {}", acc_aim);
    println!("acc_forward: {}", acc_forward);
    println!("acc_depth: {}", acc_depth);

    acc_forward * acc_depth
}

#[test]
fn check_input_star1() {
    let data = read_data("test-input.dat");
    assert_eq!(150, solve1_day2(data));

    let data = read_data("input.dat");
    assert_eq!(1694130, solve1_day2(data));
}

#[test]
fn check_input_star2() {
    let data = read_data("test-input.dat");
    assert_eq!(900, solve2_day2(data));
    assert_eq!(1, 0);
}
