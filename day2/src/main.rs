use input_lib::read_data;

fn main() {
    let data = read_data("input.dat");
    println!("Solution 1 for day2: {}", solve1_day2(data));

    let data = read_data("input.dat");
    println!("Solution 2 for day2: {}", solve2_day2(data));
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

    let data = read_data("input.dat");
    assert_eq!(1698850445, solve2_day2(data));
}
