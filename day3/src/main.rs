use day3::read_file_into_vec_string;

fn main() {
    let data = read_file_into_vec_string("input.dat");
    println!("Solution 2 for day3: {}", solve1_day3(data));

    //let data = read_file_into_vec_string("test-input.dat");
    //println!("Solution 1 for day3: {}", solve2_day3(data));
}

fn bin_to_dec(v: Vec<u32>) -> u32 {
    let mut acc: u32 = 0;
    for (c, idx) in v.iter().rev().zip(0..v.len() as u32) {
        acc += c * 2_u32.pow(idx);
    }

   acc
}

fn convert(v: Vec<String>) -> Vec<u32> {
    let mut data: Vec<u32> = Vec::new();
    for record in v {
        if data.len() < record.len() {
            for _ in 0..record.len() {
                data.push(0);
            }

        }
        for (c, idx) in record.chars().zip(0..record.len()) {
            data[idx] += String::from(c).parse::<u32>().unwrap();
        }
    }
    data
}

fn solve1_day3(v: Vec<String>) -> u32 {
    let num_records = v.len() as u32;
    let data = convert(v);

    let mut gamma: Vec<u32> = Vec::new();
    let mut epsilon: Vec<u32> = Vec::new();
    for num in data {
        //println!("num: {}", num);
        if num > (num_records / 2) {
            gamma.push(1);
            epsilon.push(0);
        } else {
            gamma.push(0);
            epsilon.push(1);
        }
    }

    let gamma_dec = bin_to_dec(gamma);
    let epsilon_dec = bin_to_dec(epsilon);

    gamma_dec * epsilon_dec
}

//fn solve2_day3(v: Vec<String>) -> u32 {
//}

#[test]
fn check_solve1_day3() {
    let data = read_file_into_vec_string("test-input.dat");
    assert_eq!(198, solve1_day3(data));

    let data = read_file_into_vec_string("input.dat");
    assert_eq!(3374136, solve1_day3(data));
}

