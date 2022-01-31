use day3::read_file_into_vec_string;

fn main() {
    let data = read_file_into_vec_string("input.dat");
    println!("Solution 2 for day3: {}", solve1_day3(data));

    println!("Solution 1 for day3: {}", solve2_day3("input.dat"));
}

fn get_bit_for_oxygen(data: &Vec<Vec<bool>>, bit_idx: usize) -> bool{
    let num_records = data.len() as i32;
    let num_true_bits = data.iter()
                            .map(|rec| rec[bit_idx] as i32)
                            .reduce(|acc, item| acc + item)
                            .unwrap();

    let num_false_bits = num_records - num_true_bits;
    let bit_for_oxygen = {
        if 2 * num_true_bits >= num_records { true }
        else                                { false}
    };
    bit_for_oxygen
}

fn convert_into_bool_matrix(data: &Vec<String>) -> Vec<Vec<bool>> {
    let mut converted_data: Vec<Vec<bool>> = Vec::new();
    for record in data {
        let new_rec: Vec<bool> = record.chars().map(|c| match c {
            '1' => true,
            '0' => false,
            _ => panic!("Cannot happen"),
        }).collect();
        converted_data.push(new_rec);
    }
    converted_data
}

fn dump_data(data: &Vec<Vec<bool>>) {
    for record in data {
        for field in record {
            print!("{} ", match field {
                true => 1,
                false => 0,
            });
        }
        println!("");
    }
    println!("----------------");
}

fn bin_to_dec_better(v: &Vec<bool>) -> u32 {
    let mut acc: u32 = 0;
    for (c, idx) in v.iter().map(|b| if b == &true {1} else {0}).rev().zip(0..v.len() as u32) {
        acc += c * 2_u32.pow(idx);
    }

   acc
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

fn solve2_day3(file: &str) -> i32 {
    // 1st half /////////////////////////////
    let mut oxygen_generator_rating: i32 = 0;
    let mut co2_scrubber_rating: i32 = 0;

    {
        let data = read_file_into_vec_string(file);
        let mut data = convert_into_bool_matrix(&data);
        let num_fields = data.iter().next().unwrap().len() as i32;

        let bit_idx_tmp = 0 as usize;

        dump_data(&data);
        for bit_idx in 0..num_fields {
            let bit_for_oxygen = get_bit_for_oxygen(&data, bit_idx as usize);
            data = data.into_iter()
                .filter(|record| record[bit_idx as usize] == bit_for_oxygen)
                .collect::<Vec<_>>();
            if data.len() == 1 {
                break;
            }
        }
        assert_eq!(1, data.len());
        dump_data(&data);
        oxygen_generator_rating = bin_to_dec_better(data.iter().next().unwrap()) as i32;
        println!("bin_to_dec_better: {}", oxygen_generator_rating);
    }

    // 2nd half /////////////////////////////

    {
        let data = read_file_into_vec_string(file);
        let mut data = convert_into_bool_matrix(&data);
        let num_fields = data.iter().next().unwrap().len() as i32;

        let bit_idx_tmp = 0 as usize;

        dump_data(&data);
        for bit_idx in 0..num_fields {
            let bit_for_co2 =  !get_bit_for_oxygen(&data, bit_idx as usize);
            data = data.into_iter()
                .filter(|record| record[bit_idx as usize] == bit_for_co2)
                .collect::<Vec<_>>();
            //dump_data(&data);
            if data.len() == 1 {
                break;
            }
        }
        assert_eq!(1, data.len());
        dump_data(&data);
        co2_scrubber_rating = bin_to_dec_better(data.iter().next().unwrap()) as i32;
        println!("bin_to_dec_better: {}", co2_scrubber_rating);
    }

    oxygen_generator_rating * co2_scrubber_rating
}

#[test]
fn check_solve1_day3() {
    let data = read_file_into_vec_string("test-input.dat");
    assert_eq!(198, solve1_day3(data));

    let data = read_file_into_vec_string("input.dat");
    assert_eq!(3374136, solve1_day3(data));
}

#[test]
fn check_solve2_day3() {
    assert_eq!(230, solve2_day3("test-input.dat"));

    assert_eq!(4432698, solve2_day3("input.dat"));
}

