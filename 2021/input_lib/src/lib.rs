use std::fs;

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

pub fn read_file_into_binary_matrix(file_name: &str) -> Vec<Vec<bool>> {
    let file_content = fs::read_to_string(file_name).unwrap();
    let mut v: Vec<Vec<bool>> = Vec::new();
    for line in file_content.lines() {
        let mut r: Vec<bool> = Vec::new();
        for c in line.chars() {
            match c {
                '0' => r.push(false),
                '1' => r.push(true),
                _ => panic!("Cannot happen!"),
            }
        }
        v.push(r);
    }
    v
}

pub fn dump_binary_matrix(data: &Vec<Vec<bool>>) {
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

pub fn read_file_into_vec_u32(file_name: &str) -> Vec<u32> {
    let file_content = fs::read_to_string(file_name).unwrap();
    let mut v: Vec<u32> = Vec::new();
    for line in file_content.lines() {
        if not_empty(line) {
            v.push(line.parse::<u32>().unwrap());
        }
    }
    v
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

pub fn read_data(file_name: &str) -> Vec<Vec<String>> {
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

