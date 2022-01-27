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

