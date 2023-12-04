use std::fs;

fn main() {
    let input = fs::read_to_string("test_input_day03.txt").unwrap();
    //let input = fs::read_to_string("input_day03.txt").unwrap();
    let input: Vec<_> = input.lines().collect();
    let max_line = input.len()-1;
    let max_idx = input[0].len()-1;

    let part1_vec = input.iter().enumerate().fold(
        Vec::new(),
        |mut acc, (n, line)| {
            let numbers: Vec<_> = extract_numbers(0, *line);
            numbers.iter().for_each(|number| acc.push((n, number.clone())));
            acc
        });

    let sum: u32 = part1_vec.iter().filter(|(idx, numbers)| {
        //println!("idx: {}, numbers: {:?}", idx, numbers);
        let neighbours = get_neighbours(*idx, &input, numbers);

        let is_part = neighbours.find(|c| c != '.');
        let result = match is_part {
            Some(_) => true,
            None => false,
        };
        //println!("result: {}, is_part: {:?}, neighbours: {}", result, is_part, neighbours);
        result
    }).map(|(idx, numbers)|{
        let (start_pos, len, value) = numbers;
        //println!("filtered: idx: {}, numbers: {:?}", idx, numbers);
        //println!("value: {}", value);
        value
    }).sum();

    println!("Part1 sum: {}", sum);

    // Collect gears: Vec<(line,idx)>
    let neighours: Vec<_>= input.iter().enumerate().fold(
        Vec::new(),
        |mut acc, (n, line)| {
            let mut gears: Vec<_> = line.chars().enumerate()
                .filter(|(idx, c)| *c == '*')
                .map(|(idx, c)| ((n, idx), c)).collect();
            acc.append(&mut gears);
            acc
        }).iter().map(|((line, idx), _)| {
            // Get all neighours from (line, idx)
            let line = *line as i32;
            let idx = *idx as i32;
            let neighbours: Vec<_> = vec![
                (line -1, idx-1), (line -1, idx), (line -1, idx+1),
                (line,    idx-1),                 (line,    idx+1),
                (line +1, idx-1), (line +1, idx), (line +1, idx+1)]
                    .into_iter().filter(|(x, y)| {
                        if *x < 0 || *y < 0 || *x > max_line as i32 || *y > max_idx as i32 {
                            false
                        } else {
                            true
                        }
                    }).collect();
            neighbours
        }).collect();

        neighours.iter().for_each(|item| println!("item: {:?}", item));

        // Find all number that has at least part within neighours area
        let result = get_numbers(input, &neighours[0]);
        result.iter().for_each(|item| println!("item: {:?}", item));
}

fn get_numbers(input: Vec<&str>, neighours: &Vec<(i32, i32)>) -> Vec<String> {
    vec![String::from("abc"), String::from("def")]
}

fn get_neighbours(idx: usize, input: &Vec<&str>, numbers: &(usize, usize, u32)) -> String {
    let line_len = input[0].len();
    let (start_pos, len, value) = numbers;

    let lower_idx = match *start_pos > 0 {
        true => start_pos-1,
        false => 0,
    };
    let upper_idx = match *start_pos+*len < line_len {
        true => start_pos+len+1,
        false => line_len,
    };

    let mut result = String::new();
    if idx > 1 {
        result.push_str(&input[idx-1][lower_idx..upper_idx]);
    }
    if lower_idx < *start_pos {
        result.push_str(&input[idx][lower_idx..lower_idx+1]);
    }
    if upper_idx > *start_pos+*len && upper_idx <= line_len {
        result.push_str(&input[idx][upper_idx-1..upper_idx]);
    }
    if idx < input.len()-1 {
        result.push_str(&input[idx+1][lower_idx..upper_idx]);
    }
    result
}

// Vec(start_pos, len, value)
fn extract_numbers(cur_pos: usize, s: &str) -> Vec<(usize, usize, u32)> {
    match s.find(|c: char| c.is_digit(10)) {
        Some(beg_pos) => {
            let slice = &s[beg_pos..];
            let end_pos = slice.find(|c: char| !c.is_digit(10));
            let (vec_entry, len) = match end_pos {
                Some(end_pos) => {
                    let slice = &slice[..end_pos];
                    //println!("slice: {}, len: {}", slice, end_pos);
                    (slice.parse::<u32>().unwrap(), end_pos)
                },
                None => {
                    //println!("slice: {}, len: {}", slice, slice.len());
                    (slice.parse::<u32>().unwrap(), slice.len())
                },
            };

            let rest = &s[beg_pos+len..];
            //println!("vec_entry: {:?}, len: {}", vec_entry, len);
            //println!("rest: {}", &s[beg_pos+len..]);
            if rest.len() > 0 {
                let mut rest_vec: Vec<_> = extract_numbers(cur_pos+beg_pos+len, rest);
                let mut vec: Vec<_> = vec![(cur_pos+beg_pos, len, vec_entry)];
                vec.append(&mut rest_vec);
                return vec;
            } else {
                return vec![(cur_pos+beg_pos, len, vec_entry)];
            }
            // TODO: recursively call for next num occurences
        }
        None => return vec![],
    }
}

