use std::fs;
use std::collections::HashMap;

fn main() {
    //let bytes = fs::read("test_input_day03.txt").unwrap();
    let bytes = fs::read("input_day03.txt").unwrap();
    let mut matrix: Vec<_> = bytes.split(|b| *b == b'\n')
        .filter(|slice| slice.len() > 0)
        .map(|slice| {
            let mut v = slice.to_vec();
            v.insert(0, b'.');
            v.push(b'.');
            v
        })
        .collect();
    let orig_y_len = matrix.len();
    let orig_x_len = matrix[0].len();

    let mut empty_row: Vec<_> = (0..orig_x_len).map(|_| b'.').collect();
    matrix.insert(0, empty_row.clone());
    matrix.push(empty_row);
    //matrix.iter().for_each(|row| println!("{:?}", row));

    let matrix_with_coords: Vec<_> = matrix.iter().enumerate().map(|(y, row)| {
        let new_row: Vec<_> = row.iter().enumerate().map(|(x, val)| {
            (x, y, *val)
        }).collect();
        new_row
    }).collect();
    //matrix_with_coords.iter().for_each(|row| println!("row: {:?}", row));

    let mut all_coords: HashMap<(usize, usize), u8> = HashMap::new();
    matrix_with_coords.iter().for_each(|row| {
        row.iter().for_each(|(x, y, val)| {all_coords.insert((*x,*y), *val);})
    });
    all_coords.iter().for_each(|item| println!("item: {:?}", item));

    let maybe_gears: Vec<_> = all_coords.iter()
        .filter(|((_, _), val)| *(*val) == b'*').collect();
    println!("maybe_gears: {:?}", maybe_gears);

    let mut nums_vec: Vec<Vec<u32>> = Vec::new();
    maybe_gears.iter().for_each(|((x,y), v)| {
        let n = neighbours(x,y);
        let n_filtered: Vec<_> = n.iter()
            .filter(|coords| {
                let v = all_coords.get(coords).unwrap();
                //println!("v: {}", v);
                v.is_ascii_digit()
            }).collect();
        println!("neighours_filtered: {:?}", n_filtered);

        let nums = get_nums_from_area(Vec::new(), n_filtered, &all_coords);
        println!("nums: {:?}", nums);
        nums_vec.push(nums);
    });

    let result: u32 = nums_vec.into_iter().filter(|nums| {
        nums.len() == 2
    }).map(|nums| {
        nums[0] * nums[1]
    }).sum();

    println!("result: {}", result);
//    nums_vec.iter().for_each(|vec| {
//        println!("vec: {:?}", vec);
//    });
}

fn get_nums_from_area(
    mut acc: Vec<u32>,
    mut area: Vec<&(usize, usize)>,
    all_coords: &HashMap<(usize, usize), u8>) -> Vec<u32> {

    if area.len() == 0 {
        return acc;
    }

    let mut cur_num: String = String::new();
    let cur_coords = area.pop().unwrap();
    let (cur_x, cur_y) = cur_coords;
    let cur_char = *all_coords.get(&cur_coords).unwrap() as char;
    cur_num.push(cur_char);

    let mut coords_to_remove_from_area = Vec::new();
    // Go rigtht and collect
    let mut next_x = cur_x + 1;
    loop {
       let next_char = all_coords.get(&(next_x, *cur_y)).unwrap(); 
       if next_char.is_ascii_digit() {
           cur_num.push(*next_char as char);
           coords_to_remove_from_area.push((next_x, *cur_y));
       } else {
           break;
       }
       next_x += 1;
    }
    // Go left and collect
    let mut next_x = cur_x - 1;
    loop {
       let next_char = all_coords.get(&(next_x, *cur_y)).unwrap(); 
       if next_char.is_ascii_digit() {
           cur_num.insert(0, *next_char as char);
           coords_to_remove_from_area.push((next_x, *cur_y));
       } else {
           break;
       }
       next_x -= 1;
    }
    let cur_num: u32 = cur_num.parse().unwrap();
    println!("Our number: {}", cur_num);
    acc.push(cur_num);

    let new_area: Vec<&(usize, usize)> = area.into_iter().filter(|coord| {
        for to_rem in coords_to_remove_from_area.iter() {
            if &to_rem == coord {
                return false;
            }
        }
        true
    }).collect();

    return get_nums_from_area(acc, new_area, all_coords);
//
//    vec![123, 456]
}

fn neighbours(x: &usize, y: &usize) -> Vec<(usize, usize)> {
    let x = *x;
    let y = *y;
    vec![
        (x-1, y-1), (x, y-1), (x+1, y-1),
        (x-1,   y),           (x+1,   y),
        (x-1, y+1), (x, y+1), (x+1, y+1)]
}

//fn main() {
//    let input = fs::read_to_string("test_input_day03.txt").unwrap();
//    //let input = fs::read_to_string("input_day03.txt").unwrap();
//    let input: Vec<_> = input.lines().collect();
//    let max_line = input.len()-1;
//    let max_idx = input[0].len()-1;
//
//    let part1_vec = input.iter().enumerate().fold(
//        Vec::new(),
//        |mut acc, (n, line)| {
//            let numbers: Vec<_> = extract_numbers(0, *line);
//            numbers.iter().for_each(|number| acc.push((n, number.clone())));
//            acc
//        });
//
//    let sum: u32 = part1_vec.iter().filter(|(idx, numbers)| {
//        //println!("idx: {}, numbers: {:?}", idx, numbers);
//        let neighbours = get_neighbours(*idx, &input, numbers);
//
//        let is_part = neighbours.find(|c| c != '.');
//        let result = match is_part {
//            Some(_) => true,
//            None => false,
//        };
//        //println!("result: {}, is_part: {:?}, neighbours: {}", result, is_part, neighbours);
//        result
//    }).map(|(idx, numbers)|{
//        let (start_pos, len, value) = numbers;
//        //println!("filtered: idx: {}, numbers: {:?}", idx, numbers);
//        //println!("value: {}", value);
//        value
//    }).sum();
//
//    println!("Part1 sum: {}", sum);
//
//    // Collect gears: Vec<(line,idx)>
//    let neighours: Vec<_>= input.iter().enumerate().fold(
//        Vec::new(),
//        |mut acc, (n, line)| {
//            let mut gears: Vec<_> = line.chars().enumerate()
//                .filter(|(idx, c)| *c == '*')
//                .map(|(idx, c)| ((n, idx), c)).collect();
//            acc.append(&mut gears);
//            acc
//        }).iter().map(|((line, idx), _)| {
//        // Get all neighours from (line, idx)
//        let line = *line as i32;
//        let idx = *idx as i32;
//        let neighbours: Vec<_> = vec![
//            (line -1, idx-1), (line -1, idx), (line -1, idx+1),
//            (line,    idx-1),                 (line,    idx+1),
//            (line +1, idx-1), (line +1, idx), (line +1, idx+1)]
//                .into_iter().filter(|(x, y)| {
//                    if *x < 0 || *y < 0 || *x > max_line as i32 || *y > max_idx as i32 {
//                        false
//                    } else {
//                        true
//                    }
//                }).collect();
//        neighbours
//    }).collect();
//
//    neighours.iter().for_each(|item| println!("item: {:?}", item));
//
//    // Find all number that has at least part within neighours area
//    let result = get_numbers(input, &neighours[0], Vec::new());
//    result.iter().for_each(|item| println!("item: {:?}", item));
//}

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

