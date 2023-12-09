use std::fs;
use std::collections::HashMap;

fn main() {
    //let input = fs::read_to_string("test_input_day08.txt").unwrap();
    let input = fs::read_to_string("input_day08.txt").unwrap();
    println!("input: {}", input);

    let mut lines = input.lines();
    let directions = lines.next().unwrap();
    let _ = lines.next();
    let map: HashMap<_,_> = lines.map(|entry| {
        let mut it = entry.split('=');
        let key = it.next().unwrap().trim();
        let mut lr_str: String = it.next().unwrap().trim().to_string();
        lr_str.pop(); lr_str.remove(0);
        let mut lr_it = lr_str.split(',').map(|s| s.trim());
        let left = lr_it.next().unwrap();
        let right = lr_it.next().unwrap();
        (key, (String::from(left), String::from(right)))
    }).collect();

    println!("directions: {}", directions);
    map.iter().for_each(|(key, val)| {
        println!("key: {}, val: {:?}", key, val);
    });

    let mut path_it = Path::new(directions);
    let mut cur_pos = "AAA";
    let dest_pos = "ZZZ";
    let mut steps_no = 0;
    loop {
        if cur_pos == dest_pos {
            break;
        }
        steps_no += 1;
        let dir = path_it.next().unwrap();
        //println!("cur_pos: {}, dir: {}", cur_pos, dir);
        let (left, right) = map.get(cur_pos).unwrap();
        cur_pos = match dir {
            'L' => left,
            _ => right,
        };
        //println!(" next_pos: {}", cur_pos);
    }

    println!("step no: {}", steps_no);
}

struct Path {
    path: String,
    pos: usize,
}

impl Path {
    fn new(path: &str) -> Path {
        Path {
           path: String::from(path),
           pos: 0,
        }
    }
}

impl Iterator for Path {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.path.as_bytes()[self.pos] as char;
        self.pos += 1;
        if self.pos == self.path.len() {
            self.pos = 0;
        }
        Some(result)
    }
}
