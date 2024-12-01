use std::fs;
use std::collections::HashMap;

fn main() {
    let input =  fs::read_to_string("input.txt").unwrap();

    let cnt = input.lines().count();
    let mut l_col: Vec<i32> = Vec::with_capacity(cnt);
    let mut r_col: Vec<i32> = Vec::with_capacity(cnt);

    let mut l_occ: HashMap<i32, i32> = HashMap::with_capacity(cnt);
    let mut r_occ: HashMap<i32, i32> = HashMap::with_capacity(cnt);

    for line in input.lines() {
        let mut split = line.split_whitespace();

        let l = String::from(split.next().unwrap());
        let r = String::from(split.next().unwrap());

        let l = l.parse::<i32>().unwrap();
        let r = r.parse::<i32>().unwrap();

        l_col.push(l);
        r_col.push(r);

        l_occ.entry(l).and_modify(|cnt| *cnt += 1).or_insert(1);
        r_occ.entry(r).and_modify(|cnt| *cnt += 1).or_insert(1);
    }

    l_col.sort();
    r_col.sort();

    let mut acc1 = 0;
    let mut acc2 = 0;
    for (l, r) in l_col.iter().zip(r_col.iter()) {
        acc1 += <i32>::abs(l - r);

        let similarity = r_occ.entry(*l).or_insert(0);
        acc2 += l * * similarity;
    }

    println!("acc1: {}", acc1);
    println!("acc2: {}", acc2);
}
