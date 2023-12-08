use std::fs;
use std::collections::HashMap;
use std::str::Lines;

fn main() {
    let input = fs::read_to_string("test_input_day05.txt").unwrap();
    let lines = input.lines();
    let (seeds, order, maps) = prepare_input(lines);
    let test_result = solve1(&seeds, &order, &maps);
    let input = fs::read_to_string("input_day05.txt").unwrap();
    let lines = input.lines();
    let (seeds, order, maps) = prepare_input(lines);
    let result = solve1(&seeds, &order, &maps);

    println!("test result: {}", test_result);
    println!("result: {}", result);
}

fn solve1(seeds: &Vec<u64>, order: &Vec<&str>, maps: &HashMap<&str, Vec<Vec<u64>>>) -> u64 {
    let mut mapping_applied = seeds.clone();
    order.iter().for_each(|label| {
        let mapping_def = maps.get(label).unwrap();
        let new_mappings_applied: Vec<_>  = mapping_applied.iter()
            .map(|num| {
                let mut new_val = *num;
                let mut it = mapping_def.iter();
                while let Some(map) = it.next() {
                    let dst_range_start = map[0];
                    let src_range_start = map[1];
                    let range_len = map[2];
                    if src_range_start <= *num && *num <= src_range_start + range_len {
                        new_val = dst_range_start + (num - src_range_start);
                        break;
                    }
                }
                new_val
        }).collect();
        mapping_applied = new_mappings_applied.clone();
    });

    let result = mapping_applied.iter().min().unwrap();
    *result
}

fn prepare_input(mut lines: Lines) -> (Vec<u64>, Vec<&str>, HashMap<&str, Vec<Vec<u64>>>) {
    let seeds: Vec<_> = lines.next()
        .unwrap()
        .split(':').nth(1).unwrap()
        .split_whitespace()
        .map(|c_s| c_s.parse::<u64>().unwrap())
        .collect();
    lines.next();
    //println!("seeds: {:?}", seeds);

    let mut order: Vec<&str> = Vec::new();
    let mut maps: HashMap<&str, Vec<_>> = HashMap::new();
    let mut curr_label = "";
    while let Some(line) = lines.next() {
        if line.contains("map") {
            let label = line.split_whitespace().nth(0).unwrap();
            let data_for_map_entry: Vec<Vec<u64>> = Vec::new();
            order.push(&label);
            maps.insert(&label, data_for_map_entry);
            curr_label = &label;
        } else if line.is_empty() {
            continue;
        } else {
            let values: Vec<_> = line.split_whitespace()
                .map(|s| s.parse::<u64>().unwrap())
                .collect();
            //let mut v: &mut Vec<Vec<u64>> = maps.get(&curr_label).unwrap();
            //v.push(values);
            maps.entry(curr_label).and_modify(|v| v.push(values));
            //println!("values: {:?}", line);
        }
    }
    (seeds, order, maps)
}
