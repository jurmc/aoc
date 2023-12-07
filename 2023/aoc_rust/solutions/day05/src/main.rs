use std::fs;
use std::collections::HashMap;

fn main() {
    let input = fs::read_to_string("test_input_day05.txt").unwrap();

    let mut input_it = input.lines();
    let seeds: Vec<_> = input_it.next()
        .unwrap()
        .split(':').nth(1).unwrap()
        .split_whitespace()
        .map(|c_s| c_s.parse::<u32>().unwrap())
        .collect();
    input_it.next();
    println!("seeds: {:?}", seeds);

    let mut maps: HashMap<&str, Vec<_>> = HashMap::new();
    let mut curr_label = "";
    while let Some(line) = input_it.next() {
        if line.contains("map") {
            let label = line.split_whitespace().nth(0).unwrap();
            let data_for_map_entry: Vec<Vec<u32>> = Vec::new();
            maps.insert(&label, data_for_map_entry);
            curr_label = &label;
        } else if line.is_empty() {
            continue;
        } else {
            let values: Vec<_> = line.split_whitespace()
                .map(|s| s.parse::<u32>().unwrap())
                .collect();
            //let mut v: &mut Vec<Vec<u32>> = maps.get(&curr_label).unwrap();
            //v.push(values);
            maps.entry(curr_label).and_modify(|v| v.push(values));
            println!("values: {:?}", line);
        }
    }
   
    maps.iter().for_each(|item| println!("item: {:?}", item));
}
