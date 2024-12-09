use std::fs;
use std::collections::HashMap;
use std::collections::HashSet;

fn main() {
    //let input = fs::read_to_string("test_input.txt").unwrap();
    let input = fs::read_to_string("input.txt").unwrap();

    let mut iterator = input.split("\n\n");
    let ordering = iterator.next().unwrap();
    let updates = iterator.next().unwrap();

    let mut ordering_map: HashMap<u32, HashSet<_>> = HashMap::new();
    ordering.split("\n")
        .filter(|s| {s.len() >0})
        .map(|s| {
            let mut it = s.split("|");
            (it.next().unwrap().parse::<u32>().unwrap(),
             it.next().unwrap().parse::<u32>().unwrap())
        }).for_each(|(k, v)| {
            //println!("k: {}, v: {}", k, v);
            ordering_map.entry(k).and_modify(|h| {h.insert(v);}).or_insert(HashSet::from([v]));
        });

    let updates: Vec<_> = updates.split("\n")
        .filter(|s| {s.len() > 0})
        .map(|item| {
        item.split(",").map(|s| s.parse::<u32>().unwrap()).collect::<Vec<_>>()
    }).collect();

//    println!("ordering_map: {:?}", &ordering_map);
//    println!("updates: {:?}", &updates);

    let correct_updates: Vec<_> = updates.into_iter().filter(|update| {
        let mut idx_range = 0..update.len();
        //println!("range: {:?}", idx_range);

        let all = idx_range.all(|idx| {
            let head = update[idx];
            let tail  = &update[idx+1..];
            //println!("  head: {:?}", head);
            //println!("  tail: {:?}", tail);

            tail.iter().all(|page| {
                match ordering_map.get(&head) {
                    None => false,
                    Some(dict) => dict.contains(page),
                }
            })
        });

        //println!("all: {}", all);
        all
    }).collect();

    let result1: u32 = correct_updates.iter().map(|update| {
        let mid_idx = (update.len() / 2) as usize;
        let retval = update[mid_idx];
        println!("corr: {:?}, mid: {}, retval: {}", update, mid_idx, retval);
        retval
    }).sum();

    println!("Result1: {}", result1);
}
