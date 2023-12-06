use std::fs;
use std::collections::HashSet;
use std::collections::HashMap;

fn main() {
    //let input = fs::read_to_string("test_input_day04.txt").unwrap();
    let input = fs::read_to_string("input_day04.txt").unwrap();

    let result: u32 = input.lines()
        .map(|line| {
            let line = line.split(':').nth(1).unwrap();
            let mut it = line.split('|');
            let wining_nums = it.next().unwrap().trim();
            let nums_i_have = it.next().unwrap().trim();

            let wining_nums: HashSet<_> = wining_nums.split_whitespace().map(|s| s.trim().parse::<u32>().unwrap()).collect();
            let nums_i_have: HashSet<_> = nums_i_have.split_whitespace().map(|s| s.trim().parse::<u32>().unwrap()).collect();

            let num_wining_i_have = wining_nums.intersection(&nums_i_have).count() as u32;
            let points = match num_wining_i_have {
                0 => 0u32,
                _ => 2u32.pow(num_wining_i_have-1),
            };
            //println!("points: {}, wining_nums: {:?} , nums_i_have: {:?}", points, wining_nums, nums_i_have);
            points
        }).sum();

    println!("result1: {}", result);

    let mut max_card_id = 0;
    let input2: HashMap<_,_> = input.lines()
        .map(|line| {
            let mut it = line.split(':');
            let card_no: u32 = it.next().unwrap().split_whitespace().nth(1).unwrap().parse().unwrap();

            let numbers = it.next().unwrap();
            let mut it = numbers.split('|');
            let wining_nums = it.next().unwrap().trim();
            let nums_i_have = it.next().unwrap().trim();

            let wining_nums: HashSet<_> = wining_nums.split_whitespace().map(|s| s.trim().parse::<u32>().unwrap()).collect();
            let nums_i_have: HashSet<_> = nums_i_have.split_whitespace().map(|s| s.trim().parse::<u32>().unwrap()).collect();

            //println!("points: {}, wining_nums: {:?} , nums_i_have: {:?}", points, wining_nums, nums_i_have);
            max_card_id = card_no;
            (card_no, (wining_nums, nums_i_have))
        }).collect();


    let mut dict: HashMap<_,_> = input2.iter().map(|(card_no, _)| {
        (*card_no, 1)
    }).collect();

    println!("input2: {:?}", input2);
    println!("dict: {:?}", dict);

    (1..max_card_id+1).for_each(|card_id| {
        let (wining_nums, nums_i_have) = &input2[&card_id];
        let num_wining_i_have = wining_nums.intersection(&nums_i_have).count() as u32;
        let num_repeat_cur_card = dict[&card_id];

        (0..num_repeat_cur_card).for_each(|_| {
            (card_id+1..card_id+num_wining_i_have+1).for_each(|next_card_id| {
                dict.entry(next_card_id).and_modify(|val| *val += 1);

            })
        });
        println!("card_id: {}, num_wining_i_have: {}, num_repeat_cur_card: {}", card_id, num_wining_i_have, num_repeat_cur_card);
        println!("dict: {:?}", dict);
    });

    println!("dict: {:?}", dict);

    let result2: u32 = dict.values().sum();
    println!("result2: {:?}", result2);
}
