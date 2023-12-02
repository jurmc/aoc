use std::fs;
use std::collections::HashMap;

fn main() {
    //let input = fs::read_to_string("test_input_day02.txt").unwrap();
    let input = fs::read_to_string("input_day02.txt").unwrap();

    let data: Vec<_> = input.lines().map(|line| {
        let (key_part, vals_part) = line.split_at(line.find(':').unwrap());
        let game_id: u32 = key_part.split(' ').skip(1).next().unwrap().parse().unwrap();
        let vals_part = &vals_part[2..];
        let games = vals_part.split(';');
        let mut game_dict = HashMap::new();
        games.for_each(|game| {
            //println!(" game: {}", game);
            game.split(',').map(|set| set.trim()).for_each(|set|{
                let mut set_iter = set.split(' ').into_iter();
                let new_num: u32 = set_iter.next().unwrap().parse().unwrap();
                let color = set_iter.next().unwrap();
                //println!("  color: {}, new_num: {}", color, new_num);
                let cur_num = game_dict.entry(color).or_insert(0);
                if *cur_num < new_num {
                    *cur_num = new_num
                }
            })

        });
        //(game_id, vals_part)
        (game_id, game_dict)
    }).collect();

    let sum: u32 = data.iter().filter(|(game_id, game_dict)| {
        if game_dict["red"] > 12 {return false};
        if game_dict["green"] > 13 {return false};
        if game_dict["blue"] > 14 {return false};
        true})
    .map(|(game_id, _game_dict)| { game_id })
    .sum();
    println!("sum: {}", sum);

    let power_sum: u32 = data.iter().map(|(_game_id, game_dict)| {
        let power = game_dict.clone().into_values().reduce(|acc, el| acc * el).unwrap();
        (power)
    //}).for_each(|(power)| println!("power: {}", power));
    }).sum();
    println!("power_sum: {}", power_sum);
}
