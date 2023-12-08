use std::fs;


fn main() {
    //let input = fs::read_to_string("test_input_day06.txt").unwrap();
    let input = fs::read_to_string("input_day06.txt").unwrap();

    let mut lines = input.lines();
    let times: Vec<_> = lines.next().unwrap().split(":").nth(1).unwrap().split_whitespace().map(|s| s.parse::<u32>().unwrap()).collect();
    let distances: Vec<_> = lines.next().unwrap().split(":").nth(1).unwrap().split_whitespace().map(|s| s.parse::<u32>().unwrap()).collect();
    println!("times: {:?}", times);
    println!("distances: {:?}", distances);
    let input: Vec<_> = times.into_iter().zip(distances.into_iter()).collect();
    println!("input: {:?}", input);

    let num_wins: Vec<_> = input.into_iter().map(|(time, dist)| {
        let max_min = q_e(time, dist);
        let result = match max_min.len() {
            2 => {
                let mut it = max_min.iter();
                let min = it.next().unwrap();
                let max = it.next().unwrap();
                max - min + 1 },
            _ => { 0 },
        };
        result
    }).collect();
    num_wins.iter().for_each(|wins| println!("wins: {:?}", wins));

    let result: u32 = num_wins.iter().product();
    println!("result: {}", result);
}

fn q_e(t: u32, d: u32) -> Vec<u32> {
    let delta = t*t - 4*d;

    if delta > 0 {
        let r1 = ((t as f64 - (delta as f64).sqrt()) / 2.0).floor() as u32 + 1;
        let r2 = ((t as f64 + (delta as f64).sqrt()) / 2.0).ceil() as u32 - 1;
        return vec![r1, r2];
    } else if delta == 0 {
        let r = (t as f64 / 2.0).round() as u32;
        return vec![r];
    }
    vec![]
}
