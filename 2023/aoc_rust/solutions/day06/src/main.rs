use std::fs;

fn main() {
    //let input = fs::read_to_string("test_input_day06.txt").unwrap();
    let input = fs::read_to_string("input_day06.txt").unwrap();

    let mut lines = input.lines();
    let times: Vec<_> = lines.next().unwrap().split(":").nth(1).unwrap().split_whitespace().map(|s| s.parse::<u64>().unwrap()).collect();
    let distances: Vec<_> = lines.next().unwrap().split(":").nth(1).unwrap().split_whitespace().map(|s| s.parse::<u64>().unwrap()).collect();


    let zipped_input: Vec<_> = times.into_iter().zip(distances.into_iter()).collect();

    let wins: Vec<_> = zipped_input.into_iter().map(|(time, dist)| {
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

    let result: u64 = wins.iter().product();
    println!("part1 result: {}", result);

    let mut lines = input.lines();
    let time2: u64 = lines.next().unwrap().split(":").nth(1).unwrap().chars().filter(|c| c.is_digit(10)).collect::<String>().parse().unwrap();
    let dist2: u64 = lines.next().unwrap().split(":").nth(1).unwrap().chars().filter(|c| c.is_digit(10)).collect::<String>().parse().unwrap();
    println!("times2: {:?}", time2);
    println!("distances2: {:?}", dist2);

    let wins: Vec<_> = q_e(time2, dist2);
    let (min, max) = (wins[0], wins[1]);
    println!("wins: {:?}", wins);
    println!("min: {:?}, max: {:?}", min, max);
    let result2 = max - min + 1;
    println!("result2: {:?}", result2);
}

fn q_e(t: u64, d: u64) -> Vec<u64> {
    let delta = t*t - 4*d;

    if delta > 0 {
        let r1 = ((t as f64 - (delta as f64).sqrt()) / 2.0).floor() as u64 + 1;
        let r2 = ((t as f64 + (delta as f64).sqrt()) / 2.0).ceil() as u64 - 1;
        return vec![r1, r2];
    } else if delta == 0 {
        let r = (t as f64 / 2.0).round() as u64;
        return vec![r];
    }
    vec![]
}
