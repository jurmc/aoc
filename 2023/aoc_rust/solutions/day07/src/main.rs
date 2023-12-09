use std::fs;
use std::collections::HashMap;
use std::collections::HashSet;

fn main() {
    //let input = fs::read_to_string("test_input_day07.txt").unwrap();
    let input = fs::read_to_string("input_day07.txt").unwrap();

    let v = vec!['A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J'];
    let card_vals: HashMap<_,_> = HashMap::from_iter(v.iter().rev().enumerate().map(|(idx,symbol)| (symbol, idx+1)));
    println!("card_vals: {:?}", card_vals);

    println!("input: {}", input);
    let mut hands_with_bids: Vec<_> = input.lines().map(|line| {
        let mut it = line.split_whitespace();
        let hand = it.next().unwrap();
        let bid: u32 = it.next().unwrap().parse().unwrap();
        let this_card_val: Vec<u32> = hand.chars().map(|c| *(card_vals.get(&c).unwrap()) as u32).collect();
        ((hand_val(hand), this_card_val.clone()), (hand, bid))
    }).collect();

    hands_with_bids.iter().for_each(|item| {
        println!("item: {:?}", item);
    });

    hands_with_bids.sort_by(|(val1, _), (val2, _)| { val1.partial_cmp(val2).unwrap() });

    println!("after sor");
    hands_with_bids.iter().for_each(|item| {
        println!("item: {:?}", item);
    });

    let rank_with_bids: Vec<_> = hands_with_bids.iter().enumerate().map(|(zb_idx,(_, (_, bid)))| ((zb_idx+1) as u32, *bid)).collect();
    rank_with_bids.iter().for_each(|item| {
        println!("rank and bid: {:?}", item);
    });

    let result: u32 = rank_with_bids.iter().map(|(v1,v2)| v1 * v2).sum(); 
    println!("result: {}", result);
}

fn hand_val(hand: &str) -> u32 {
    let result = match hand_type(hand) {
        "five_of_a_kind" => 6,
        "four_of_a_kind" => 5,
        "full_house" => 4,
        "three_of_a_kind" => 3,
        "two_pair" => 2,
        "one_pair" => 1,
        &_ => 0,
    };

    if hand != "JJJJJ" && hand.contains('J') {
        let result_j = match hand_type_j(hand) {
            "five_of_a_kind" => 6,
            "four_of_a_kind" => 5,
            "full_house" => 4,
            "three_of_a_kind" => 3,
            "two_pair" => 2,
            "one_pair" => 1,
            &_ => 0,
        };

        if result_j > result {
            return result_j
        }
    }

    result
}

fn hand_type_j(hand: &str) -> &str {
    if !hand.contains('J') {
        return "high_card"
    }
    let jokers_num: u32 = hand.chars().filter(|c| *c == 'J').count() as u32;
    println!("hand_type_j: {}, jokers_num: {}", hand, jokers_num);

    let hand_without_j: String = hand.chars().filter(|c| *c != 'J').collect();
    let dict: HashMap<_,_> = hand_without_j.chars()
        .fold(HashMap::new(), |mut acc: HashMap<char, u32>, c| {
            acc.entry(c).and_modify(|v| *v += 1).or_insert(1);
            acc
        });
    let mut vals_vec_sorted: Vec<_> = dict.values().collect();
    vals_vec_sorted.sort();
    let vals_vec_rev_sorted: Vec<_> = vals_vec_sorted.clone().into_iter().rev().collect();
    let max_num_occureneces = dict.values().max().unwrap();

    if max_num_occureneces + jokers_num == 5 {
        return "five_of_a_kind"
    }

    if max_num_occureneces + jokers_num == 4 {
        return "four_of_a_kind"
    }

    let mut it = vals_vec_rev_sorted.iter();
    let max_occ_1 = *(it.next().unwrap());
    let max_occ_2 = *(it.next().unwrap());

    if max_occ_1 + max_occ_2 + jokers_num == 5 {
        return "full_house"
    }

    if max_occ_1 + jokers_num == 3 {
        return "three_of_a_kind"
    }

    if max_occ_1 + jokers_num == 2 {
        return "one_pair"
    }

    "high_card"
}


fn hand_type(hand: &str) -> &str {
    let dict: HashMap<_,_> = hand.chars()
        .fold(HashMap::new(), |mut acc: HashMap<char, u32>, c| {
            acc.entry(c).and_modify(|v| *v += 1).or_insert(1);
            acc
        });

    let mut vals_vec_sorted: Vec<_> = dict.values().collect();
    vals_vec_sorted.sort();
    let vals_vec_rev_sorted: Vec<_> = vals_vec_sorted.clone().into_iter().rev().collect();
    let mut vals_set: HashSet<_> = dict.values().collect();
    let max_num_occureneces = dict.values().max().unwrap();
    //println!("max_num_occureneces: {:?}", max_num_occureneces);
    if *max_num_occureneces == 5 {
        return "five_of_a_kind";
    }

    if *max_num_occureneces == 4 {
        return "four_of_a_kind";
    }

    let full_house_set: HashSet<_> = vec![&2, &3].into_iter().collect();
    if full_house_set == vals_set {
        return "full_house";
    }

    if *max_num_occureneces == 3 {
        return "three_of_a_kind";
    }

    let mut it = vals_vec_rev_sorted.iter();
    let max_occ_1 = *(it.next().unwrap());
    let max_occ_2 = *(it.next().unwrap());
    if (max_occ_1, max_occ_2) == (&2u32, &2u32) {
        return "two_pair";
    }

    if max_occ_1 == &2u32 {
        return "one_pair";
    }

    "high_card"
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn hand_type_tests() {
        assert_eq!("five_of_a_kind", hand_type("KKKKK"));
        assert_eq!("four_of_a_kind", hand_type("KKAKK"));
        assert_eq!("full_house", hand_type("23332"));
        assert_eq!("full_house", hand_type("TATTA"));
        assert_eq!("three_of_a_kind", hand_type("TKTTA"));
        assert_eq!("two_pair", hand_type("A7TTA"));
        assert_eq!("one_pair", hand_type("A7TTQ"));
        assert_eq!("high_card", hand_type("A7T4Q"));
    }

   #[test]
   fn hand_val_tests() {
       assert_eq!(6, hand_val("KKKKK"));
       assert_eq!(5, hand_val("KKAKK"));
       assert_eq!(4, hand_val("23332"));
       assert_eq!(4, hand_val("TATTA"));
       assert_eq!(3, hand_val("TKTTA"));
       assert_eq!(2, hand_val("A7TTA"));
       assert_eq!(1, hand_val("A7TTQ"));
       assert_eq!(0, hand_val("A7T4Q"));
   }

    #[test]
    fn hand_val_with_joker_tests() {
        assert_eq!(6, hand_val("8888J"));
        assert_eq!(5, hand_val("8JJ82"));
        assert_eq!(4, hand_val("88J77"));
        assert_eq!(3, hand_val("88J23"));
        assert_eq!(1, hand_val("89J23"));
    }


}
