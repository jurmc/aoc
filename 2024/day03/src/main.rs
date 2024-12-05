use std::fs;
use std::collections::VecDeque;

fn only_digits(s: &str) -> bool {
    s.as_bytes().iter().all(|c| c.is_ascii_digit())
}

enum State {
    Idle,
    Op1,
    Op2,
    Do,
    Dont,
}

fn main() {
    //let input = fs::read_to_string("test_input.txt").unwrap();
    //let input = fs::read_to_string("test_input2.txt").unwrap();
    let input = fs::read_to_string("input.txt").unwrap();
    println!("input: {}", &input);

    let mut input: VecDeque<_> = input.chars().collect();

    let mut state = State::Idle;
    let mut op1_str = String::new();
    let mut op2_str = String::new();
    let mut multiplication_enabled = true;
    let mut acc: u64 = 0;
    loop {
        match input.pop_front() {
            None => break,
            Some(ch) => {
                println!("front: {}", ch);

                match state {
                    State::Idle => {
                        if ch == 'm'
                           && Some(&'u') == input.get(0)
                           && Some(&'l') == input.get(1)
                           && Some(&'(') == input.get(2)
                        {
                            (0..3).for_each(|_| {input.pop_front();});
                            println!("Entering OP1 state");
                            state = State::Op1;
                        }
                    }
                    State::Op1 => {
                        if ch.is_digit(10) {
                            op1_str.push(ch);
                        } else if ch == ',' {
                            println!("== Op1 -> Op2 (op1_str: {})", op1_str);
                            state = State::Op2;
                        } else {
                            op1_str.clear();
                            println!("== Op1 -> Idle");
                            input.insert(0, ch);
                            state = State::Idle;
                        }
                    }
                    State::Op2 => {
                        if ch.is_digit(10) {
                            op2_str.push(ch);
                        } else if ch == ')' {
                            println!("Found multiplication: {} {}", op1_str, op2_str);

                            let op1 = op1_str.parse::<u64>().unwrap();
                            let op2 = op2_str.parse::<u64>().unwrap();
                            acc += op1 * op2;

                            op1_str.clear();
                            op2_str.clear();
                            println!("== Op1 -> Idle");
                            input.insert(0, ch);
                            state = State::Idle;
                        } else {
                            op1_str.clear();
                            op2_str.clear();
                            println!("== Op2 -> Idle");
                            input.insert(0, ch);
                            state = State::Idle;
                        }
                    }
                    _ => {},
                }
            },
        }
    }

    println!("acc: {}", acc);

}
