use std::fs;
use std::collections::VecDeque;

enum State {
    Idle,
    Op1,
    Op2,
}

fn main() {
    //let input = fs::read_to_string("test_input2.txt").unwrap();
    let input = fs::read_to_string("input.txt").unwrap();

    let mut input: VecDeque<_> = input.chars().collect();

    let mut state = State::Idle;
    let mut op1_str = String::new();
    let mut op2_str = String::new();
    let mut multiplication_enabled = true;
    let mut acc1: u64 = 0;
    let mut acc2: u64 = 0;
    loop {
        match input.pop_front() {
            None => break,
            Some(ch) => {

                if ch == 'm'
                    && Some(&'u') == input.get(0)
                        && Some(&'l') == input.get(1)
                        && Some(&'(') == input.get(2)
                {
                    (0..3).for_each(|_| {input.pop_front();});
                    state = State::Op1;
                    continue;
                }

                if ch == 'd'
                    && Some(&'o') == input.get(0)
                    && Some(&'n') == input.get(1)
                    && Some(&'\'') == input.get(2)
                    && Some(&'t') == input.get(3) {
                        (0..4).for_each(|_| {input.pop_front();});
                        state = State::Idle;
                        op1_str.clear();
                        op2_str.clear();
                        multiplication_enabled = false;
                        continue;
                }

                if ch == 'd'
                    && Some(&'o') == input.get(0) {
                    input.pop_front();
                    state = State::Idle;
                    op1_str.clear();
                    op2_str.clear();
                    multiplication_enabled = true;
                    continue;
                }

                match state {
                    State::Op1 => {
                        if ch.is_digit(10) {
                            op1_str.push(ch);
                        } else if ch == ',' {
                            state = State::Op2;
                        } else {
                            op1_str.clear();
                            input.insert(0, ch);
                            state = State::Idle;
                        }
                    }
                    State::Op2 => {
                        if ch.is_digit(10) {
                            op2_str.push(ch);
                        } else if ch == ')' {

                            let op1 = op1_str.parse::<u64>().unwrap();
                            let op2 = op2_str.parse::<u64>().unwrap();

                            acc1 += op1 * op2;
                            if multiplication_enabled {
                                acc2 += op1 * op2;
                            }

                            op1_str.clear();
                            op2_str.clear();
                            input.insert(0, ch);
                            state = State::Idle;
                        } else {
                            op1_str.clear();
                            op2_str.clear();
                            input.insert(0, ch);
                            state = State::Idle;
                        }
                    }
                    _ => {},
                }
            },
        }
    }

    println!("Result1: {}", acc1);
    println!("Result2: {}", acc2);

}
