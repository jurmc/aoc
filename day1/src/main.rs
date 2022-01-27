use day1::read_file_into_vec_u32;

//Window2Elements///////////////////////////////////////////////
struct Window2Elements {
    v: Vec<u32>,
    cnt: usize,
    len: usize,
}

impl Window2Elements {
    fn new(v: Vec<u32>) -> Window2Elements {
        let cnt = 0;
        let len = v.len() -1;
        Window2Elements {v, cnt, len}
    }
}

impl Iterator for Window2Elements {
    type Item = (u32, u32);

    fn next(&mut self) -> Option<Self::Item> {
        if self.cnt < self.len {
            let retval = (self.v[self.cnt], self.v[self.cnt+1]);
            self.cnt += 1;
            Option::Some(retval)
        } else {
            Option::None
        }
    }
}

//TwoWindows3Elements///////////////////////////////////////////
struct TwoWindows3Elements {
    v: Vec<u32>,
    cnt: usize,
    len: usize,
}

impl TwoWindows3Elements {
    fn new(v: Vec<u32>) -> TwoWindows3Elements {
        let cnt = 0;
        let len = v.len() -3;
        TwoWindows3Elements {v, cnt, len}
    }
}

impl Iterator for TwoWindows3Elements {
    type Item = ((u32, u32, u32), (u32, u32, u32));

    fn next(&mut self) -> Option<Self::Item> {
        if self.cnt < self.len {
            let c = self.cnt;
            let v = &self.v;
            let retval = ((v[c], v[c+1], v[c+2]),
                          (v[c+1], v[c+2], v[c+3]));
            self.cnt += 1;
            Option::Some(retval)
        } else {
            Option::None
        }
    }
}

fn main() {
    let v = read_file_into_vec_u32("input.dat");
    let w = Window2Elements::new(v);
    println!("Answer for star1 is: {}", count_larger_measurments_for_star1(w));

    let v = read_file_into_vec_u32("input.dat");
    let w = TwoWindows3Elements::new(v);
    println!("Answer for star2 is: {}", count_larger_measurments_for_star2(w));
}

fn count_larger_measurments_for_star1(w: Window2Elements) -> u32 {
    let mut rv = 0;
    for item in w {
        if item.1 > item.0 {
            rv += 1;
        }
    }
    rv
}

fn count_larger_measurments_for_star2(w: TwoWindows3Elements) -> u32 {
    let mut rv = 0;
    for item in w {
        let prev_sum = item.0.0 + item.0.1 + item.0.2;
        let next_sum = item.1.0 + item.1.1 + item.1.2;

        if next_sum > prev_sum {
            rv += 1;
        }
    }
    rv
}

#[test]
fn check_input_star1() {
    let v = read_file_into_vec_u32("test-input.dat");
    let w = Window2Elements::new(v);
    assert_eq!(7, count_larger_measurments_for_star1(w));

    let v = read_file_into_vec_u32("input.dat");
    let w = Window2Elements::new(v);
    assert_eq!(1616, count_larger_measurments_for_star1(w));
}

#[test]
fn check_input_star2() {
    let v = read_file_into_vec_u32("test-input.dat");
    let w = TwoWindows3Elements::new(v);
    assert_eq!(5, count_larger_measurments_for_star2(w));

    let v = read_file_into_vec_u32("input.dat");
    let w = TwoWindows3Elements::new(v);
    assert_eq!(1645, count_larger_measurments_for_star2(w));
}
