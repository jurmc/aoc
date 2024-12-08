use std::fs;
use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::Iterator;

#[derive(Debug)]
struct XmasIter1<'a> {
    pos: (i32, i32),
    dir: (i8, i8),
    matrix: &'a HashMap<(i32, i32), char>,
}

impl XmasIter1<'_> {
    fn new(pos: (i32, i32), dir: (i8, i8), matrix: &HashMap<(i32, i32), char>) -> XmasIter1 {
        XmasIter1 {pos, dir, matrix}
    }
}

impl Iterator for XmasIter1<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        match self.matrix.get(&self.pos) {
            None => None,
            Some(c) => {
                self.pos.0 += self.dir.0 as i32;
                self.pos.1 += self.dir.1 as i32;
                Some(c.clone())
            }
        }
    }
}

struct XmasIter2<'a> {
    matrix: &'a HashMap<(i32, i32), char>,
    v: Vec<(i32, i32)>,
    idx: usize,
}

impl XmasIter2<'_> {
    fn new((x, y): (i32, i32), matrix: &HashMap<(i32, i32), char>) -> XmasIter2 {
        let v = vec![
            (x, y),
            (x-1, y-1), (x+1, y-1),
            (x-1, y+1), (x+1, y+1),
        ];
        XmasIter2 {matrix, v, idx: 0}
    }
}

impl Iterator for XmasIter2<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let key = self.v.get(self.idx)?;
        let ch = self.matrix.get(key)?;
        self.idx += 1;
        Some(*ch)
    }
}

fn main() {
    //let input = fs::read_to_string("test_input.txt").unwrap();
    let input = fs::read_to_string("input.txt").unwrap();

    let mut matrix: HashMap<(i32, i32), char> = HashMap::with_capacity(input.len());
    input.lines().enumerate().for_each(|(y, line)| {
        line.chars().enumerate().for_each(|(x, ch)| {
            matrix.insert((x as i32, y as i32), ch);
        })
    });

    let all_dirs = vec![
        (1i8, 0i8), (-1i8, 0i8), (0i8, 1i8), (0i8, -1i8),
        (1i8, 1i8), (-1i8, -1i8), (-1i8, 1i8), (1i8, -1i8),
    ];

    let xmas= vec!['X', 'M', 'A', 'S'];

    let mut x_mas: HashSet<_>  = HashSet::new();
    x_mas.insert(vec!['A', 'M', 'M', 'S', 'S']);
    x_mas.insert(vec!['A', 'S', 'S', 'M', 'M']);
    x_mas.insert(vec!['A', 'M', 'S', 'M', 'S']);
    x_mas.insert(vec!['A', 'S', 'M', 'S', 'M']);

    let mut acc1 = 0;
    let mut acc2 = 0;

    matrix.iter().for_each(|((x, y), ch)| {

        // Part 1
        all_dirs.iter().for_each(|dir| {
            let v: Vec<char> = XmasIter1::new((*x, *y), *dir, &matrix).take(4).collect();
            if v == xmas {
                acc1 += 1;
            }
        });

        // Part 2
        let v: Vec<char> = XmasIter2::new((*x, *y), &matrix).collect();
        if x_mas.contains(&v) {
            acc2 += 1;
        }
    });

    println!("Result1: {}", acc1);
    println!("Result2: {}", acc2);
}
