use std::fs;
use std::collections::HashMap;
use std::iter::Iterator;

#[derive(Debug)]
struct XmasIter<'a> {
    pos: (i32, i32),
    dir: (i8, i8),
    matrix: &'a HashMap<(i32, i32), char>,
}

impl XmasIter<'_> {
    fn new(pos: (i32, i32), dir: (i8, i8), matrix: &HashMap<(i32, i32), char>) -> XmasIter {
        XmasIter {pos, dir, matrix}
    }
}

impl Iterator for XmasIter<'_> {
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

fn main() {
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
    let xmas = vec![Some('X'), Some('M'), Some('A'), Some('S')];
    let mut acc1 = 0;

    matrix.iter().for_each(|((x, y), ch)| {
        all_dirs.iter().for_each(|dir| {
            let a = 0;
            let mut it = XmasIter::new((*x, *y), *dir, &matrix);
            let v = vec![it.next(), it.next(), it.next(), it.next()];
            let assesment = v == xmas;
            //println!("x: {}, y: {}, -- {:?}, assesment: {}", x, y, v, assesment);
            if assesment == true {
                acc1 += 1;
            }
        });
    });

    println!("Result1: {}", acc1);
}
