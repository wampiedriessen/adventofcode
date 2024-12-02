use std::collections::{HashMap, HashSet};
use crate::Day;

pub struct Day08 {
    pub input: Vec<String>,
}

type Forest = HashMap<(usize, usize), i32>;

fn read_forest(input: &[String]) -> Forest {
    let mut forest = HashMap::new();

    for (y, line) in input.iter().enumerate() {
        for (x, height) in line.chars().enumerate() {
            forest.insert((x, y), height.to_string().parse().unwrap());
        }
    }

    forest
}

impl Day for Day08 {
    fn part1(&self) -> String {
        let width = self.input[0].len();
        let height = self.input.len();

        let mut visibles: HashSet<(usize, usize)> = HashSet::new();
        let forest = read_forest(&self.input);

        let mut checker = |c: (usize, usize), maxh: &mut i32| {
            let theight = forest.get(&c).unwrap();
            if *maxh >= *theight { return false; }
            //else
            *maxh = *theight;
            visibles.insert(c);
            true
        };

        let mut highest: i32;
        for y in 0..height {
            highest = -1;
            for x in 0..width {
                checker((x, y), &mut highest);
            }
            highest = -1;
            for x in (0..width).rev() {
                checker((x, y), &mut highest);
            }
        }
        for x in 0..width {
            highest = -1;
            for y in 0..height {
                checker((x, y), &mut highest);
            }
            highest = -1;
            for y in (0..height).rev() {
                checker((x, y), &mut highest);
            }
        }

        visibles.len().to_string()
    }

    fn part2(&self) -> String {
        let width = self.input[0].len();
        let height = self.input.len();

        let forest = read_forest(&self.input);

        let mut best_score = 0;

        for y in 0..height {
            for x in 0..width {
                best_score = best_score.max(calc_score(&forest, x, y, width, height));
            }
        }

        best_score.to_string()
    }
}

fn calc_score(forest: &Forest, x: usize, y: usize, width: usize, height: usize) -> usize {
    let mut left = 0;
    let mut right = 0;
    let mut up = 0;
    let mut down = 0;

    let my_height = forest.get(&(x, y)).unwrap();
    for lx in (0..x).rev() {
        let th = forest.get(&(lx, y)).unwrap();
        if my_height <= th {
            left += 1;
            break;
        }
        if my_height > th {
            left += 1;
        }
    }
    for rx in (x+1)..width {
        let th = forest.get(&(rx, y)).unwrap();
        if my_height <= th {
            right += 1;
            break;
        }
        if my_height > th {
            right += 1;
        }
    }

    for ly in (0..y).rev() {
        let th = forest.get(&(x, ly)).unwrap();
        if my_height <= th {
            up += 1;
            break;
        }
        if my_height > th {
            up += 1;
        }
    }
    for ry in (y+1)..height {
        let th = forest.get(&(x, ry)).unwrap();
        if my_height <= th {
            down += 1;
            break;
        }
        if my_height > th {
            down += 1;
        }
    }

    left * right * up * down
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &str = "30373
25512
65332
33549
35390";

    fn get_day(input_num: u8) -> Day08 {
        let inp = match input_num {
            0 => include_str!("../inputs/day08.txt"),
            1 => INPUT,
            _ => panic!(),
        };

        Day08 {
            input: inp.lines().map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example() {
        let d = get_day(1);

        assert_eq!("21", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = get_day(1);

        assert_eq!("8", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("1782", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("474606", d.part2());
    }
}
