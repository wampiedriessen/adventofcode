use std::collections::HashSet;

use crate::Day;

pub struct Day11 {
    pub input: Vec<String>,
}

fn add_max_nine(x: usize) -> (usize, bool) {
    return (x + 1, x >= 9);
}

fn get_nbs(coords: &(usize, usize)) -> Vec<(usize, usize)> {
    let (x, y) = *coords;

    let xs = [x.overflowing_sub(1), (x, false), add_max_nine(x)];
    let ys = [y.overflowing_sub(1), (y, false), add_max_nine(y)];

    let mut result = Vec::new();

    for (xn, overflow) in xs {
        if overflow {
            continue;
        }
        for (yn, overflow) in ys {
            if overflow || (xn == x && yn == y) {
                continue;
            }
            result.push((xn, yn));
        }
    }

    result
}

fn step(octopi: &mut [[u8; 10]; 10]) -> usize {
    let mut flashing_octopi = HashSet::new();

    // substep 1
    for y in 0..10 {
        for x in 0..10 {
            octopi[y][x] += 1;

            if octopi[y][x] > 9 {
                flashing_octopi.insert((x, y));
            }
        }
    }

    let mut added_flashers: Vec<(usize, usize)> = flashing_octopi.clone().into_iter().collect();

    while !added_flashers.is_empty() {
        let mut new_flashers = Vec::new();

        // substep 2
        for flashing in added_flashers {
            for neighbour in get_nbs(&flashing) {
                if flashing_octopi.contains(&neighbour) {
                    continue
                }
                let (x, y) = neighbour;

                octopi[y][x] += 1;
                if octopi[y][x] > 9 {
                    new_flashers.push((x, y));
                    flashing_octopi.insert((x, y));
                }
            }
        }

        added_flashers = new_flashers;
    }

    // substep 3
    for flash in &flashing_octopi {
        let (x, y): (usize, usize) = *flash;
        octopi[y][x] = 0;
    }

    flashing_octopi.len()
}

impl Day11 {
    fn parse(&self) -> [[u8; 10]; 10] {
        let mut octopi = [[0; 10]; 10];

        for y in 0..10 {
            let line: Vec<&str> = self.input[y].split("").collect();
            for x in 0..10 {
                octopi[y][x] = line[x+1].parse::<u8>().unwrap();
            }
        }

        octopi
    }
}

impl Day for Day11 {
    fn part1(&self) -> String {
        let mut octopi: [[u8; 10]; 10] = self.parse();

        let mut sum = 0;

        for _ in 0..100 {
            sum += step(&mut octopi);
        }

        sum.to_string()
    }

    fn part2(&self) -> String {
        let mut octopi: [[u8; 10]; 10] = self.parse();

        let mut steps = 0;

        loop {
            let count = step(&mut octopi);

            steps += 1;

            if count == 100 { break; }
        }

        steps.to_string()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &str = r#"5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"#;

    fn get_day(input_num: u8) -> Day11 {
        let inp = match input_num {
            0 => include_str!("../inputs/day11.txt"),
            1 => INPUT,
            _ => panic!(),
        };

        Day11 {
            input: inp.split('\n').map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example() {
        let d = get_day(1);

        assert_eq!("1656", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = get_day(1);

        assert_eq!("195", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("1713", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("502", d.part2());
    }
}
