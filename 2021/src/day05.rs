use std::collections::HashSet;

use crate::Day;

pub struct Day05 {
    pub input: Vec<String>,
}

struct Line {
    pub x1: i64,
    pub x2: i64,
    pub y1: i64,
    pub y2: i64,
}

impl std::str::FromStr for Line {
    type Err = core::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let coords: Vec<&str> = s.split(" -> ").collect();

        let start: Vec<&str> = coords[0].split(',').collect();
        let end: Vec<&str> = coords[1].split(',').collect();

        Ok(Line {
            x1: start[0].parse()?,
            y1: start[1].parse()?,
            x2: end[0].parse()?,
            y2: end[1].parse()?,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Coord {
    pub x: i64,
    pub y: i64,
}

impl Line {
    pub fn is_horizontal_or_vertical(&self) -> bool {
        return self.x1 == self.x2 || self.y1 == self.y2;
    }

    pub fn get_coords(&self) -> Vec<Coord> {
        let mut coords = Vec::new();

        let mut x = self.x1;
        let mut y = self.y1;

        coords.push(Coord { x, y });
        while x != self.x2 || y != self.y2 {
            if x != self.x2 {
                if x > self.x2 {
                    x -= 1;
                } else {
                    x += 1;
                }
            }
            if y != self.y2 {
                if y > self.y2 {
                    y -= 1;
                } else {
                    y += 1;
                }
            }

            coords.push(Coord { x, y });
        }

        coords
    }
}

impl Day05 {
    fn calculate_overlap(&self, lines: Vec<Line>) -> String {
        let mut coords = HashSet::new();
        let mut double_coords = HashSet::new();

        for line in lines {
            for coord in line.get_coords() {
                let inserted = coords.insert(coord.clone());
                if !inserted {
                    let _ = &double_coords.insert(coord.clone());
                }
            }
        }

        double_coords.len().to_string()
    }
}

impl Day for Day05 {
    fn part1(&self) -> String {
        let mut lines: Vec<Line> = self
            .input
            .iter()
            .map(|l| l.parse::<Line>().unwrap())
            .collect();

        lines.retain(|l| l.is_horizontal_or_vertical());

        self.calculate_overlap(lines)
    }

    fn part2(&self) -> String {
        let lines: Vec<Line> = self
            .input
            .iter()
            .map(|l| l.parse::<Line>().unwrap())
            .collect();

        self.calculate_overlap(lines)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &str = "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2";

    fn get_day(input_num: u8) -> Day05 {
        let inp = match input_num {
            0 => include_str!("../inputs/day05.txt"),
            1 => INPUT,
            _ => panic!(),
        };

        Day05 {
            input: inp.split('\n').map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example() {
        let d = get_day(1);

        assert_eq!("5", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = get_day(1);

        assert_eq!("12", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("7297", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("21038", d.part2());
    }
}
