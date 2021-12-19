use std::collections::HashSet;

use crate::Day;

pub struct Day13 {
    pub input: Vec<String>,
}

type Map = HashSet<(u32, u32)>;

enum FoldAlong {
    Vertical(u32),
    Horizontal(u32),
}

fn parse_coord(x: &String) -> (u32, u32) {
    let s: Vec<&str> = x.split(",").collect();
    (s[0].parse().unwrap(), s[1].parse().unwrap())
}

fn parse_command(x: &String) -> FoldAlong {
    let s: Vec<&str> = x.split("=").collect();
    let val = s[1].parse().unwrap();
    match s[0] {
        "fold along y" => FoldAlong::Horizontal(val),
        "fold along x" => FoldAlong::Vertical(val),
        _ => panic!("nooo"),
    }
}

struct Manual {
    largest_x: u32,
    largest_y: u32,
    command_stack: Vec<FoldAlong>,
    map: Map,
}

impl Manual {
    fn from_input(input: &Vec<String>) -> Manual {
        let mut input_iter = input.iter();

        let mut largest_x = 0;
        let mut largest_y = 0;
        let map = Map::from_iter(
            input_iter
                .by_ref()
                .take_while(|l| l.as_str() != "")
                .map(|l| {
                    let key @ (x, y) = parse_coord(l);
                    largest_x = x.max(largest_x);
                    largest_y = y.max(largest_y);
                    key
                }),
        );
        let mut command_stack: Vec<FoldAlong> = input_iter.map(|l| parse_command(l)).collect();

        command_stack.reverse();

        Manual {
            largest_x,
            largest_y,
            command_stack,
            map,
        }
    }

    fn fold(&mut self) {
        let cmd = match self.command_stack.pop() {
            None => return,
            Some(x) => x,
        };

        let (xedge, yedge) = match cmd {
            FoldAlong::Vertical(x) => {
                self.largest_x /= 2;
                (x, 0)
            }
            FoldAlong::Horizontal(y) => {
                self.largest_y /= 2;
                (0, y)
            }
        };

        let mut new_map = Map::new();

        for oldcoord @ (x, y) in self.map.iter() {
            if *x >= xedge && *y >= yedge {
                let newcoord = match cmd {
                    FoldAlong::Vertical(_) => (xedge - (*x - xedge), *y),
                    FoldAlong::Horizontal(_) => (*x, yedge - (*y - yedge)),
                };
                new_map.insert(newcoord);
            } else {
                new_map.insert(*oldcoord);
            }
        }
        self.map = new_map;
    }

    fn print(&self) -> String {
        let mut output = String::new();
        for y in 0..self.largest_y {
            for x in 0..self.largest_x {
                if self.map.contains(&(x, y)) {
                    output += "#";
                } else {
                    output += ".";
                }
            }
            output += "\n";
        }
        output.pop();
        output
    }
}

impl Day for Day13 {
    fn part1(&self) -> String {
        let mut manual = Manual::from_input(&self.input);

        manual.fold();

        manual.map.len().to_string()
    }
    fn part2(&self) -> String {
        let mut manual = Manual::from_input(&self.input);

        while !manual.command_stack.is_empty() {
            manual.fold();
        }

        println!("{}", manual.print());

        manual.print()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT1: &str = r#"6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"#;

    fn get_day(input_num: u8) -> Day13 {
        let inp = match input_num {
            0 => include_str!("../inputs/day13.txt"),
            1 => INPUT1,
            _ => panic!(),
        };

        Day13 {
            input: inp.split('\n').map(|s| s.to_string()).collect(),
        }
    }

    const OUTPUT1: &str = r#"#####
#...#
#...#
#...#
#####
.....
....."#;

    #[test]
    fn part1_example1() {
        let d = get_day(1);

        assert_eq!("17", d.part1());
    }

    #[test]
    fn part2_example1() {
        let d = get_day(1);

        assert_eq!(OUTPUT1, d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("745", d.part1());
    }

    const PART2_OUTPUT: &str = r#".##..###..#..#...##.####.###...##...##..
#..#.#..#.#.#.....#.#....#..#.#..#.#..#.
#..#.###..##......#.###..###..#....#....
####.#..#.#.#.....#.#....#..#.#.##.#....
#..#.#..#.#.#..#..#.#....#..#.#..#.#..#.
#..#.###..#..#..##..#....###...###..##.."#;

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!(PART2_OUTPUT, d.part2());
    }
}
