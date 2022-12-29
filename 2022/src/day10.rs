use crate::Day;
use std::str::FromStr;

pub struct Day10 {
    pub input: Vec<String>,
}

#[derive(PartialEq)]
enum Instruction {
    Noop,
    Addx(i32),
}

impl FromStr for Instruction {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "noop" => Instruction::Noop,
            x if x.starts_with("addx") => Instruction::Addx(x[5..].parse().unwrap()),
            _ => return Err(()),
        })
    }
}

impl Day for Day10 {
    fn part1(&self) -> String {
        let mut cycle = 1;
        let mut x_val = 1;

        let mut sum = 0;

        for op in &self.input {
            let inst: Instruction = op.parse().unwrap();
            cycle += 1;
            if (cycle % 40) == 20 && cycle < 230 {
                sum += cycle * x_val;
            }
            if inst == Instruction::Noop {
                continue;
            } else if let Instruction::Addx(add) = inst {
                x_val += add;
                cycle += 1;
                if (cycle % 40) == 20 && cycle < 230 {
                    sum += cycle * x_val;
                }
            }
        }
        sum.to_string()
    }

    fn part2(&self) -> String {
        let mut cycle = 0;
        let mut x_val = 1;

        let mut output = String::new();

        for op in &self.input {
            let inst: Instruction = op.parse().unwrap();
            if [x_val, x_val + 1, x_val + 2].contains(&(cycle % 40 + 1)) {
                output.push('#');
            } else {
                output.push('.');
            }
            if cycle % 40 == 39 {
                output.push('\n');
            }
            cycle += 1;
            if inst == Instruction::Noop {
                continue;
            }
            if let Instruction::Addx(add) = inst {
                if [x_val, x_val + 1, x_val + 2].contains(&(cycle % 40 + 1)) {
                    output.push('#');
                } else {
                    output.push('.');
                }
                if cycle % 40 == 39 {
                    output.push('\n');
                }
                x_val += add;
                cycle += 1;
            }
        }
        // drop last enter
        output.pop();
        output
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &str = "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop";

    fn get_day(input_num: u8) -> Day10 {
        let inp = match input_num {
            0 => include_str!("../inputs/day10.txt"),
            1 => INPUT,
            _ => panic!(),
        };

        Day10 {
            input: inp.lines().map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example() {
        let d = get_day(1);

        assert_eq!("13140", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = get_day(1);

        assert_eq!(
            "##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....",
            d.part2()
        );
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("14420", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        //RGLRBZAU
        assert_eq!(
            "###...##..#....###..###..####..##..#..#.
#..#.#..#.#....#..#.#..#....#.#..#.#..#.
#..#.#....#....#..#.###....#..#..#.#..#.
###..#.##.#....###..#..#..#...####.#..#.
#.#..#..#.#....#.#..#..#.#....#..#.#..#.
#..#..###.####.#..#.###..####.#..#..##..",
            d.part2()
        );
    }
}
