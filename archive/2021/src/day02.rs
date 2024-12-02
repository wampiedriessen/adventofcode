use crate::Day;

pub struct Day02 {
    pub input: Vec<String>,
}

impl Day for Day02 {
    fn part1(&self) -> String {
        let mut horizontal = 0;
        let mut depth = 0;

        for line in &self.input {
            let split: Vec<&str> = line.split(' ').collect();
            let val = split[1].parse::<i32>().unwrap();
            match split[0] {
                "forward" => {
                    horizontal += val;
                }
                "down" => {
                    depth += val;
                }
                "up" => {
                    depth -= val;
                }
                _ => panic!(),
            }
        }

        (depth * horizontal).to_string()
    }

    fn part2(&self) -> String {
        let mut horizontal = 0;
        let mut depth = 0;
        let mut aim = 0;

        for line in &self.input {
            let split: Vec<&str> = line.split(' ').collect();
            let val = split[1].parse::<i32>().unwrap();
            match split[0] {
                "forward" => {
                    horizontal += val;
                    depth += aim * val;
                }
                "down" => {
                    aim += val;
                }
                "up" => {
                    aim -= val;
                }
                _ => panic!(),
            }
        }

        (depth * horizontal).to_string()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &str = "forward 5
down 5
forward 8
up 3
down 8
forward 2";

    fn get_day(input_num: u8) -> Day02 {
        let inp = match input_num {
            0 => include_str!("../inputs/day02.txt"),
            1 => INPUT,
            _ => panic!(),
        };

        Day02 {
            input: inp.split('\n').map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example() {
        let d = get_day(1);

        assert_eq!("150", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = get_day(1);

        assert_eq!("900", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("1728414", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("1765720035", d.part2());
    }
}
