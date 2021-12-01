use crate::Day;

pub struct Day01 {
    pub input: Vec<String>,
}

impl Day for Day01 {
    fn part1(&self) -> String {
        let mut num = 0;

        for w in self.input.windows(2) {
            let w1: i64 = w[0].parse().unwrap();
            let w2: i64 = w[1].parse().unwrap();

            if w2 > w1 {
                num += 1;
            }
        }

        num.to_string()
    }

    fn part2(&self) -> String {
        let mut num = 0;

        let mut sprev = std::i32::MAX;

        for w in self.input.windows(3) {
            let s: i32 = w.iter().map(|x: &String| x.parse::<i32>().unwrap()).sum();

            if s > sprev {
                num += 1;
            }

            sprev = s;
        }

        num.to_string()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &str = "199
200
208
210
200
207
240
269
260
263";

    fn get_day(input_num: u8) -> Day01 {
        let inp = match input_num {
            0 => include_str!("../inputs/day01.txt"),
            1 => INPUT,
            _ => panic!(),
        };

        Day01 {
            input: inp.split('\n').map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example() {
        let d = get_day(1);

        assert_eq!("7", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = get_day(1);

        assert_eq!("5", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("1602", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("1633", d.part2());
    }
}
