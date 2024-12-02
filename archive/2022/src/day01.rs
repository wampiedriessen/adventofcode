use crate::Day;

pub struct Day01 {
    pub input: Vec<String>,
}

impl Day for Day01 {
    fn part1(&self) -> String {
        let mut most = 0;
        let mut current: i32 = 0;

        for cals in &self.input {
            if cals.trim() == "" {
                most = if most > current { most } else { current };
                current = 0;
                continue;
            }

            current += cals.parse::<i32>().unwrap();
        }

        most = if most > current { most } else { current };

        most.to_string()
    }

    fn part2(&self) -> String {
        let mut current = 0;
        let mut elves: Vec<i32> = Vec::new();

        for cals in &self.input {
            if cals.trim() == "" {
                elves.push(current);
                current = 0;
                continue;
            }

            current += cals.parse::<i32>().unwrap();
        }

        elves.sort();
        elves.reverse();

        (elves[0] + elves[1] + elves[2]).to_string()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &str = "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000

";

    fn get_day(input_num: u8) -> Day01 {
        let inp = match input_num {
            0 => include_str!("../inputs/day01.txt"),
            1 => INPUT,
            _ => panic!(),
        };

        Day01 {
            input: inp.lines().map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example() {
        let d = get_day(1);

        assert_eq!("24000", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = get_day(1);

        assert_eq!("45000", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("71023", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("206289", d.part2());
    }
}
