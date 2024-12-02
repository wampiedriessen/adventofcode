use std::str::FromStr;
use crate::Day;

pub struct Day04 {
    pub input: Vec<String>,
}

struct SectionAssignment {
    section1: (u32, u32),
    section2: (u32, u32)
}

impl SectionAssignment {
    fn is_contained(&self) -> bool {
        self.section1.0 <= self.section2.0 &&
            self.section1.1 >= self.section2.1 ||
        self.section2.0 <= self.section1.0 &&
            self.section2.1 >= self.section1.1
    }

    fn overlap(&self) -> bool {
        self.is_contained() ||
        self.section1.0 <= self.section2.1 &&
            self.section1.0 >= self.section2.0 ||
        self.section2.0 <= self.section1.1 &&
            self.section2.0 >= self.section1.0
    }
}

impl FromStr for SectionAssignment {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let sa: Vec<&str> = s.split(',').collect();

        let l: Vec<&str> = sa.first().unwrap().split('-').collect();
        let r: Vec<&str> = sa.get(1).unwrap().split('-').collect();

        Ok(SectionAssignment {
            section1: (l.first().unwrap().parse().unwrap(), l.get(1).unwrap().parse().unwrap()),
            section2: (r.first().unwrap().parse().unwrap(), r.get(1).unwrap().parse().unwrap())
        })
    }
}

impl Day for Day04 {
    fn part1(&self) -> String {
        let mut sum = 0;

        for line in &self.input {
            let sa: SectionAssignment = line.parse().unwrap();

            if sa.is_contained() {
                sum += 1;
            }
        }

        sum.to_string()
    }

    fn part2(&self) -> String {
        let mut sum = 0;

        for line in &self.input {
            let sa: SectionAssignment = line.parse().unwrap();

            if sa.overlap() {
                sum += 1;
            }
        }

        sum.to_string()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &str = "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8";

    fn get_day(input_num: u8) -> Day04 {
        let inp = match input_num {
            0 => include_str!("../inputs/day04.txt"),
            1 => INPUT,
            _ => panic!(),
        };

        Day04 {
            input: inp.lines().map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example() {
        let d = get_day(1);

        assert_eq!("2", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = get_day(1);

        assert_eq!("4", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("431", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("823", d.part2());
    }
}
