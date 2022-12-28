use std::collections::HashSet;
use std::str::FromStr;
use crate::Day;

pub struct Day03 {
    pub input: Vec<String>,
}

struct Rucksack {
    compartment1: HashSet<char>,
    compartment2: HashSet<char>,
    rs: HashSet<char>
}

impl Rucksack {
    fn non_segregated_item(&self) -> &char {
        self.compartment1
            .intersection(&self.compartment2)
            .next().unwrap()
    }

    fn badge_item_type<'a>(&'a self, other1: &'a Rucksack, other2: &'a Rucksack) -> &'a char {
        self.rs.intersection(&other1.rs).collect::<HashSet<&char>>()
            .intersection(&other2.rs.iter().collect())
            .next().unwrap()
    }
}

impl FromStr for Rucksack {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut c1 = HashSet::new();
        let mut c2 = HashSet::new();
        let mut rs = HashSet::new();

        let m = s.len();

        for (i, item) in s.chars().enumerate() {
            if i < m/2 {
                c1.insert(item);
            } else {
                c2.insert(item);
            }
            rs.insert(item);
        }

        Ok(Rucksack {
            compartment1: c1,
            compartment2: c2,
            rs
        })
    }
}

fn priority(x: &char) -> u32 {
    (if x.is_ascii_lowercase() {
        (*x as u8) - b'a' + 1
    } else {
        (*x as u8) - b'A' + 27
    }).into()
}

impl Day for Day03 {
    fn part1(&self) -> String {
        let mut sum = 0;

        for line in &self.input {
            let rucksack: Rucksack = line.parse().unwrap();
            sum += priority(rucksack.non_segregated_item());
        }

        sum.to_string()
    }

    fn part2(&self) -> String {
        let mut sum = 0;

        let groups: Vec<&[String]> = self.input.chunks(3).collect();

        for group in groups {
            let rs0: Rucksack = group[0].parse().unwrap();
            let rs1: Rucksack = group[1].parse().unwrap();
            let rs2: Rucksack = group[2].parse().unwrap();

            let badge = rs0.badge_item_type(&rs1, &rs2);

            sum += priority(badge);
        }

        sum.to_string()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &str = "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw";

    fn get_day(input_num: u8) -> Day03 {
        let inp = match input_num {
            0 => include_str!("../inputs/day03.txt"),
            1 => INPUT,
            _ => panic!(),
        };

        Day03 {
            input: inp.lines().map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example() {
        let d = get_day(1);

        assert_eq!("157", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = get_day(1);

        assert_eq!("70", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("7967", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("2716", d.part2());
    }
}
