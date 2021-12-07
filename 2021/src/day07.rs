use crate::Day;

pub struct Day07 {
    pub input: Vec<String>,
}

impl Day07 {
    fn average(&self, crabs: &Vec<i64>) -> f64 {
        crabs.iter().sum::<i64>() as f64 / crabs.len() as f64
    }

    fn median(&self, crabs: &mut Vec<i64>) -> i64 {
        crabs.sort();
        let mid = crabs.len() / 2;
        crabs[mid]
    }

    fn crab_fuel_use(&self, crabs: &Vec<i64>, goto: i64) -> i64 {
        crabs.iter().map(|c| (1..=(c - goto).abs()).sum::<i64>()).sum()
    }
}

impl Day for Day07 {
    fn part1(&self) -> String {
        let mut crabs: Vec<i64> = self.input[0].split(",").map(|c| c.parse().unwrap()).collect();

        let goto = self.median(&mut crabs);

        let fuel_use: i64 = crabs.iter().map(|c| (c - goto).abs()).sum();
        
        fuel_use.to_string()
    }

    fn part2(&self) -> String {
        let crabs: Vec<i64> = self.input[0].split(",").map(|c| c.parse().unwrap()).collect();

        let goto = self.average(&crabs);

        let fuel_use1: i64 = self.crab_fuel_use(&crabs, goto.floor() as i64);
        let fuel_use2: i64 = self.crab_fuel_use(&crabs, goto.ceil() as i64);
        
        fuel_use1.min(fuel_use2).to_string()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &str = "16,1,2,0,4,2,7,1,2,14";

    fn get_day(input_num: u8) -> Day07 {
        let inp = match input_num {
            0 => include_str!("../inputs/day07.txt"),
            1 => INPUT,
            _ => panic!(),
        };

        Day07 {
            input: inp.split('\n').map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example() {
        let d = get_day(1);

        assert_eq!("37", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = get_day(1);

        assert_eq!("168", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("336120", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("96864235", d.part2());
    }
}
