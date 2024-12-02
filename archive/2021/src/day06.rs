use crate::Day;

pub struct Day06 {
    pub input: Vec<String>,
}

struct FishSchool {
    pub map: [i64; 9],
}

impl FishSchool {
    pub fn add_seven_days(&mut self) {
        let mut addmap = [0; 9];

        for f in 0..7 {
            addmap[f + 2] += self.map[f];
        }
        for f in 7..9 {
            addmap[f] -= self.map[f];
            addmap[f - 7] += self.map[f];
        }
        for f in 0..9 {
            self.map[f] += addmap[f];
        }
    }

    pub fn num_fish_with_extra_days(&self, d: i64) -> i64 {
        let mut sum = 0;

        for f in 0..9 {
            sum += self.map[f];
            if f < d as usize {
                sum += self.map[f];
            }
        }

        sum
    }
}

impl std::str::FromStr for FishSchool {
    type Err = core::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let ints: Vec<usize> = s.split(",").map(|f| f.parse().unwrap()).collect();

        let mut map = [0; 9];
        for int in ints {
            map[int] += 1;
        }

        Ok(FishSchool { map })
    }
}

impl Day06 {
    fn calculate_for_days(&self, mut school: FishSchool, mut days: i64) -> String {
        while days > 7 {
            school.add_seven_days();
            days -= 7;
        }

        school.num_fish_with_extra_days(days).to_string()
    }
}

impl Day for Day06 {
    fn part1(&self) -> String {
        let school: FishSchool = self.input[0].parse().unwrap();

        self.calculate_for_days(school, 80)
    }

    fn part2(&self) -> String {
        let school: FishSchool = self.input[0].parse().unwrap();

        self.calculate_for_days(school, 256)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &str = "3,4,3,1,2";

    fn get_day(input_num: u8) -> Day06 {
        let inp = match input_num {
            0 => include_str!("../inputs/day06.txt"),
            1 => INPUT,
            _ => panic!(),
        };

        Day06 {
            input: inp.split('\n').map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example() {
        let d = get_day(1);

        assert_eq!("5934", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = get_day(1);

        assert_eq!("26984457539", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("345387", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("1574445493136", d.part2());
    }
}
