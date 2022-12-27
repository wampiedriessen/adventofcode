use std::str::FromStr;
use crate::Day;
use crate::day02::RPS::{Paper, Rock, Scissors};
use crate::day02::WDL::{Draw, Lose, Win};

pub struct Day02 {
    pub input: Vec<String>,
}

#[derive(PartialEq, Clone, Copy)]
enum RPS {
    Rock,
    Paper,
    Scissors
}

#[derive(PartialEq, Clone, Copy)]
enum WDL {
    Lose,
    Draw,
    Win
}

struct Tactic {
    request: RPS,
    response: RPS,
    endgame: WDL
}

impl RPS {
    pub fn point(&self) -> i32 {
        match self {
            Rock => 1,
            Paper => 2,
            Scissors => 3,
        }
    }

    pub fn winner(&self) -> RPS {
        match self {
            Rock => Paper,
            Paper => Scissors,
            Scissors => Rock,
        }
    }

    pub fn loser(&self) -> RPS {
        match self {
            Rock => Scissors,
            Paper => Rock,
            Scissors => Paper,
        }
    }

}

impl Tactic {
    pub fn points1(&self) -> i32 {
        (match (self.request, self.response) {
            (x, y) if (y == x.winner()) => 6,
            (x, y) if (x == y) => 3,
            _ => 0
        }) + self.response.point()
    }

    pub fn points2(&self) -> i32 {
        (match self.endgame {
            Lose => 0,
            Draw => 3,
            Win => 6
        }) + (match (self.endgame, self.request) {
            (Draw, x) => x.point(),
            (Win, x) => x.winner().point(),
            (Lose, x) => x.loser().point()
        })
    }
}

impl FromStr for Tactic {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let request = match s.chars().nth(0).unwrap() {
            'A' => Rock,
            'B' => Paper,
            'C' => Scissors,
            _ => panic!()
        };

        let response = match s.chars().nth(2).unwrap() {
            'X' => Rock,
            'Y' => Paper,
            'Z' => Scissors,
            _ => panic!()
        };

        let endgame = match s.chars().nth(2).unwrap() {
            'X' => Lose,
            'Y' => Draw,
            'Z' => Win,
            _ => panic!()
        };

        Ok(Tactic {
            request,
            response,
            endgame
        })
    }
}

impl Day for Day02 {
    fn part1(&self) -> String {
        let mut score = 0;
        for cals in &self.input {
            let tactic: Tactic = cals.parse().unwrap();

            score += tactic.points1();
        }

        score.to_string()
    }

    fn part2(&self) -> String {
        let mut score = 0;
        for cals in &self.input {
            let tactic: Tactic = cals.parse().unwrap();

            score += tactic.points2();
        }

        score.to_string()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &str = "A Y
B X
C Z";

    fn get_day(input_num: u8) -> Day02 {
        let inp = match input_num {
            0 => include_str!("../inputs/day02.txt"),
            1 => INPUT,
            _ => panic!(),
        };

        Day02 {
            input: inp.lines().map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example() {
        let d = get_day(1);

        assert_eq!("15", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = get_day(1);

        assert_eq!("12", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("14297", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("10498", d.part2());
    }
}
