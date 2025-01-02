use regex::Regex;
use lazy_static::lazy_static;
use aoc_util::PuzzleDay;

pub struct Day03 {
    input: String,
}

impl PuzzleDay for Day03 {
    fn new(input: &str) -> Self {
        Day03 { input: String::from(input) }
    }

    fn solve(&mut self) -> Result<(String, String), String> {
        Ok((self.part1().to_string(), self.part2().to_string()))
    }
}

lazy_static! {
    static ref day3_re: Regex = Regex::new(r"(mul\((?<a>[0-9]+),(?<b>[0-9]+)\)|do\(\)|don't\(\))").unwrap();
}

impl Day03 {
    fn part1(&self) -> i32 {
        let num_matched: i32 = day3_re.captures_iter(&self.input)
            .map(|capt|
                if (&capt[0]).starts_with("mul") {
                    (capt["a"].parse::<i32>().unwrap(), capt["b"].parse::<i32>().unwrap())
                } else { (0, 0) }
            )
            .map(|(a, b)| a * b)
            .sum();

        num_matched
    }

    fn part2(&self) -> i32 {
        let mut sum: i32 = 0;
        let mut recording = true;

        for cap in day3_re.captures_iter(&self.input) {
            if (&cap[0]).starts_with("don") {
                recording = false;
            } else if (&cap[0]).starts_with("do") {
                recording = true;
            } else if recording {
                sum += cap["a"].parse::<i32>().unwrap() * cap["b"].parse::<i32>().unwrap();
            }
        }

        sum
    }
}