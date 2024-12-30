use std::collections::HashMap;
use aoc_util::PuzzleDay;

pub struct Day01 {
    list1: Vec<i32>,
    list2: Vec<i32>,
}

impl PuzzleDay for Day01 {
    fn new(input: &str) -> Self {
        let mut list1 = Vec::new();
        let mut list2 = Vec::new();

        let mut flip = false;
        for word in input.split_ascii_whitespace() {
            if !flip {
                list1.push(word.parse::<i32>().unwrap());
            } else {
                list2.push(word.parse::<i32>().unwrap());
            }
            flip = !flip;
        }

        list1.sort();
        list2.sort();

        Day01 {
            list1,
            list2
        }
    }

    fn solve(&mut self) -> Result<(String, String), String> {
        let mut sum1 = 0;
        for i in 0..self.list1.len() {
            sum1 += (self.list1[i] - self.list2[i]).abs();
        }

        let mut scores = HashMap::new();
        for i in &self.list2 {
            *scores.entry(i).or_insert(0) += i;
        }
        let mut sum2 = 0;
        for i in &self.list1 {
            sum2 += scores.get(&i).unwrap_or(&0);
        }

        Ok((sum1.to_string(), sum2.to_string()))
    }
}