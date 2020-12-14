use super::intcode::{Intcode, IntcodeProg};
use super::Day;

use std::cmp::max;

pub struct Day07 {
  input: IntcodeProg,
}

impl Day for Day07 {
  fn new(input: &str) -> Day07 {
    Day07 {
      input: Intcode::read_input(input),
    }
  }

  fn part1(&self) -> Box<dyn std::fmt::Display> {
    return Box::new(self.run1());
  }

  fn part2(&self) -> Box<dyn std::fmt::Display> {
    return Box::new(self.run2());
  }
}

// -- Privates
impl Day07 {

  fn run_amps(&self, config: [i64; 5]) -> i64
  {
    let mut amps = [
      Intcode::new(&self.input),
      Intcode::new(&self.input),
      Intcode::new(&self.input),
      Intcode::new(&self.input),
      Intcode::new(&self.input),
    ];

    for i in 0..5 {
      amps[i].stdin(config[i]);
    }

    let mut input = 0;

    while !amps[4].is_finished() {
      for i in 0..5 {
        amps[i].stdin(input);

        amps[i].compute();

        input = amps[i].stdout().unwrap();
      }
    }

    return input;
  }

  fn permutations(&self, range: std::ops::Range<i64>) -> Vec<[i64; 5]> {
    let mut vec = Vec::new();

    for a in range.clone() {
      for b in range.clone() {
        if b == a { continue; }
        for c in range.clone() {
          if c == a || c == b { continue; }
          for d in range.clone() {
            if d == a || d == b || d == c { continue; }
            for e in range.clone() {
              if e == a || e == b || e == c || e == d { continue; }
              vec.push([a, b, c, d, e]);
            }
          }
        }
      }
    }
    return vec;
  }

  fn run1(&self) -> i64 {
    let mut max_output = 0;

    for perm in self.permutations(0..5) {
      max_output = max(self.run_amps(perm), max_output);
    }
    return max_output;
  }

  fn run2(&self) -> i64 {
    let mut max_output = 0;

    for perm in self.permutations(5..10) {
      max_output = max(self.run_amps(perm), max_output);
    }
    return max_output;
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  const EXAMPLE1: &str = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0";
  const EXAMPLE2: &str = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0";
  const EXAMPLE3: &str = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0";

  #[test]
  fn part1_example_tests() {
    let day1 = Day07::new(EXAMPLE1);
    assert_eq!(43210, day1.run1());

    let day2 = Day07::new(EXAMPLE2);
    assert_eq!(54321, day2.run1());

    let day3 = Day07::new(EXAMPLE3);
    assert_eq!(65210, day3.run1());
  }

  #[test]
  fn part1_phase_setting_tests() {
    let day1 = Day07::new(EXAMPLE1);
    assert_eq!(43210, day1.run_amps([4,3,2,1,0]));

    let day2 = Day07::new(EXAMPLE2);
    assert_eq!(54321, day2.run_amps([0,1,2,3,4]));

    let day3 = Day07::new(EXAMPLE3);
    assert_eq!(65210, day3.run_amps([1,0,4,3,2]));
  }

  const EXAMPLE4: &str = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5";
  const EXAMPLE5: &str = "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10";

  #[test]
  fn part2_example_tests() {
    let day1 = Day07::new(EXAMPLE4);
    assert_eq!(139629729, day1.run2());

    let day2 = Day07::new(EXAMPLE5);
    assert_eq!(18216, day2.run2());
  }

  #[test]
  fn part2_phase_setting_tests() {
    let day1 = Day07::new(EXAMPLE4);
    assert_eq!(139629729, day1.run_amps([9,8,7,6,5]));

    let day2 = Day07::new(EXAMPLE5);
    assert_eq!(18216, day2.run_amps([9,7,8,5,6]));
  }

  #[test]
  fn part1_tests() {
    let input = include_str!("../inputs/day07.txt").trim();
    let day = Day07::new(input);
    assert_eq!("43812", format!("{}", day.part1()));
  }

  #[test]
  fn part2_tests() {
    let input = include_str!("../inputs/day07.txt").trim();
    let day = Day07::new(input);
    assert_eq!("59597414", format!("{}", day.part2()));
  }
}