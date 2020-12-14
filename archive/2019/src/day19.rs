use super::Day;
use intcode::{Intcode, IntcodeProg};

pub struct Day19 {
  input: IntcodeProg,
}

impl Day for Day19 {
  fn new(input: &str) -> Day19 {
    Day19 {
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
impl Day19 {
  fn run1(&self) -> i32 {
    let mut count = 0;
    for x in 0..50 {
      for y in 0..50 {
        let mut p = Intcode::new(&self.input);
        p.stdin(x);
        p.stdin(y);
        p.compute();
        let x = p.stdout().unwrap();
        if x == 1 {
          count += 1;
        }
      }
    }
    count
  }

  fn run2(&self) -> i32 {
    return 0;
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn part1() {
    let day = Day19::new(include_str!("../inputs/day19.txt"));
    assert_eq!(day.run1(), 0);
  }

  #[test]
  fn part2() {
    let day = Day19::new(include_str!("../inputs/day19.txt"));
    assert_eq!(day.run2(), 0);
  }
}
