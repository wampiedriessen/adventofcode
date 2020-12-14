use super::intcode::{IntcodeProg, Intcode};
use super::Day;

pub struct Day09 {
  input: IntcodeProg
}

impl Day for Day09 {
  fn new(input: &str) -> Day09 {
    Day09 {
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
impl Day09 {
  fn run1(&self) -> i64 {
    let mut t = Intcode::new(&self.input);

    t.stdin(1);

    t.compute();

    return t.stdout().unwrap();
  }

    fn run2(&self) -> i64 {
      let mut t = Intcode::new(&self.input);

      t.stdin(2);
  
      t.compute();
  
      return t.stdout().unwrap();
    }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn part1() {
    let input = include_str!("../inputs/day09.txt").trim();
    let day = Day09::new(input);
    assert_eq!("2204990589", format!("{}", day.part1()));
  }

  #[test]
  fn part2() {
    let input = include_str!("../inputs/day09.txt").trim();
    let day = Day09::new(input);
    assert_eq!("50008", format!("{}", day.part2()));
  }
}