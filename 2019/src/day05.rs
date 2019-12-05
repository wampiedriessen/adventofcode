use super::intcode::{Intcode,IntcodeProg};
use super::Day;

pub struct Day05 {
  input: IntcodeProg
}

impl Day for Day05 {
  fn new(input: &str) -> Day05 {
    Day05 {
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
impl Day05 {
    fn run1(&self) -> i32 {
        return self.boot_system_id(1);
    }

    fn run2(&self) -> i32 {
        return self.boot_system_id(5);
    }

    fn boot_system_id(&self, system_id: i32) -> i32 {
        let mut t = Intcode::new(&self.input);

        t.stdin(system_id);
        t.compute();

        let mut last_output = -1;
        while let Some(output) = t.stdout() {
            last_output = output;
        }

        return last_output;
    }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn part1_test() {
    let input = include_str!("../inputs/day05.txt").trim();
    let day = Day05::new(input);

    assert_eq!("16225258", format!("{}", day.part1()));
  }

  #[test]
  fn part2_test() {
    let input = include_str!("../inputs/day05.txt").trim();
    let day = Day05::new(input);

    assert_eq!("2808771", format!("{}", day.part2()));
  }
}