use super::Day;
use super::intcode::{Intcode,IntcodeProg};

pub struct Day02 {
  input: IntcodeProg
}

impl Day for Day02 {
  fn new(input: &str) -> Day02 {
    Day02 {
      input: Intcode::read_input(input),
    }
  }

  fn part1(&self) -> Box<dyn std::fmt::Display> {
    return Box::new(self.run1());
  }

  fn part2(&self) -> Box<dyn std::fmt::Display> {
    return Box::new(self.run2(19690720));
  }
}

// -- Privates
impl Day02 {
  
  fn compute(&self, program: IntcodeProg) -> i32 {
    let mut t = Intcode::new(program);

    t.compute();

    return t.result();
  }

  fn run1(&self) -> i32 {
    let mut program = self.input.clone();

    program[1] = 12;
    program[2] = 2;

    return self.compute(program);
  }

  fn run2(&self, goal: i32) -> i32 {
    let orig_program = self.input.clone();

    for noun in 0..100 {
        for verb in 0..100 {
            let mut program = orig_program.clone();
            program[1] = noun;
            program[2] = verb;

            if self.compute(program) == goal {
                return 100 * noun + verb;
            }
        }
    }

    panic!("Goal not found");
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn part1_sample_test() {
    let day = Day02::new("99");
    
    assert_eq!(2, day.compute(vec![1,0,0,0,99]));
    assert_eq!(2, day.compute(vec![2,3,0,3,99]));
    assert_eq!(2, day.compute(vec![2,4,4,5,99,0]));
    assert_eq!(30, day.compute(vec![1,1,1,4,99,5,6,0,99]));
  }

  #[test]
  fn part2_sample_test() {
    let input = include_str!("../inputs/day02.txt").trim();
    let day = Day02::new(input);
    assert_eq!(1202, day.run2(4714701));
  }

  #[test]
  fn part1_test() {
    let input = include_str!("../inputs/day02.txt").trim();
    let day = Day02::new(input);
    assert_eq!("4714701", format!("{}", day.part1()));
  }

  #[test]
  fn part2_test() {
    let input = include_str!("../inputs/day02.txt").trim();
    let day = Day02::new(input);
    assert_eq!("5121", format!("{}", day.part2()));
  }
}