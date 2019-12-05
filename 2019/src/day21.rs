use super::Day;

pub struct Day21 {
  _input: u32
}

impl Day for Day21 {
  fn new(_input: &str) -> Day21 {
    Day21 {
      _input: 0,
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
impl Day21 {
    fn run1(&self) -> i32 {
        return 0;
    }

    fn run2(&self) -> i32 {
        return 0;
    }
}