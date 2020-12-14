use super::Day;

pub struct Day01 {
  input: Vec<u32>
}

impl Day for Day01 {
  fn new(input: &str) -> Day01 {
    Day01 {
      input: input.lines().map(|l| l.parse().unwrap()).collect(),
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
impl Day01 {
  fn fuel_usage(&self, mass: u32) -> u32 {
    return (mass / 3) - 2;
  }

  fn run1(&self) -> u32 {
    let mut fuel_need: u32 = 0;
    for mass in &self.input {
      fuel_need += self.fuel_usage(*mass);
    }
    return fuel_need;
  }

  fn fuel_need_r(&self, mass: u32) -> u32 {
    if mass < 6 { return 0; }
    let extra_fuel = self.fuel_usage(mass);
    return extra_fuel + self.fuel_need_r(extra_fuel);
  }

  fn run2(&self) -> u32 {
    let mut fuel_need: u32 = 0;
    for mass in &self.input {
      fuel_need += self.fuel_need_r(*mass);
    }
    return fuel_need;
  }

}

#[cfg(test)]
mod tests {
  use super::*;

  const EXAMPLE: &str = "12
14
1969
100756
";

  #[test]
  fn part1_sample_test() {
    let day = Day01::new(EXAMPLE);
    
    assert_eq!(34241, day.run1());
  }

  #[test]
  fn part2_sample_test() {
    let day = Day01::new(EXAMPLE);
    
    assert_eq!(51316, day.run2());
  }

  #[test]
  fn part1_test() {
    let input = include_str!("../inputs/day01.txt").trim();
    let day = Day01::new(input);
    assert_eq!("3405637", format!("{}", day.part1()));
  }

  #[test]
  fn part2_test() {
    let input = include_str!("../inputs/day01.txt").trim();
    let day = Day01::new(input);
    assert_eq!("5105597", format!("{}", day.part2()));
  }
}