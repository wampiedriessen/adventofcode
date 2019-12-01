pub fn part1() -> u32 {
    let input = include_str!("../inputs/day01.txt").trim();

    return run1(input);
}

pub fn part2() -> u32 {
    let input = include_str!("../inputs/day01.txt").trim();

    return run2(input);
}

fn fuel_usage(mass: u32) -> u32 {
  return (mass / 3) - 2;
}

fn run1(input:&str) -> u32 {
  let mut fuel_need: u32 = 0;
  for mass in str::lines(input) {
    fuel_need += fuel_usage(mass.parse().unwrap());
  }
  return fuel_need;
}

fn fuel_need_r(mass: u32) -> u32 {
  if mass < 6 { return 0; }
  let extra_fuel = fuel_usage(mass);
  return extra_fuel + fuel_need_r(extra_fuel);
}

fn run2(input:&str) -> u32 {
  let mut fuel_need: u32 = 0;
  for mass in str::lines(input) {
    fuel_need += fuel_need_r(mass.parse().unwrap());
  }
  return fuel_need;
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
    
    assert_eq!(34241, run1(EXAMPLE));
  }

  #[test]
  fn part2_sample_test() {
    assert_eq!(51316, run2(EXAMPLE));
  }

  #[test]
  fn part1_test() {
    assert_eq!(3405637, part1());
  }

  #[test]
  fn part2_test() {
    assert_eq!(5105597, part2());
  }
}