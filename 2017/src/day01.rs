#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn part1_sample_test() {
    assert_eq!(3, run1("1122"));
    assert_eq!(4, run1("1111"));
    assert_eq!(0, run1("1234"));
    assert_eq!(9, run1("91212129"));
  }

  #[test]
  fn part2_sample_test() {
    assert_eq!(6, run2("1212"));
    assert_eq!(0, run2("1221"));
    assert_eq!(4, run2("123425"));
    assert_eq!(12, run2("123123"));
    assert_eq!(4, run2("12131415"));
  }

  #[test]
  fn part1_test() {
    assert_eq!(1223, part1());
  }

  #[test]
  fn part2_test() {
    assert_eq!(1284, part2());
  }
}

pub fn part1() -> u32 {
    let input = include_str!("../inputs/day01.txt").trim();

    return run1(input);
}

pub fn part2() -> u32 {
    let input = include_str!("../inputs/day01.txt").trim();

    return run2(input);
}

fn run1(input:&str) -> u32 {
    let mut sum = 0;
    let mut last:char = ' ';
    let characters = input.trim().chars();
    let mut first:char = ' ';

    for s in characters {
        if first == ' '
        {
            first = s;
        }
        if s == last
        {
            sum += s.to_string().parse::<u32>().unwrap();
        }
        last = s;
    }

    if first == last
    {
        sum += last.to_string().parse::<u32>().unwrap();;
    }

    return sum;
}

fn run2(input:&str) -> u32 {
    let length = input.len();
    let lookahead = length / 2;
    let mut sum = 0;
    for i in 0..length {
        if input.chars().nth(i) == input.chars().nth((i+lookahead) % length)
        {
            sum += input.chars().nth(i).unwrap().to_string().parse::<u32>().unwrap();
        }
    }

    return sum;
}