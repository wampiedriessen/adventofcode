use std::collections::HashMap;

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn part1_sample_test() {
    let sample_input = "0: 3
1: 2
4: 4
6: 4";

    assert_eq!(24, run1(parse_input(sample_input)));
  }

  #[test]
  fn part2_sample_test() {
    let sample_input = "0: 3
1: 2
4: 4
6: 4";

    assert_eq!(10, run2(parse_input(sample_input)));
  }

  #[test]
  fn part1_test() {
    assert_eq!(1504, part1());
  }

  #[test]
  fn part2_test() {
    assert_eq!(3823370, part2());
  }
}

pub fn part1() -> i32 {
    let input = include_str!("../inputs/day13.txt");

    run1(parse_input(input))
}

pub fn part2() -> i32 {
    let input = include_str!("../inputs/day13.txt");

    run2(parse_input(input))
}

fn run1(scanners:HashMap<i32, i32>) -> i32 {
    let mut severity = 0;
    for index in scanners.keys() {
        let range = scanners.get(index).unwrap();

        if index % ((range-1)*2) == 0 {
            severity += index*range;
        }
    }
    return severity;
}

fn run2(scanners:HashMap<i32, i32>) -> i32 {
    let mut delay = 0;

    let mut unseen:bool = false;
    while !unseen {
        unseen = true;
        for index in scanners.keys() {
            let range = scanners.get(index).unwrap();

            if (delay+index) % ((range-1)*2) == 0 {
                unseen = false;
                break;
            }
        }
        delay += 1;
    }
    
    delay-1
}

fn parse_input(input:&str) -> HashMap<i32, i32> {
    let lines:Vec<&str> = input
        .split("\n")
        .collect();

    let mut scanners:HashMap<i32, i32> = HashMap::new();
    for l in lines {
        let split:Vec<&str> = l.split(": ").collect();

        let index = split[0].parse::<i32>().unwrap();
        let range = split[1].parse::<i32>().unwrap();

        scanners.insert(index, range);
    }
    scanners
}