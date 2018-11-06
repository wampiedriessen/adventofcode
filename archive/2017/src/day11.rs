#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn part1_sample_test() {
    assert_eq!(3, run1("ne,ne,ne"));
    assert_eq!(0, run1("ne,ne,sw,sw"));
    assert_eq!(2, run1("ne,ne,s,s"));
    assert_eq!(3, run1("se,sw,se,sw,sw"));
  }

  #[test]
  fn part2_sample_test() {
    // nothing provided
  }

  #[test]
  fn part1_test() {
    assert_eq!(707, part1());
  }

  #[test]
  fn part2_test() {
    assert_eq!(1490, part2());
  }
}

pub fn part1() -> i32 {
    let input = include_str!("../inputs/day11.txt");

    run1(input)
}

pub fn part2() -> i32 {
    let input = include_str!("../inputs/day11.txt");

    run2(input)
}

fn run1(input:&str) -> i32 {
    let mut nw: i32 = 0;
    let mut n: i32 = 0;
    let mut ne: i32 = 0;

    for dir in input.trim().split(",") {
        step(dir, &mut n, &mut ne, &mut nw);
    }

    (n+ne+nw).abs()
}

fn run2(input:&str) -> i32 {
    let mut nw: i32 = 0;
    let mut n: i32 = 0;
    let mut ne: i32 = 0;
    let mut max: i32 = 0;

    for dir in input.trim().split(",") {
        step(dir, &mut n, &mut ne, &mut nw);

        if n+ne+nw > max {
            max = n+ne+nw;
        }
    }

    max
}

fn step(dir:&str, n:&mut i32, ne:&mut i32, nw:&mut i32) {
    match dir {
        "nw" => *nw += 1,
        "n" => *n += 1,
        "ne" => *ne += 1,
        "sw" => *ne -= 1,
        "s" => *n -= 1,
        "se" => *nw -= 1,
        _ => panic!("gekke input")
    }
    while *nw > 0 && *ne > 0 {
        *nw -= 1;
        *ne -= 1;
        *n += 1;
    }
    while (*n > 0 && *ne < 0) || (*n > 0 && *nw < 0) {
        *n -= 1;
        *ne += 1;
        *nw += 1;
    }
    while *nw < 0 && *ne < 0 {
        *nw += 1;
        *ne += 1;
        *n -= 1;
    }
    while (*n < 0 && *ne > 0) || (*n < 0 && *nw > 0) {
        *n += 1;
        *ne -= 1;
        *nw -= 1;
    }
}