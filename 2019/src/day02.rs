use super::intcode::Intcode;

pub fn part1() -> i32 {
    let input = include_str!("../inputs/day02.txt").trim();

    return run1(input);
}

pub fn part2() -> i32 {
    let input = include_str!("../inputs/day02.txt").trim();

    return run2(input, 19690720);
}

fn compute(program: Vec<i32>) -> i32 {
    let mut t = Intcode::new(program);

    t.compute();

    return t.result();
}

fn run1(input:&str) -> i32 {
    let mut program = Intcode::read_input(input);

    program[1] = 12;
    program[2] = 2;

    return compute(program);
}

fn run2(input:&str, goal: i32) -> i32 {
    let orig_program = Intcode::read_input(input);

    for noun in 0..100 {
        for verb in 0..100 {
            let mut program = orig_program.clone();
            program[1] = noun;
            program[2] = verb;

            if compute(program) == goal {
                return 100 * noun + verb;
            }
        }
    }

    panic!("Goal not found");
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn part1_sample_test() {
    
    assert_eq!(2, compute(vec![1,0,0,0,99]));
    assert_eq!(2, compute(vec![2,3,0,3,99]));
    assert_eq!(2, compute(vec![2,4,4,5,99,0]));
    assert_eq!(30, compute(vec![1,1,1,4,99,5,6,0,99]));
  }

  #[test]
  fn part2_sample_test() {
    let input = include_str!("../inputs/day02.txt").trim();
    assert_eq!(1202, run2(input, 4714701));
  }

  #[test]
  fn part1_test() {
    assert_eq!(4714701, part1());
  }

  #[test]
  fn part2_test() {
    assert_eq!(5121, part2());
  }
}