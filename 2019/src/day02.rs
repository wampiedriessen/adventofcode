pub fn read_input(input:&str) -> Vec<usize> {
    let mut vec: Vec<usize> = Vec::new();
    for op in input.split(",") {
        vec.push(op.parse().unwrap())
    }
    return vec;
}

pub fn compute(mut program:Vec<usize>) -> Vec<usize> {
    let mut pc = 0;
    while program[pc] != 99 {
        let op = program[pc];
        let a: usize = program[pc+1];
        let b: usize = program[pc+2];
        let dst: usize = program[pc+3];
        if op == 1 {
            program[dst] = program[a] + program[b];
        }
        if op == 2 {
            program[dst] = program[a] * program[b];
        }
        pc += 4;
    }
    return program;
}

pub fn part1() -> usize {
    let input = include_str!("../inputs/day02.txt").trim();

    return run1(input);
}

pub fn part2() -> usize {
    let input = include_str!("../inputs/day02.txt").trim();

    return run2(input, 19690720);
}

fn run1(input:&str) -> usize {
    let mut program = read_input(input);

    program[1] = 12;
    program[2] = 2;

    let outcome = compute(program);

    return outcome[0];
}

fn run2(input:&str, goal: usize) -> usize {
    let orig_program = read_input(input);

    for noun in 0..100 {
        for verb in 0..100 {
            let mut program = orig_program.clone();
            program[1] = noun;
            program[2] = verb;

            if compute(program)[0] == goal {
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
    
    assert_eq!(vec![2,0,0,0,99], compute(vec![1,0,0,0,99]));
    assert_eq!(vec![2,3,0,6,99], compute(vec![2,3,0,3,99]));
    assert_eq!(vec![2,4,4,5,99,9801], compute(vec![2,4,4,5,99,0]));
    assert_eq!(vec![30,1,1,4,2,5,6,0,99], compute(vec![1,1,1,4,99,5,6,0,99]));
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