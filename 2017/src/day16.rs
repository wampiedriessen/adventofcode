#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn part1_sample_test() {
    let instr = parse_input("s1,x3/4,pe/b");
    assert_eq!("baedc", run1(&instr,vec!['a','b','c','d','e']));
  }

  #[test]
  fn part2_sample_test() {
    let instr = parse_input("s1,x3/4,pe/b");
    assert_eq!("abcde", run2(&instr,vec!['a','b','c','d','e']));
  }

  #[test]
  fn part1_test() {
    assert_eq!("doeaimlbnpjchfkg", part1());
  }

  #[test]
  fn part2_test() {
    assert_eq!("agndefjhibklmocp", part2());
  }
}

struct Instruction {
    fun:u8,
    p1:char,
    p2:char,
    a:u8,
    b:u8,
}

pub fn part1() -> String {
    let input = include_str!("../inputs/day16.txt").trim();

    let line = start_dance();

    return run1(&parse_input(input), line);
}

pub fn part2() -> String {
    let input = include_str!("../inputs/day16.txt").trim();

    let line = start_dance();

    return run2(&parse_input(input), line);
}

fn run1(instructions:&Vec<Instruction>, mut line:Vec<char>) -> String {
    for mv in instructions {
        if mv.fun == 0 {
            for _ in 0..mv.a {
                let c = line.pop().unwrap();
                line.insert(0, c);
            }
        }
        if mv.fun == 1 {
            line.swap(mv.a as usize, mv.b as usize);
        }
        if mv.fun == 2 {
            let a:usize = line.iter().position(|&x| x == mv.p1).unwrap();
            let b:usize = line.iter().position(|&x| x == mv.p2).unwrap();

            line.swap(a, b);
        }
    }
    return line.into_iter().collect();
}

fn run2(instructions:&Vec<Instruction>, line:Vec<char>) -> String {
    let mut line:String = line.into_iter().collect();
    let mut seen:Vec<String> = Vec::new();
    for i in 0..1000000000 {
        if seen.contains(&line) {
            return seen[1000000000 % i].clone();
        }
        seen.push(line.clone());
        line = run1(&instructions, line.chars().collect());
    }
    return line;
}

fn start_dance() -> Vec<char> {
    let mut line:Vec<char> = Vec::with_capacity(16);

    for i in 0..16 {
        line.push((('a' as u8) + i) as char);
    }

    return line;
}

fn parse_input(input:&str) -> Vec<Instruction> {
    let mut instructions:Vec<Instruction> = Vec::new();
    let moves:Vec<&str> = input.split(",").collect();

    for mv in moves {
        let mut chars:Vec<char> = mv.chars().collect();
        let first = chars[0];
        chars = chars.split_off(1);
        let arg2:String = chars.into_iter().collect();
        if first == 's' {
            instructions.push(Instruction {
                fun: 0,
                a: arg2.parse::<u8>().unwrap(),
                b: 0,
                p1: '\t',
                p2: '\t',
            });
            continue;
        }
        let progs:Vec<&str> = arg2.split("/").collect();
        if first == 'x' {
            instructions.push(Instruction {
                fun: 1,
                a: progs[0].parse::<u8>().unwrap(),
                b: progs[1].parse::<u8>().unwrap(),
                p1: '\t',
                p2: '\t',
            });
        }
        if first == 'p' {
            instructions.push(Instruction {
                fun: 2,
                a: 0,
                b: 0,
                p1: progs[0].chars().next().unwrap(),
                p2: progs[1].chars().next().unwrap(),
            });
        }
    }
    return instructions;
}