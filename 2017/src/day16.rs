use std::collections::HashMap;

#[cfg(test)]
mod tests {
  use super::*;

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

    let instructions = parse_input(input);

    return run1(&instructions, line.clone()).into_iter().collect();
}

pub fn part2() -> String {
    let input = include_str!("../inputs/day16.txt").trim();

    return run2(parse_input(input));
}

fn run1(instructions:&Vec<Instruction>, mut line:Vec<char>) -> Vec<char> {
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
    return line;
}

fn run2(instructions:Vec<Instruction>, ) -> String {
    let line2 = start_dance();
    let mut line:String = line2.into_iter().collect();
    let mut seen:HashMap<String, String> = HashMap::new();
    for _ in 0..1000000000 {
        if seen.contains_key(&line) {
            line = seen.get(&line).unwrap().clone();
        } else {
            let lin = run1(&instructions, line.chars().collect());
            seen.insert(line, lin.clone().into_iter().collect());
            line = lin.into_iter().collect();
        }
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
        }
        if first == 'x' {
            let progs:Vec<&str> = arg2.split("/").collect();
            instructions.push(Instruction {
                fun: 1,
                a: progs[0].parse::<u8>().unwrap(),
                b: progs[1].parse::<u8>().unwrap(),
                p1: '\t',
                p2: '\t',
            });
        }
        if first == 'p' {
            let progs:Vec<&str> = arg2.split("/").collect();
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