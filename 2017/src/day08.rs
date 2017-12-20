use std::collections::HashMap;

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE_INPUT:&str = "b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10";

    #[test]
    fn part1_sample_test() {
        assert_eq!(1, run1(SAMPLE_INPUT));
    }

    #[test]
    fn part2_sample_test() {
        assert_eq!(10, run2(SAMPLE_INPUT));
    }

    #[test]
    fn part1_test() {
        assert_eq!(5075, part1());
    }

    #[test]
    fn part2_test() {
        assert_eq!(7310, part2());
    }
}

pub fn part1() -> i32 {
    let input = include_str!("../inputs/day08.txt");

    return run1(input);
}

pub fn part2() -> i32 {
    let input = include_str!("../inputs/day08.txt");

    return run2(input);
}

fn run1(input:&str) -> i32 {
    let (max, _) = run(input);
    return max;
}

fn run2(input:&str) -> i32 {
    let (_, max) = run(input);
    return max;
}

fn run(input:&str) -> (i32, i32) {
    let lines:Vec<&str> = input.split("\n").collect();
    let mut registers:HashMap<String, i32> = HashMap::new();
    let mut maxvalever = 0;

    for line in lines {
        let args:Vec<&str> = line.split(" ").collect();
        let reg1 = *registers.entry(args[0].to_string()).or_insert(0);
        let reg2 = *registers.entry(args[4].to_string()).or_insert(0);
        if comparison(args[5], reg2, args[6].parse::<i32>().unwrap()) {
            let val = args[2].parse::<i32>().unwrap();
            let newval;
            if args[1] == "inc" {
                *registers.get_mut(args[0]).unwrap() += val;
                newval = reg1+val;
            } else {
                *registers.get_mut(args[0]).unwrap() -= val;
                newval = reg1-val;
            }
            if newval > maxvalever {
                maxvalever = newval;
            }
        }
    }

    let max = registers.values().max().unwrap();

    return (*max, maxvalever);
}

fn comparison(op: &str, reg: i32, val: i32) -> bool {
    match op {
        "==" => return reg == val,
        "!=" => return reg != val,
        "<=" => return reg <= val,
        ">=" => return reg >= val,
        "<" => return reg < val,
        ">" => return reg > val,
        _ => panic!("NO OP? {:?}", op)
    }
}