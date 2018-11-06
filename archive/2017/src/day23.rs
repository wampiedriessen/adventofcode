use day18;
use std::collections::HashMap;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_sample_test() {
        // nothing provided
    }

    #[test]
    fn part2_sample_test() {
        // nothing provided
    }

    #[test]
    fn part1_test() {
        assert_eq!(6724, part1());
    }

    #[test]
    fn part2_test() {
        assert_eq!(903, part2());
    }
}


pub fn part1() -> i32 {
	let input = include_str!("../inputs/day23.txt");

	run1(input)
}

pub fn part2() -> i64 {
	run2()
}

fn run1(input:&str) -> i32{
	let mut regs:HashMap<char, i64> = HashMap::new();
	let ops:Vec<&str> = input.split("\n").collect();
	let mut index:usize = 0;
	let mut multiplied = 0;
	while index < ops.len() {
		let op = ops[index as usize];
		let args:Vec<&str> = op.split_whitespace().collect();
		match args[0] {
			"mul" => {
				multiplied += 1;
				day18::compute(&mut regs, args);
			},
			"jnz" => {
				if day18::get_int(&mut regs, args[1]) != 0 {
					let jump = day18::get_int(&mut regs, args[2]);
					index = ((index as i64) + jump) as usize;
					continue;
				}
			},
			_ => day18::compute(&mut regs, args)
		}
		index += 1;
	}
	return multiplied;
}

fn run2() -> i64 {
	let mut b = 108400;
	let c = 125400;
	let mut h = 0;
	while b != c {
		if !is_prime(b) {
			h += 1;
		}
		b += 17;
	}
	if !is_prime(b) {
		h += 1;
	}

	h
}

fn is_prime(n: i64) -> bool {
    if n == 2 || n == 3 {
        return true;
    } else if n % 2 == 0 || n % 3 == 0 {
        return false;
    }

    let mut i = 5i64;
    let mut w = 2i64;
    while i*i <= n {
        if n % i == 0 {
            return false;
        }
        i += w;
        w = 6 - w;
    }
    true
}