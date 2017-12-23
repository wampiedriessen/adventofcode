use day18;
use std::collections::HashMap;

pub fn part1() -> i32 {
	let input = include_str!("../inputs/day23.txt");

	run1(input)
}

pub fn part2() -> i64 {
	let input = include_str!("../inputs/day23.txt");

	run2(input)
}

fn run1(input:&str) -> i32{
	let mut regs:HashMap<char, i64> = HashMap::new();
	let mut ops:Vec<&str> = input.split("\n").collect();
	ops.push("end");
	let mut index:i64 = 0;
	let mut multiplied = 0;
	loop {
		let op = ops[index as usize];
		let args:Vec<&str> = op.split_whitespace().collect();
		match args[0] {
			"mul" => {
				multiplied += 1;
				day18::compute(&mut regs, args);
			},
			"jnz" => {
				if day18::get_int(&mut regs, args[1]) != 0 {
					index += day18::get_int(&mut regs, args[2]);
					continue;
				}
			},
			"end" => break,
			_ => day18::compute(&mut regs, args)
		}
		index += 1;
	}
	return multiplied;
}

fn run2(input:&str) -> i64 {
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

fn run2_little_optimized(input:&str) -> i64 {
	let b = 108400;
	let c = 125400;
	let mut h = 0;
	let mut ophogen;
	for _ in 0..(c-b)/17 {
		ophogen = false;
		for d in 2..b {
			for e in d..b {
				if d*e == b {
					ophogen = true;
				}
			}
		}
		if ophogen {
			h += 1;
		}
	}	

	h
}

pub fn run2_unoptimized() -> i64 {
	let mut a:i64 = 1;
	let mut b:i64 = 0;
	let mut c:i64 = 0;
	let mut d:i64 = 0;
	let mut e:i64 = 0;
	let mut f:i64 = 0;
	let mut g:i64 = 0;
	let mut h:i64 = 0;

	b = 84;
	c = 84;
	if a == 1 {
		b = 108400;
		c = 125400;
	}

	while {
		f = 1;
		d = 2;
		while {
			e = 2;

			while {
				g = d*e-b;

				if g == 0 {
					f = 0;
				}

				e += 1;
				g = e - b;
				g != 0
			} {}

			d += 1; // d = 1;
			g = d - b;
			g != 0
		} {}

		if f == 0 {
			h += 1;
		}

		g = b-c;

		b += 17;
		g != 0
	} {}

	h
}