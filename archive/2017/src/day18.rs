use std::collections::HashMap;

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn part1_sample_test() {
		let ops = "set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2";
	assert_eq!(4, run1(ops.to_string()));
  }

  #[test]
  fn part2_sample_test() {
	let ops = "snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d";
	assert_eq!(3, run2(ops.to_string()));
  }

  #[test]
  fn part1_test() {
	assert_eq!(2951, part1());
  }

  #[test]
  fn part2_test() {
	assert_eq!(7366, part2());
  }
}

pub fn part1() -> i64 {
	let input = include_str!("../inputs/day18.txt");

	return run1(input.to_string());
}

pub fn part2() -> i64 {
	let input = include_str!("../inputs/day18.txt");

	return run2(input.to_string());
}

fn run1(input:String) -> i64 {
	let mut regs:HashMap<char, i64> = HashMap::new();
	let ops:Vec<&str> = input.split("\n").collect();
	let mut index:i64 = 0;
	let mut freqplayed = -1;
	loop {
		let op = ops[index as usize];
		let args:Vec<&str> = op.split_whitespace().collect();
		match args[0] {
			"snd" => {
				freqplayed = get_int(&mut regs, args[1]);
			},
			"rcv" => {
				if get_int(&mut regs, args[1]) > 0 {
					break;
				}
			},
			"jgz" => {
				if get_int(&mut regs, args[1]) > 0 {
					index += get_int(&mut regs, args[2]);
					continue;
				}
			},
			_ => compute(&mut regs, args)
		}
		index += 1;
	}
	return freqplayed;
}

fn run2(input:String) -> i64 {
	let mut regs:[HashMap<char, i64>; 2] = [HashMap::new(), HashMap::new()];
	let ops:Vec<&str> = input.split("\n").collect();
	let mut waiting:[bool; 2] = [false, false];
	let mut queue:[Vec<i64>; 2] = [Vec::new(), Vec::new()];
	let mut indices:[i64; 2] = [0, 0];
	regs[0].insert('p', 0);
	regs[1].insert('p', 1);
	let mut p = 0;
	let mut p1counter = 0;
	loop {
		if waiting[p] {
			if waiting[(p+1) % 2] { break; }
			if queue[p].len() == 0 {
				p = (p+1) % 2; // next prog
			}
		}
		let op = ops[indices[p] as usize];
		let args:Vec<&str> = op.split_whitespace().collect();
		match args[0] {
			"snd" => {
				let val = get_int(&mut regs[p], args[1]);
				queue[(p+1)%2].push(val);

				if p == 1 { p1counter += 1; }
			},
			"rcv" => {
				if queue[p].len() > 0 {
					let reg = regs[p].entry(args[1].chars().next().unwrap()).or_insert(0);
					*reg = queue[p].remove(0);
					waiting[p] = false;
				} else {
					waiting[p] = true;
					continue; // dont increase prog_counter
				}
			},
			"jgz" => {
				if get_int(&mut regs[p], args[1]) > 0 {
					indices[p] += get_int(&mut regs[p], args[2]);
					continue;
				}
			},
			_ => compute(&mut regs[p], args)
		}
		indices[p] += 1;
		p = (p+1) % 2;
	}
	return p1counter;
}

pub fn compute(regs:&mut HashMap<char, i64>, args:Vec<&str>) {
	let x = args[1].chars().next().unwrap();
	let y = get_int(regs, args[2]);
	match args[0] {
		"set" => {
			regs.insert(x, y);
		},
		"add" => {
			let v = regs.entry(x).or_insert(0);
			*v += y;
		},
		"sub" => {
			let v = regs.entry(x).or_insert(0);
			*v -= y;
		},
		"mul" => {
			let v = regs.entry(x).or_insert(0);
			*v *= y;
		},
		"mod" => {
			let v = regs.entry(x).or_insert(0);
			*v = *v % y;
		},
		_ => panic!("no such op"),
	}
}

pub fn get_int(regs:&mut HashMap<char, i64>, arg:&str) -> i64 {
	match arg.parse::<i64>() {
		Ok(val) => return val,
		Err(_) => return *regs.entry(arg.chars().next().unwrap()).or_insert(0)
	}
}