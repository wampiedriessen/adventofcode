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

pub fn part1() -> String {
	let input = include_str!("../inputs/day19.txt");

	return run1(input.to_string());
}

pub fn part2() -> String {
	let input = include_str!("../inputs/day19.txt");

	return run2(input.to_string());
}

fn run1(input:String) -> String {
	let mut chargrid:Vec<Vec<char>> = Vec::new();

	let rows:Vec<&str> = input.split("\n").collect();
	for row in rows {
		chargrid.push(row.chars().collect());
	}

	let mut x:usize = 0;
	let mut y:usize = 0;

	for i in 0..chargrid[0].len() {
		if chargrid[0][i] == '|' {
			x = i;
		}
	}

	let mut letters:String = String::new();
	let mut dir = "s";
	let xbound = chargrid[0].len();
	let ybound = chargrid.len();

	let mut hadspace:bool = false;
	let mut steps:u32 = 0;

	loop {
		if x >= xbound || x < 0 ||
			y >= ybound || y < 0 { break; }
		match &chargrid[y][x] {
		    &'+' => dir = &find_new_dir(&chargrid, x, y, dir),
		    &'|' => dir = dir,
		    &' ' => { if !hadspace { dir = dir; hadspace = true; } else { steps -= 1; break; } },
		    &'-' => dir = dir,
		    _ => letters += &chargrid[y][x].to_string(),
		}
		if &chargrid[y][x] != &' ' {
			hadspace = false;
		}
		match dir {
		    "n" => y -= 1,
		    "s" => y += 1,
		    "w" => x -= 1,
		    "e" => x += 1,
		    _ => panic!("Unknown direction given"),
		}
		steps += 1;
	}

	println!("Steps: {:?}", steps);
	
	return letters;
}

fn find_new_dir(chargrid:&Vec<Vec<char>>, x:usize, y:usize, dir:&str) -> &'static str {
	let xbound = chargrid[0].len();
	let ybound = chargrid.len();
	let dirs:Vec<i32> = vec![-1,1];
	if chargrid[y][x] != '+' {
		panic!("no crossroads!");
	}

	for a in dirs.clone() {
		let x:i32 = x as i32 + a;
		if x >= xbound as i32 || x < 0 { break; }
		if (chargrid[y][x as usize] == '-' ||
			chargrid[y][x as usize] == '+') {
				if (a == -1 && dir != "e") {
					return "w";
				} else if (a == 1 && dir != "w") {
					return "e";
				}
			}
	}
	for b in dirs {
		let y:i32 = y as i32 + b;
		if y >= ybound as i32 || y < 0 { break; }
		if (chargrid[y as usize][x] == '|' ||
			chargrid[y as usize][x] == '+') {
				if (b == -1 && dir != "s") {
					return "n";
				} else if (b == 1) {
					return "s";
				}
			}
	}
	println!("not ok at: x: {:?}, y: {:?}", x, y);
	return "ok";
}

fn run2(input:String) -> String {
	return String::new();
}