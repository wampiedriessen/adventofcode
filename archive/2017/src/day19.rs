#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn part1_sample_test() {
    let sample_input:String = "     |          
     |  +--+    
     A  |  C    
 F---|--|-E---+ 
     |  |  |  D 
     +B-+  +--+ 
".to_string();
    assert_eq!("ABCDEF", run1(sample_input.to_string()));
  }

  #[test]
  fn part2_sample_test() {
        let sample_input:String = "     |          
     |  +--+    
     A  |  C    
 F---|--|-E---+ 
     |  |  |  D 
     +B-+  +--+ 
".to_string();
    assert_eq!(38, run2(sample_input.to_string()));
  }

  #[test]
  fn part1_test() {
    assert_eq!("BPDKCZWHGT", part1());
  }

  #[test]
  fn part2_test() {
    assert_eq!(17728, part2());
  }
}

pub fn part1() -> String {
	let input = include_str!("../inputs/day19.txt");

	return run1(input.to_string());
}

pub fn part2() -> u32 {
	let input = include_str!("../inputs/day19.txt");

	return run2(input.to_string());
}

#[derive(PartialEq)]
enum Dir {
	Up,
	Dn,
	Lf,
	Rg
}

struct MazeRunner {
	grid:Vec<Vec<char>>,
	steps:u32,
	letters:String
}

impl MazeRunner {
	pub fn run(&mut self) {
		let mut x:usize = 0;
		let mut y:usize = 0;

		for i in 0..self.grid[0].len() {
			if self.grid[0][i] == '|' {
				x = i;
			}
		}
		let mut dir = Dir::Dn;

		loop {
			match &self.grid[y][x] {
			    &'+' => dir = self.find_new_dir(x, y, dir),
			    &'|' => dir = dir, // nop
			    &' ' => break,
			    &'-' => dir = dir, // nop
			    _ => self.letters += &self.grid[y][x].to_string(),
			}
			match dir {
			    Dir::Up => y -= 1,
			    Dir::Dn => y += 1,
			    Dir::Lf => x -= 1,
			    Dir::Rg => x += 1
			}
			self.steps += 1;
		}
	}

	fn find_new_dir(&self, x:usize, y:usize, dir:Dir) -> Dir {
		if dir == Dir::Up || dir == Dir::Dn {
			if self.grid[y][x-1] != ' ' {
				return Dir::Lf;
			}
			if self.grid[y][x+1] != ' ' {
				return Dir::Rg;
			}
		}

		if dir == Dir::Lf || dir == Dir::Rg {
			if self.grid[y-1][x] != ' ' {
				return Dir::Up;
			}
			if self.grid[y+1][x] != ' ' {
				return Dir::Dn;
			}
		}
		panic!("Uhhhhhhhhh");
	}
}

fn grid_from_input(input:String) -> Vec<Vec<char>> {
	let mut grid = Vec::new();

	let rows:Vec<&str> = input.split("\n").collect();
	for row in rows {
		grid.push(row.chars().collect());
	}

	return grid;
}

fn run1(input:String) -> String {
	let mut runner = MazeRunner {
		grid: grid_from_input(input),
		letters: String::new(),
		steps: 0
	};

	runner.run();
	return runner.letters;
}

fn run2(input:String) -> u32 {
	let mut runner = MazeRunner {
		grid: grid_from_input(input),
		letters: String::new(),
		steps: 0
	};

	runner.run();
	return runner.steps;
}