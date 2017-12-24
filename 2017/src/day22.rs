
#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn part1_sample_test() {
		let example_input = "..#
#..
...".to_string();

		assert_eq!(5587, run1(example_input));
	}

	#[test]
	fn part2_sample_test() {
		let example_input = "..#
#..
...".to_string();

		assert_eq!(2511944, run2(example_input));
	}

	#[test]
	fn part1_test() {
		assert_eq!(5261, part1());
	}

	#[test]
	fn part2_test() {
		assert_eq!(2511927, part2());
	}
}
pub fn part1() -> u32 {
	let input = include_str!("../inputs/day22.txt");

	return run1(input.to_string());
}

pub fn part2() -> usize {
	let input = include_str!("../inputs/day22.txt");

	return run2(input.to_string());
}

enum Dir {
	Up,
	Dn,
	Lf,
	Rg
}

#[derive(PartialEq, Clone)]
enum State {
	Clean,
	Weakened,
	Infected,
	Flagged
}

fn turn_left(d:&Dir) -> Dir {
	match *d {
		Dir::Up => Dir::Lf,
		Dir::Lf => Dir::Dn,
		Dir::Dn => Dir::Rg,
		Dir::Rg => Dir::Up,
	}
}

fn turn_right(d:&Dir) -> Dir {
	match *d {
		Dir::Up => Dir::Rg,
		Dir::Rg => Dir::Dn,
		Dir::Dn => Dir::Lf,
		Dir::Lf => Dir::Up,
	}
}

fn reverse_dir(d:&Dir) -> Dir {
	match *d {
		Dir::Up => Dir::Dn,
		Dir::Dn => Dir::Up,
		Dir::Rg => Dir::Lf,
		Dir::Lf => Dir::Rg,
	}
}

fn run1(input:String) -> u32 {
	let mut grid:Vec<Vec<State>> = parse_input(input);

	let mut x = grid[0].len()/2;
	let mut y = grid.len()/2;

	let mut sum = 0;
	let mut dir:Dir = Dir::Up;

	for _ in 0..10000 {
		if step1(&mut grid, &mut x, &mut y, &mut dir) {
			sum += 1;
		}
	}

	return sum;
}

fn run2(input:String) -> usize {
	let mut grid:Vec<Vec<State>> = parse_input(input);

	let mut x = grid[0].len()/2;
	let mut y = grid.len()/2;

	let mut sum = 0;
	let mut dir:Dir = Dir::Up;

	for _ in 0..10000000 {
		if step2(&mut grid, &mut x, &mut y, &mut dir) {
			sum += 1;
		}
	}

	return sum;
}

fn step1(mut grid:&mut Vec<Vec<State>>, x:&mut usize, y:&mut usize, dir:&mut Dir) -> bool {
	// step one
	if grid[*y][*x] == State::Infected {
		*dir = turn_right(dir);
		grid[*y][*x] = State::Clean;
	} else {
		*dir = turn_left(dir);
		grid[*y][*x] = State::Infected;
	}

	let out = grid[*y][*x] == State::Infected;
	// step two

	let mut newx = *x as i32;
	let mut newy = *y as i32;

	// step three
	match *dir {
	    Dir::Up => newy -= 1,
	    Dir::Dn => newy += 1,
	    Dir::Lf => newx -= 1,
	    Dir::Rg => newx += 1,
	}


	grow_grid(&mut grid, &mut newx, &mut newy);

	*x = newx as usize;
	*y = newy as usize;

	return out;
}


fn step2(mut grid:&mut Vec<Vec<State>>, x:&mut usize, y:&mut usize, dir:&mut Dir) -> bool {
	// step one
	match grid[*y][*x] {
		State::Clean => {
			*dir = turn_left(dir);
			grid[*y][*x] = State::Weakened;
		},
		State::Weakened => {
			grid[*y][*x] = State::Infected;
		},
		State::Infected => {
			*dir = turn_right(dir);
			grid[*y][*x] = State::Flagged;
		},
		State::Flagged => {
			*dir = reverse_dir(dir);
			grid[*y][*x] = State::Clean;
		},
	}

	let out = grid[*y][*x] == State::Infected;
	// step two

	let mut newx = *x as i32;
	let mut newy = *y as i32;

	// step three
	match *dir {
	    Dir::Up => newy -= 1,
	    Dir::Dn => newy += 1,
	    Dir::Lf => newx -= 1,
	    Dir::Rg => newx += 1,
	}


	grow_grid(&mut grid, &mut newx, &mut newy);

	*x = newx as usize;
	*y = newy as usize;

	return out;
}


fn parse_input(input:String) -> Vec<Vec<State>> {
	let mut grid:Vec<Vec<State>> = Vec::new();

	let lines:Vec<&str> = input.split_whitespace().collect();

	for line in lines {
		grid.push(parse_line(line.to_string()).clone());
	}

	return grid;
}

fn parse_line(line:String) -> Vec<State> {
	let kars:Vec<char> = line.trim().chars().collect();
	let mut row:Vec<State> = Vec::new();
	for kar in kars
	{
		if kar == '#' {
			row.push(State::Infected);
		} else {
			row.push(State::Clean);
		}
	}
	return row;
}

fn grow_grid(mut grid:&mut Vec<Vec<State>>, x:&mut i32, y:&mut i32) {
	if *x == -1 {
		grow_grid_left(&mut grid);
		*x = 0;
	}
	if *x == grid[0].len() as i32 {
		grow_grid_right(&mut grid);
	}
	if *y == -1 {
		grow_grid_top(&mut grid);
		*y = 0;
	}
	if *y == grid.len() as i32 {
		grow_grid_bot(&mut grid);
	}
}

fn grow_grid_bot(grid:&mut Vec<Vec<State>>) {
	let len = grid[0].len();

	grid.push(vec![State::Clean; len]);
}

fn grow_grid_top(grid:&mut Vec<Vec<State>>) {
	let len = grid[0].len();

	grid.insert(0, vec![State::Clean; len]);
}

fn grow_grid_left(grid:&mut Vec<Vec<State>>) {
	let len = grid.len();

	for i in 0..len {
		grid[i].insert(0, State::Clean);
	}
}

fn grow_grid_right(grid:&mut Vec<Vec<State>>) {
	let len = grid.len();

	for i in 0..len {
		grid[i].push(State::Clean);
	}
}