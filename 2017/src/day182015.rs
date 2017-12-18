pub fn part1() -> i32 {
	let input = include_str!("../inputs/day18.txt");
	let grid = parse_input(input.to_string(), false);
	return run1(grid, false);
}

pub fn part2() -> i32 {
	let input = include_str!("../inputs/day18.txt");
	let grid = parse_input(input.to_string(), true);
	return run1(grid, true);
}

fn run1(input:Vec<Vec<bool>>, alwayson:bool) -> i32 {
	let mut grids = [input, vec![vec![false; 100]; 100]];
	let mut lightson = 0;
	for run in 0..100 {
		lightson = 0;
		let i = run % 2;
		let i2 = (run + 1) % 2;
		for x in 0..100 {
			for y in 0..100 {
				if alwayson {
					grids[i2][x][y] = set_light2(&grids[i], x, y);
				} else {
					grids[i2][x][y] = set_light(&grids[i], x, y);
				}
				if grids[i2][x][y] {
					lightson += 1;
				}
			}
		}
	}

	return lightson;
}

fn set_light2(grid:&Vec<Vec<bool>>, x:usize, y:usize) -> bool {
	match (x, y) {
		(0, 0) => return true,
		(0, 99) => return true,
		(99, 0) => return true,
		(99, 99) => return true,
		_ => return set_light(&grid, x, y)
	}
}

fn set_light(grid:&Vec<Vec<bool>>, x:usize, y:usize) -> bool {
	let neigh = count_neighbours_on(&grid, x, y);
	if grid[x][y] {
		if neigh == 2 || neigh == 3 {
			return true;
		}
	} else {
		if neigh == 3 {
			return true
		}
	}
	return false;
}

fn count_neighbours_on(grid:&Vec<Vec<bool>>, x:usize, y:usize) -> i32 {
	let mut count = 0;

	if x != 0 {
	    if grid[x-1][y] { count += 1; }
	}
	if y != 0 {
	    if grid[x][y-1] { count += 1; }
	}
	if x != 0 && y != 0 {
	    if grid[x-1][y-1] { count += 1; }
	}
	if x != 99 {
	    if grid[x+1][y] { count += 1; }
	}
	if y != 99 {
	    if grid[x][y+1] { count += 1; }
	}
	if x != 99 && y != 99 {
	    if grid[x+1][y+1] { count += 1; }
	}
	if x != 99 && y != 0 {
	    if grid[x+1][y-1] { count += 1; }
	}
	if x != 0 && y != 99 {
	    if grid[x-1][y+1] { count += 1; }
	}

	return count;
}

fn parse_input(input:String, alwayson:bool) -> Vec<Vec<bool>> {
	let lines:Vec<&str> = input.split("\n").collect();

	let mut grid:Vec<Vec<bool>> = Vec::with_capacity(100);
	let mut x = 0;
	for line in lines {
		let mut y = 0;
		let mut row:Vec<bool> = Vec::with_capacity(100);
		let chars:Vec<char> = line.chars().collect();
		for c in chars {
			if alwayson {
				match (x, y) {
					(0, 0) => row.push(true),
					(0, 99) => row.push(true),
					(99, 0) => row.push(true),
					(99, 99) => row.push(true),
					_ => row.push(c == '#')
				}
			} else {
				row.push(c == '#');
			}
			y += 1;
		}
		grid.push(row);
		x += 1;
	}
	return grid;
}