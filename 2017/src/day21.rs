use std::collections::HashMap;

pub fn part1() -> i32 {
let input = include_str!("../inputs/day21.txt");

let lut = parse_input(input);

println!("{:?}", lut.keys().len());

return run1(&lut);
}

pub fn part2() -> i32 {
let input = include_str!("../inputs/day21.txt");

let lut = parse_input(input);

return run2(&lut);
}

fn run1(lut:&HashMap<String,String>) -> i32 {
	let mut grid = ".#./..#/###".to_string();

	for _ in 0..5 {
		grid = grow(&lut, grid);
	}

	return count_pixels(grid);
}

fn run2(lut:&HashMap<String,String>) -> i32 {
	let mut grid = ".#./..#/###".to_string();

	for _ in 0..18 {
		grid = grow(&lut, grid);
	}

	return count_pixels(grid);
}

fn count_pixels(grid:String) -> i32 {
	let mut count = 0;
	for kar in grid.chars() {
		if kar == '#' {
			count += 1;
		}
	}
	count
}

fn grow(lut:&HashMap<String,String>, grid:String) -> String {
	if grid.len() <= 11 {
		return lut.get(&grid).unwrap().clone();
	}

	let dots = grid.replace('/',"");

	if dots.len() % 2 == 0 {
		return split_grow_2(&lut, grid);
	}

	if dots.len() % 3 == 0 {
		return split_grow_3(&lut, grid);
	}

	"".to_string()
}

fn split_grow_2(lut:&HashMap<String,String>, grid:String) -> String {
	let lines:Vec<&str> = grid.split("/").collect();
	let len = lines.len();

	let mut subgrids:Vec<String> = Vec::new();

	for i in step_range(0, len, 2) {
		for j in step_range(0, len, 2) {
			let mut subgrid:String = lines[i][j..j+2].to_string();
			subgrid.push('/');
			subgrid.push_str(&lines[i+1][j..j+2]);
			subgrids.push(grow(&lut, subgrid));
		}
	}

	// subgrids have grown

	let mut newgrid:String = String::new();

	for i in 0..len/2 {
		let mut lines:[String; 3] = ["".to_string(),"".to_string(),"".to_string()];
		for j in 0..len/2 {
			let grlines:Vec<&str> = subgrids[i*len/2 + j].split("/").collect();
			lines[0].push_str(grlines[0].clone());
			lines[1].push_str(grlines[1].clone());
			lines[2].push_str(grlines[2].clone());
		}
		for line in lines.iter() {
			newgrid += &line.clone();
			newgrid.push('/');
		}
	}

	newgrid.pop();
	newgrid
}

fn split_grow_3(lut:&HashMap<String,String>, grid:String) -> String {
	let lines:Vec<&str> = grid.split("/").collect();
	let len = lines.len();

	let mut subgrids:Vec<String> = Vec::new();

	for i in step_range(0, len, 3) {
		for j in step_range(0, len, 3) {
			let mut subgrid:String = lines[i][j..j+3].to_string();
			subgrid.push('/');
			subgrid.push_str(&lines[i+1][j..j+3]);
			subgrid.push('/');
			subgrid.push_str(&lines[i+2][j..j+3]);
			subgrids.push(grow(&lut, subgrid));
		}
	}

	// subgrids have grown

	let mut newgrid:String = String::new();

	for i in 0..len/3 {
		let mut lines:[String; 4] = ["".to_string(),"".to_string(),"".to_string(),"".to_string()];
		for j in 0..len/3 {
			let grlines:Vec<&str> = subgrids[i*len/3 + j].split("/").collect();
			lines[0].push_str(grlines[0].clone());
			lines[1].push_str(grlines[1].clone());
			lines[2].push_str(grlines[2].clone());
			lines[3].push_str(grlines[3].clone());
		}
		for line in lines.iter() {
			newgrid += &line.clone();
			newgrid.push('/');
		}
	}

	newgrid.pop();
	newgrid
}

fn parse_input(input:&str) -> HashMap<String,String> {
	let mut lut:HashMap<String,String> = HashMap::new();

	let lines:Vec<&str> = input.split("\n").collect();

	for line in lines {
		let split:Vec<&str> = line.split(" => ").collect();

		for possibility in possibilities(split[0]) {
			lut.insert(possibility.clone(), split[1].to_string());
		}
	}

	lut
}

fn possibilities(input:&str) -> Vec<String> {
	let mut inp:String = input.clone().to_string();
	let mut out = Vec::new();
	if input.len() == 5 {
		for _ in 0..3 {
			out.push(inp.clone());

			let kars:Vec<u8> = inp.clone().chars().filter(|&x| x != '/').map(|x| x as u8).collect();
			// inp geroteerd
			inp = String::from_utf8(vec![kars[1], kars[3], kars[0], kars[2]]).unwrap();
			let mut x:Vec<char> = inp.clone().chars().collect();

			x.swap(0,1);
			x.swap(2,3);
			
			x.insert(2, '/');
			let tmp:String = x.iter().collect();
			out.push(tmp.clone());
			x = inp.clone().chars().collect();

			x.swap(0,2);
			x.swap(1,3);
			
			x.insert(2, '/');
			let tmp:String = x.iter().collect();
			out.push(tmp.clone());

			inp.insert(2, '/');
		}
		out.push(inp.clone());
	}
	if input.len() == 11 {
		for _ in 0..3 {
			out.push(inp.clone());

			let kars:Vec<u8> = inp.clone().chars().filter(|&x| x != '/').map(|x| x as u8).collect();
			// inp geroteerd
			inp = String::from_utf8(vec![kars[2], kars[5], kars[8], kars[1], kars[4], kars[7], kars[0], kars[3], kars[6]]).unwrap();
			let mut x:Vec<char> = inp.clone().chars().collect();

			x.swap(0,2);
			x.swap(3,5);
			x.swap(6,8);
			
			x.insert(3, '/');
			x.insert(7, '/');
			let tmp:String = x.iter().collect();
			out.push(tmp.clone());
			x = inp.clone().chars().collect();

			x.swap(0,6);
			x.swap(1,7);
			x.swap(2,8);
			
			x.insert(3, '/');
			x.insert(7, '/');
			let tmp:String = x.iter().collect();
			out.push(tmp.clone());

			inp.insert(3, '/');
			inp.insert(7, '/');
		}
		out.push(inp.clone());
	}
	out
}

fn step_range(start:usize, end:usize, stepsize:usize) -> Vec<usize> {
	let mut out:Vec<usize> = Vec::new();
	let mut i = 0;
	while i*stepsize < end - start {
		out.push(start+i*stepsize);

		i += 1;
	}
	out
}