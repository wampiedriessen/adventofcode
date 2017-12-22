use std::collections::HashMap;

pub fn part1() -> i32{
	let input = include_str!("../inputs/day21.txt");

	let lut = parse_input(input);

	println!("{:?}", lut.keys().len());

	return run1(&lut);
}

pub fn part2() -> i32{
	let input = include_str!("../inputs/day21.txt");

	let lut = parse_input(input);

	return run2(&lut);
}

fn run1(lut:&HashMap<String,String>) -> i32 {
	let mut grid = ".#./..#/###".to_string();

	for _ in 0..5 {
		grid = grow(grid);
	}

	return grid.len() as i32;
}

fn run2(lut:&HashMap<String,String>) -> i32 {
	0
}

fn grow(grid:String) -> String {
	"".to_string()
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