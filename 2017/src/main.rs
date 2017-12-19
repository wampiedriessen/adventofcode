// #[cfg(test)]
// #[macro_use]
// extern crate indoc;

mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
// mod day06;
// mod day07;
// mod day08;
// mod day09;
// mod day10;
// mod day11;
// mod day12;
// mod day13;
mod day14;
// mod day15;
mod day16;
mod day17;
mod day18;
mod day182015;
mod day19;

use std::env;
use std::process::exit;

fn main() {
	let args: Vec<String> = env::args().collect();

	if args.len() == 3 {
		let day = args[1].parse::<u32>().expect("day is invalid");
		let part = args[2].parse::<u32>().expect("part is invalid");
		
		run(day,part);
		return;
	}

	if args.len() == 2 {
		let day = args[1].parse::<u32>().expect("day is invalid");
		for i in 1..3 {
			run(day,i);
		}
		return;
	}

	if args.len() == 1 {
		for i in 1..26 {
			for j in 1..3 {
				run(i,j);
			}
		}
		return;
	}

	println!("Usage: {} day part", &args[0]);
	exit(1);
}

fn run(day:u32, part:u32) {
	match (day, part) {
		(01, 1) => println!("{}", day01::part1()),
		(01, 2) => println!("{}", day01::part2()),
		(02, 1) => println!("{}", day02::part1()),
		(02, 2) => println!("{}", day02::part2()),
		(03, 1) => println!("{}", day03::part1()),
		(03, 2) => println!("{}", day03::part2()),
		(04, 1) => println!("{}", day04::part1()),
		(04, 2) => println!("{}", day04::part2()),
		(05, 1) => println!("{}", day05::part1()),
		(05, 2) => println!("{}", day05::part2()),
		// (06, 1) => println!("{}", day06::part1()),
		// (06, 2) => println!("{}", day06::part2()),
		// (07, 1) => println!("{}", day07::part1()),
		// (07, 2) => println!("{}", day07::part2()),
		// (08, 1) => println!("{}", day08::part1()),
		// (08, 2) => println!("{}", day08::part2()),
		// (09, 1) => println!("{}", day09::part1()),
		// (09, 2) => println!("{}", day09::part2()),
		// (10, 1) => println!("{}", day10::part1()),
		// (10, 2) => println!("{}", day10::part2()),
		// (11, 1) => println!("{}", day11::part1()),
		// (11, 2) => println!("{}", day11::part2()),
		// (12, 1) => println!("{}", day12::part1()),
		// (12, 2) => println!("{}", day12::part2()),
		// (13, 1) => println!("{}", day13::part1()),
		// (13, 2) => println!("{}", day13::part2()),
		(14, 1) => println!("{}", day14::part1()),
		(14, 2) => println!("{}", day14::part2()),
		// (15, 1) => println!("{}", day15::part1()),
		// (15, 2) => println!("{}", day15::part2()),
		(16, 1) => println!("{}", day16::part1()),
		(16, 2) => println!("{}", day16::part2()),
		(17, 1) => println!("{}", day17::part1()),
		(17, 2) => println!("{}", day17::part2()),
		(18, 1) => println!("{}", day18::part1()),
		(18, 2) => println!("{}", day18::part2()),
		(19, 1) => println!("{}", day19::part1()),
		(19, 2) => println!("{}", day19::part2()),


		(201518, 1) => println!("{}", day182015::part1()),
		(201518, 2) => println!("{}", day182015::part2()),
		_ => println!("Day {} part {} not found", day, part),
	}
}