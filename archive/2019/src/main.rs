#[macro_use]
extern crate lazy_static;
mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod day08;
mod day09;
mod day10;
mod day11;
mod day12;
mod day13;
mod day14;
mod day15;
mod day16;
mod day17;
mod day18;
mod day19;
mod day20;
mod day21;
mod day22;
mod day23;
mod day24;
mod day25;
mod headings;
mod intcode;
mod util;

use std::env;
use std::time::Instant;

trait Day {
	fn new(input: &str) -> Self
	where
		Self: Sized;
	fn part1(&self) -> Box<dyn std::fmt::Display>;
	fn part2(&self) -> Box<dyn std::fmt::Display>;
}

fn main() {
	let args: Vec<String> = env::args().collect();

	if args.len() == 1 {
		for day in 1..26 {
			run(day, 0, false, true);
		}
		return;
	}

	if args.len() == 2 {
		if args[1] == "perf" {
			for day in 1..26 {
				run(day, 0, true, true);
			}
			return;
		} else if args[1] == "perfall" {
			perf_all_full();
			return;
		}

		let day = match args[1].parse() {
			Ok(x) => x,
			Err(_) => panic!("Please use `#day`, `perf #day` or `perfall`"),
		};
		run(day, 0, false, false);
		return;
	}

	if args.len() == 3 {
		if args[1] == "perf" {
			let day = args[2].parse().unwrap();
			run(day, 0, true, false);
			return;
		}
		let day = args[1].parse().unwrap();
		let part = args[2].parse().unwrap();

		run(day, part, false, false);
		return;
	}

	if args.len() == 4 {
		if args[1] != "perf" {
			panic!("Usage is wrong");
		}
		let day = args[2].parse().unwrap();
		let part = args[3].parse().unwrap();

		run(day, part, true, false);
		return;
	}

	panic!("Usage is wrong");
}

fn get_day(day: u32) -> Box<dyn Day> {
	// relative to working directory, NOT executable
	let inputfile = format!("inputs/day{:02}.txt", day);
	let content = std::fs::read_to_string(inputfile).unwrap();
	let input = content.as_str().trim();

	match day {
		01 => return Box::new(day01::Day01::new(input)),
		02 => return Box::new(day02::Day02::new(input)),
		03 => return Box::new(day03::Day03::new(input)),
		04 => return Box::new(day04::Day04::new(input)),
		05 => return Box::new(day05::Day05::new(input)),
		06 => return Box::new(day06::Day06::new(input)),
		07 => return Box::new(day07::Day07::new(input)),
		08 => return Box::new(day08::Day08::new(input)),
		09 => return Box::new(day09::Day09::new(input)),
		10 => return Box::new(day10::Day10::new(input)),
		11 => return Box::new(day11::Day11::new(input)),
		12 => return Box::new(day12::Day12::new(input)),
		13 => return Box::new(day13::Day13::new(input)),
		14 => return Box::new(day14::Day14::new(input)),
		15 => return Box::new(day15::Day15::new(input)),
		16 => return Box::new(day16::Day16::new(input)),
		17 => return Box::new(day17::Day17::new(input)),
		18 => return Box::new(day18::Day18::new(input)),
		19 => return Box::new(day19::Day19::new(input)),
		20 => return Box::new(day20::Day20::new(input)),
		21 => return Box::new(day21::Day21::new(input)),
		22 => return Box::new(day22::Day22::new(input)),
		23 => return Box::new(day23::Day23::new(input)),
		24 => return Box::new(day24::Day24::new(input)),
		25 => return Box::new(day25::Day25::new(input)),
		_ => panic!("Day {} not found", day),
	}
}

fn run(day: u32, part: u32, perf: bool, in_loop_of_multiple: bool) {
	let solution = get_day(day);

	if perf {
		perf_internal(solution, day, part, in_loop_of_multiple);
		return;
	}
	run_internal(solution, part);
}

fn run_internal(solution: Box<dyn Day>, part: u32) {
	match part {
		0 => {
			println!("{}", solution.part1());
			println!("{}", solution.part2());
		}
		1 => println!("{}", solution.part1()),
		2 => println!("{}", solution.part2()),
		_ => panic!("Unknown part"),
	}
}

fn perf_internal(solution: Box<dyn Day>, day: u32, part: u32, in_loop_of_multiple: bool) {
	let n = 100;

	if !in_loop_of_multiple || day == 1 || day == 13 {
		println!("|--------|------------|-----------|------------|");
		println!("| Puzzle |       Mean |     Error |     StdDev |");
	}
	println!("|--------|------------|-----------|------------|");

	match part {
		0 => {
			let rounds1 = (0..n).map(|_| perf_one(&solution, 1)).collect();
			print_perf(day, "p1", rounds1, n);
			let rounds2 = (0..n).map(|_| perf_one(&solution, 2)).collect();
			print_perf(day, "p2", rounds2, n);

			let rounds_full1 = (0..n).map(|_| perf_one_full(day, 1)).collect();
			print_perf(day, "f1", rounds_full1, n);

			let rounds_full2 = (0..n).map(|_| perf_one_full(day, 2)).collect();
			print_perf(day, "f2", rounds_full2, n);
		}
		x => {
			let rounds = (0..n).map(|_| perf_one(&solution, x)).collect();
			print_perf(day, &x.to_string(), rounds, n);
		}
	}
}

fn perf_one(solution: &Box<dyn Day>, part: u32) -> std::time::Duration {
	let start = Instant::now();
	match part {
		1 => {
			let _ = solution.part1();
		}
		2 => {
			let _ = solution.part2();
		}
		_ => panic!("Unknown part"),
	}
	return start.elapsed();
}

fn perf_one_full(day: u32, part: u32) -> std::time::Duration {
	let start = Instant::now();
	let day = get_day(day);
	match part {
		1 => {
			let _ = day.part1();
		}
		2 => {
			let _ = day.part2();
		}
		_ => panic!("Unknown part"),
	}
	return start.elapsed();
}

fn perf_all_full() {
	let n = 100;

	println!("|--------|------------|-----------|------------|");
	println!("| Puzzle |       Mean |     Error |     StdDev |");
	println!("|--------|------------|-----------|------------|");

	let rounds = (0..n)
		.map(|_| {
			let start = Instant::now();
			for d in 1..26 {
				let day = get_day(d);
				day.part1();
				day.part2();
			}
			return start.elapsed();
		})
		.collect();

	let mean = mean(&rounds);
	let stddev = stddeviation(mean, &rounds);
	let error = stddev / (n as f64).sqrt();

	println!(
		"|    All | {:>7.0} ms | {:>6.2} ms | {:>7.2} ms |",
		mean, error, stddev
	);
}

fn print_perf(day: u32, part: &str, rounds: Vec<std::time::Duration>, n: u32) {
	let mean = mean(&rounds);
	let stddev = stddeviation(mean, &rounds);
	let error = stddev / (n as f64).sqrt();

	println!(
		"| D{:02} {:} | {:>7.2} ms | {:>6.4} ms | {:>7.4} ms |",
		day, part, mean, error, stddev
	);
}

fn mean(list: &Vec<std::time::Duration>) -> f64 {
	let sum: u128 = Iterator::sum(list.iter().map(|d| d.as_micros()));
	return f64::from(sum as u32 / 1000) / (list.len() as f64);
}

fn stddeviation(mean: f64, list: &Vec<std::time::Duration>) -> f64 {
	return list
		.iter()
		.map(|v| f64::from(v.as_micros() as u32 / 1000))
		.map(|v| (v - mean) * (v - mean))
		.sum::<f64>()
		/ (list.len() - 1) as f64;
}
