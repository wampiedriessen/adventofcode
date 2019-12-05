// #[cfg(test)]
// #[macro_use]
// extern crate indoc;

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

mod intcode;

use std::time::Instant;
use std::env;

fn main() {
	let args: Vec<String> = env::args().collect();

	if args.len() == 1 {
		for day in 1..26 {
			run(day, 1);
			run(day, 2);
		}
		return;
	}

	if args.len() == 2 {
		if args[1] == "perf" {
			for day in 1..26 {
				perf(day, 1);
				perf(day, 2);
			}
			return;
		}
		let day = args[1].parse().unwrap();
		run(day, 1);
		run(day, 2);
		return;
	}

	if args.len() == 3 {
		if args[1] == "perf" {
			let day = args[2].parse().unwrap();
			perf(day, 1);
			perf(day, 2);
			return;
		}
		let day = args[1].parse().unwrap();
		let part = args[2].parse().unwrap();

		run(day, part);
		return;
	}

	if args.len() == 4 {
		if args[1] != "perf" {
			panic!("Usage is wrong");
		}
		let day = args[2].parse().unwrap();
		let part = args[3].parse().unwrap();

		perf(day, part);
		return;
	}

	panic!("Usage is wrong");
}

fn run(day: u32, part: u32) {
	println!("{}", run_internal(day, part));
}

fn run_internal(day:u32, part:u32) -> String {
	match (day, part) {
		(01, 1) => return format!("{}", day01::part1()),
		(01, 2) => return format!("{}", day01::part2()),
		(02, 1) => return format!("{}", day02::part1()),
		(02, 2) => return format!("{}", day02::part2()),
		(03, 1) => return format!("{}", day03::part1()),
		(03, 2) => return format!("{}", day03::part2()),
		(04, 1) => return format!("{}", day04::part1()),
		(04, 2) => return format!("{}", day04::part2()),
		(05, 1) => return format!("{}", day05::part1()),
		(05, 2) => return format!("{}", day05::part2()),
		(06, 1) => return format!("{}", day06::part1()),
		(06, 2) => return format!("{}", day06::part2()),
		(07, 1) => return format!("{}", day07::part1()),
		(07, 2) => return format!("{}", day07::part2()),
		(08, 1) => return format!("{}", day08::part1()),
		(08, 2) => return format!("{}", day08::part2()),
		(09, 1) => return format!("{}", day09::part1()),
		(09, 2) => return format!("{}", day09::part2()),
		(10, 1) => return format!("{}", day10::part1()),
		(10, 2) => return format!("{}", day10::part2()),
		(11, 1) => return format!("{}", day11::part1()),
		(11, 2) => return format!("{}", day11::part2()),
		(12, 1) => return format!("{}", day12::part1()),
		(12, 2) => return format!("{}", day12::part2()),
		(13, 1) => return format!("{}", day13::part1()),
		(13, 2) => return format!("{}", day13::part2()),
		(14, 1) => return format!("{}", day14::part1()),
		(14, 2) => return format!("{}", day14::part2()),
		(15, 1) => return format!("{}", day15::part1()),
		(15, 2) => return format!("{}", day15::part2()),
		(16, 1) => return format!("{}", day16::part1()),
		(16, 2) => return format!("{}", day16::part2()),
		(17, 1) => return format!("{}", day17::part1()),
		(17, 2) => return format!("{}", day17::part2()),
		(18, 1) => return format!("{}", day18::part1()),
		(18, 2) => return format!("{}", day18::part2()),
		(19, 1) => return format!("{}", day19::part1()),
		(19, 2) => return format!("{}", day19::part2()),
		(20, 1) => return format!("{}", day20::part1()),
		(20, 2) => return format!("{}", day20::part2()),
		(21, 1) => return format!("{}", day21::part1()),
		(21, 2) => return format!("{}", day21::part2()),
		(22, 1) => return format!("{}", day22::part1()),
		(22, 2) => return format!("{}", day22::part2()),
		(23, 1) => return format!("{}", day23::part1()),
		(23, 2) => return format!("{}", day23::part2()),
		(24, 1) => return format!("{}", day24::part1()),
		(24, 2) => return format!("{}", day24::part2()),
		(25, 1) => return format!("{}", day25::part1()),
		(25, 2) => return format!("{}", day25::part2()),

		_ => return format!("Day {} part {} not found", day, part),
	}
}

fn perf(day: u32, part: u32) {
	let n = 20;
	println!("Day {} part {}. Running {} times.", day, part, n);

	let mut rounds: Vec<std::time::Duration> = Vec::new();
	for _ in 0..n {
		let start = Instant::now();
		run_internal(day, part);
		rounds.push(start.elapsed());
	}

	let mean = mean(&rounds);
	let stddev = stddeviation(mean, &rounds);
	let error = stddev / (n as f64).sqrt();

	println!("|  Puzzle  |     Mean |     Error |    StdDev |");
	println!("|----------|----------|-----------|-----------|");
	println!("|  D{:02} p{}  | {:>5.0} ms | {:.4} ms | {:.4} ms |", day, part, mean, error, stddev);
}

fn mean(list: &Vec<std::time::Duration>) -> f64 {
    let sum: u128 = Iterator::sum(list.iter().map(|d| d.as_millis()));
    f64::from(sum as u32) / (list.len() as f64)
}

fn stddeviation(mean: f64, list: &Vec<std::time::Duration>) -> f64 {
	return list.iter()
		.map(|v| f64::from(v.as_millis()as u32))
		.map(|v| (v - mean)*(v - mean))
		.sum::<f64>() / (list.len() - 1) as f64;
}