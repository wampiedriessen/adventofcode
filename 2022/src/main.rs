mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;

use std::io::BufRead;
use crate::day01::Day01;
use crate::day02::Day02;
use crate::day03::Day03;
use crate::day04::Day04;
use crate::day05::Day05;
use crate::day06::Day06;
use crate::day07::Day07;

pub trait Day {
    fn part1(&self) -> String;
    fn part2(&self) -> String;
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    // Prints each argument on a separate line
    let day: Box<dyn Day> = match args[1].as_str() {
        "1" | "01" => Box::new(Day01 { input: all_input() }),
        "2" | "02" => Box::new(Day02 { input: all_input() }),
        "3" | "03" => Box::new(Day03 { input: all_input() }),
        "4" | "04" => Box::new(Day04 { input: all_input() }),
        "5" | "05" => Box::new(Day05 { input: all_input() }),
        "6" | "06" => Box::new(Day06 { input: all_input() }),
        "7" | "07" => Box::new(Day07 { input: all_input() }),
        // "8" | "08" => Box::new(Day08 { input: all_input() }),
        // "9" | "09" => Box::new(Day09 { input: all_input() }),
        // "10" => Box::new(Day10 { input: all_input() }),
        // "11" => Box::new(Day11 { input: all_input() }),
        // "12" => Box::new(Day12 { input: all_input() }),
        // "13" => Box::new(Day13 { input: all_input() }),
        // "14" => Box::new(Day14 { input: all_input() }),
        // "15" => Box::new(Day15 { input: all_input() }),
        // "16" => Box::new(Day16 { input: all_input() }),
        // "17" => Box::new(Day17 { input: all_input() }),
        // "18" => Box::new(Day18 { input: all_input() }),
        // "19" => Box::new(Day19 { input: all_input() }),
        // "20" => Box::new(Day20 { input: all_input() }),
        // "21" => Box::new(Day21 { input: all_input() }),
        // "22" => Box::new(Day22 { input: all_input() }),
        // "23" => Box::new(Day23 { input: all_input() }),
        // "24" => Box::new(Day24 { input: all_input() }),
        // "25" => Box::new(Day25 { input: all_input() }),
        _ => panic!("Unkown day!"),
    };

    println!("Performing part 1:");
    println!("{}", day.part1());
    println!("Performing part 2:");
    println!("{}", day.part2());
}

fn all_input() -> Vec<String> {
    let mut input = Vec::new();

    let stdin = std::io::stdin();
    for line in stdin.lock().lines() {
        input.push(line.unwrap());
    }

    input
}
