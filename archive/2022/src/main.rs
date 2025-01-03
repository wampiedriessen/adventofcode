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

use std::io::{BufRead, Read};
use crate::day01::Day01;
use crate::day02::Day02;
use crate::day03::Day03;
use crate::day04::Day04;
use crate::day05::Day05;
use crate::day06::Day06;
use crate::day07::Day07;
use crate::day08::Day08;
use crate::day09::Day09;
use crate::day10::Day10;
use crate::day11::Day11;
use crate::day12::Day12;
use crate::day13::Day13;
use crate::day14::Day14;
use crate::day15::Day15;

pub trait Day {
    fn part1(&self) -> String;
    fn part2(&self) -> String;
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    // Prints each argument on a separate line
    let day: Box<dyn Day> = match args[1].as_str() {
        "1" | "01" => Box::new(Day01 { input: all_lines() }),
        "2" | "02" => Box::new(Day02 { input: all_lines() }),
        "3" | "03" => Box::new(Day03 { input: all_lines() }),
        "4" | "04" => Box::new(Day04 { input: all_lines() }),
        "5" | "05" => Box::new(Day05 { input: all_lines() }),
        "6" | "06" => Box::new(Day06 { input: all_lines() }),
        "7" | "07" => Box::new(Day07 { input: all_lines() }),
        "8" | "08" => Box::new(Day08 { input: all_lines() }),
        "9" | "09" => Box::new(Day09 { input: all_lines() }),
        "10" => Box::new(Day10 { input: all_lines() }),
        "11" => Box::new(Day11 { input: full_input() }),
        "12" => Box::new(Day12 { input: all_lines() }),
        "13" => Box::new(Day13 { input: all_lines() }),
        "14" => Box::new(Day14 { input: all_lines() }),
        "15" => Box::new(Day15 { input: all_lines() }),
        // "16" => Box::new(Day16 { input: all_lines() }),
        // "17" => Box::new(Day17 { input: all_lines() }),
        // "18" => Box::new(Day18 { input: all_lines() }),
        // "19" => Box::new(Day19 { input: all_lines() }),
        // "20" => Box::new(Day20 { input: all_lines() }),
        // "21" => Box::new(Day21 { input: all_lines() }),
        // "22" => Box::new(Day22 { input: all_lines() }),
        // "23" => Box::new(Day23 { input: all_lines() }),
        // "24" => Box::new(Day24 { input: all_lines() }),
        // "25" => Box::new(Day25 { input: all_lines() }),
        _ => panic!("Unkown day!"),
    };

    println!("Performing part 1:");
    println!("{}", day.part1());
    println!("Performing part 2:");
    println!("{}", day.part2());
}

fn all_lines() -> Vec<String> {
    let mut input = Vec::new();

    let stdin = std::io::stdin();
    for line in stdin.lock().lines() {
        input.push(line.unwrap());
    }

    input
}

fn full_input() -> String {
    let mut input = Vec::new();
    std::io::stdin().read_to_end(&mut input).unwrap();
    String::from_utf8(input).unwrap()
}
