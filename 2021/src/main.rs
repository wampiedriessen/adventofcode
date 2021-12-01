mod day01;
// mod day02;
// mod day03;
// mod day04;
// mod day05;
// mod day06;
// mod day07;
// mod day08;
// mod day09;
// mod day10;
// mod day11;
// mod day12;
// mod day13;
// mod day14;
// mod day15;
// mod day16;
// mod day17;
// mod day18;
// mod day19;
// mod day20;
// mod day21;
// mod day22;
// mod day23;
// mod day24;
// mod day25;

use crate::day01::Day01;
// use crate::day02::Day02;
// use crate::day03::Day03;
// use crate::day04::Day04;
// use crate::day05::Day05;
// use crate::day06::Day06;
// use crate::day07::Day07;
// use crate::day08::Day08;
// use crate::day09::Day09;
// use crate::day10::Day10;
// use crate::day11::Day11;
// use crate::day12::Day12;
// use crate::day13::Day13;
// use crate::day14::Day14;
// use crate::day15::Day15;
// use crate::day16::Day16;
// use crate::day17::Day17;
// use crate::day18::Day18;
// use crate::day19::Day19;
// use crate::day20::Day20;
// use crate::day21::Day21;
// use crate::day22::Day22;
// use crate::day23::Day23;
// use crate::day24::Day24;
// use crate::day25::Day25;

use std::io::BufRead;

pub trait Day {
    fn part1(&self) -> String;
    fn part2(&self) -> String;
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    // Prints each argument on a separate line
    let day = match args[1].as_str() {
        "1" | "01" => Day01 { input: all_input() },
        // "2" | "02" => Day02 { input },
        // "3" | "03" => Day03 { input },
        // "4" | "04" => Day04 { input },
        // "5" | "05" => Day05 { input },
        // "6" | "06" => Day06 { input },
        // "7" | "07" => Day07 { input },
        // "8" | "08" => Day08 { input },
        // "9" | "09" => Day09 { input },
        // "10" => Day10 { input },
        // "11" => Day11 { input },
        // "12" => Day12 { input },
        // "13" => Day13 { input },
        // "14" => Day14 { input },
        // "15" => Day15 { input },
        // "16" => Day16 { input },
        // "17" => Day17 { input },
        // "18" => Day18 { input },
        // "19" => Day19 { input },
        // "20" => Day20 { input },
        // "21" => Day21 { input },
        // "22" => Day22 { input },
        // "23" => Day23 { input },
        // "24" => Day24 { input },
        // "25" => Day25 { input },
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
