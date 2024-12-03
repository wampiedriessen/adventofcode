use std::io;
use std::io::Read;
use regex::Regex;
use lazy_static::lazy_static;

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    part1(&input);
    part2(&input);

    Ok(())
}

lazy_static! {
    static ref day3_re: Regex = Regex::new(r"(mul\((?<a>[0-9]+),(?<b>[0-9]+)\)|do\(\)|don't\(\))").unwrap();
}

fn part1(input: &str) {

    let num_matched: i32 = day3_re.captures_iter(&input)
        .map(|capt|
            if (&capt[0]).starts_with("mul") {
                (capt["a"].parse::<i32>().unwrap(), capt["b"].parse::<i32>().unwrap())
            } else { (0, 0) }
        )
        .map(|(a,b)| a*b)
        .sum();

    println!("Part 1: {}", num_matched);
}

fn part2(input: &str) {
    let mut sum: i32 = 0;
    let mut recording = true;

    for cap in day3_re.captures_iter(&input) {
        if (&cap[0]).starts_with("don") {
            recording = false;
        } else if (&cap[0]).starts_with("do") {
            recording = true;
        } else if recording {
            sum += cap["a"].parse::<i32>().unwrap() * cap["b"].parse::<i32>().unwrap();
        }
    }

    println!("Part 2: {}", sum);
}