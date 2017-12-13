use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;

fn calc_severity(lines: &Vec<String>, delay: i32) -> i32 {
    let mut severity = 0;
    for line in lines {
        let split:Vec<&str> = line.split(": ").collect();

        let index = split[0].parse::<i32>().unwrap() + delay;
        let range = split[1].parse::<i32>().unwrap();
        if index % ((range-1)*2) == 0 {
            severity += index*range;
        }
    }
    return severity;
}

fn main() {
    let f = File::open("input.txt").expect("no file?");
    let reader = BufReader::new(f);
    let lines:Vec<String> = reader.lines()
        .map(|l| l.expect("Could not parse line"))
        .collect();

    let mut severity = calc_severity(&lines, 0);
    let mut delay = 0;

    println!("Part 1: {:?}", severity);

    while severity > 0 {
        delay += 1;
        severity = calc_severity(&lines, delay);
    }

    println!("Part 2: {:?}", delay);
}