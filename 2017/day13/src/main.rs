use std::collections::HashMap;
use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;

fn calc_severity(scanners: &HashMap<i32, i32>, delay: i32) -> i32 {
    let mut severity = 0;
    for index in scanners.keys() {
        let range = scanners.get(index).unwrap();

        if (delay+index) % ((range-1)*2) == 0 {
            severity += (index+delay)*range;
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

    let mut scanners:HashMap<i32, i32> = HashMap::new();
    for l in lines {
        let split:Vec<&str> = l.split(": ").collect();

        let index = split[0].parse::<i32>().unwrap();
        let range = split[1].parse::<i32>().unwrap();

        scanners.insert(index, range);
    }

    let mut severity = calc_severity(&scanners, 0);
    let mut delay = 0;

    println!("Part 1: {:?}", severity);

    while severity > 0 {
        delay += 1;
        severity = calc_severity(&scanners, delay);
    }

    println!("Part 2: {:?}", delay);
}