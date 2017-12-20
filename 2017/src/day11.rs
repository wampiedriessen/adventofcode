use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;

fn main() {
    let f = File::open("input.txt").expect("no file?");
    let mut reader = BufReader::new(f);

    let mut line = String::new();
    reader.read_line(&mut line).expect("Could not read line");

    let mut nw: i32 = 0;
    let mut n: i32 = 0;
    let mut ne: i32 = 0;
    let mut max: i32 = 0;

    for dir in line.trim().split(",") {
        match dir {
            "nw" => nw += 1,
            "n" => n += 1,
            "ne" => ne += 1,
            "sw" => ne -= 1,
            "s" => n -= 1,
            "se" => nw -= 1,
            _ => panic!("gekke input")
        }
        while nw > 0 && ne > 0 {
            nw -= 1;
            ne -= 1;
            n += 1;
        }
        while (n > 0 && ne < 0) || (n > 0 && nw < 0) {
            n -= 1;
            ne += 1;
            nw += 1;
        }
        while nw < 0 && ne < 0 {
            nw += 1;
            ne += 1;
            n -= 1;
        }
        while (n < 0 && ne > 0) || (n < 0 && nw > 0) {
            n += 1;
            ne -= 1;
            nw -= 1;
        }

        if n+ne+nw > max {
            max = n+ne+nw;
        }
    }

    println!("Part 1: {:?}", n+ne+nw);
    println!("Part 2: {:?}", max);
}