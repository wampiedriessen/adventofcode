use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;

fn calc_score(line: String) {
    let mut depth:u32 = 0;
    let mut score:u32 = 0;
    let mut in_garbage:bool = false;
    let mut skip:bool = false;
    let mut garbage_count:u32 = 0;

    for chr in line.chars() {
        if skip {
            skip = false;
            continue;
        }
        if chr == '!' {
            if in_garbage {
                skip = true;
                continue;
            }
        }
        if in_garbage {
            if chr == '>' {
                in_garbage = false;
                continue;
            }
            garbage_count += 1;
            continue;
        }
        if chr == '{' {
            depth += 1;
        }
        if chr == '}' {
            if !in_garbage {
                score += depth;
                depth -= 1;
            }
        }
        if chr == '<' {
            in_garbage = true;
        }
    }

    println!("Part 1: {:?}", score);
    println!("Part 2: {:?}", garbage_count);
}

fn main() {
    let f = File::open("input.txt").expect("no file?");
    let mut reader = BufReader::new(f);

    let mut line:String = String::new();
    reader.read_line(&mut line).expect("failed to read line");

    calc_score(line);
}