use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;

fn calc_score(line: String) {
    let mut depth:u64 = 0;
    let mut score:u64 = 0;
    let mut in_garbage:bool = false;
    let mut skip:bool = false;
    let mut garbage_count:u64 = 0;

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

    println!("Score: {:?}", score);
    println!("Garbage count: {:?}", garbage_count);
}

fn main() {
    let f = File::open("input.txt").expect("no file?");
    let reader = BufReader::new(f);

    let line = reader.lines().next().unwrap().unwrap();

    calc_score(line);
}