use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;
use std::vec::Vec;
use std::collections::HashMap;

fn is_real_room(s: String) -> i32 {
    let split: Vec<&str> = s.split("[").collect();

    let code = split[0];
    let checksum = split[1];
    let mut codesplit: Vec<&str> = code.split("-").collect();

    let num: i32 = codesplit.pop().expect("no split?").parse().expect("nan?");
    // println!("Room no: {}", num);

    let mut characters: HashMap<char, i32> = HashMap::new();

    for c in codesplit.join("").chars() {
        let counter = characters.entry(c).or_insert(0);
        *counter += 1;
    }

    // let mut checklist: Vec<(i32, char)> = Vec::new();

    // read checksum
    let mut biggest = 0;
    let mut lastcount = 9999999;
    let mut lastchar = '!';
    for x in checksum.chars() {
        if x == ']' {
            break;
        }
        if !characters.contains_key(&x) {
            return 0;
        }
        // println!("last: {}, x: {}", lastchar, x);
        if lastcount < characters[&x] {
            return 0;
        } else if lastcount == characters[&x] && lastchar > x {
            return 0;
        }
        lastcount = characters[&x];
        if biggest == 0 {
            biggest = lastcount;
        }
        lastchar = x;
        // checklist.push((characters[&x], x.clone()));
    }

    for (_, count) in characters {
        if count > biggest {
            return 0;
        }
    }

    num
}

fn print_char_spec(mut p: u8, mut x: i32) {
    x = x % 26;
    if (p as char) == '-' {
        print!(" ");
        return;
    }
    while x > 0 {
        if (p as char) == 'z' || (p as char) == 'Z' {
            p -= 26;
        }
        p += 1;
        x -= 1;
    }
    p += (x % 26) as u8;
    print!("{}", p as char);
}

fn main() {
    
    let f = File::open("input.txt").expect("no file?");
    let reader = BufReader::new(f);

    let mut sum: i32 = 0;

    for line in reader.lines() {
        let linestring = line.unwrap().clone();
        let x = is_real_room(linestring.trim().to_string());
        sum += x;

        if x != 0 {

            let bytes = linestring.into_bytes();

            for byte in bytes {
                print_char_spec(byte.clone(), x);
            }
            print!(" \t num: {}\n", x);
        }
    }

    println!("Sum: {}", sum);
}