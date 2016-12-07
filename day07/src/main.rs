#[macro_use] extern crate lazy_static;
extern crate regex;

use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;

use std::vec::Vec;
use regex::Regex;

fn contains_abba(text: &str) -> bool {

    for i in 0..(text.len()-3) {

        let slice = text.to_string().into_bytes();
        if  slice[i+0] == slice[i+3] &&
            slice[i+1] == slice[i+2] &&
            slice[i+0] != slice[i+1] {
                return true;
            }
    }
    false
}

fn line_contains_abba(line: String) -> bool {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"\[([a-z]+)\]").unwrap();
    }

    let linestring = line.clone();
    let supernet_seqences: Vec<&str> = RE.split(&linestring).collect();

    let mut abba_in_supernet: bool = false;
    for sequence in supernet_seqences {
        if contains_abba(sequence) {
            abba_in_supernet = true;
            break;
        }
    }

    for caps in RE.captures_iter(&linestring) {
        if contains_abba(caps.at(1).unwrap()) {
            return false;
        }
    }

    abba_in_supernet
}

fn main() {
    let f = File::open("input.txt").expect("no file?");
    let reader = BufReader::new(f);

    let mut counter = 0;
    for line in reader.lines() {

        let linestring = line.unwrap();
        if line_contains_abba(linestring.clone()) {
            counter += 1;
            println!("True: {}", linestring);            
        } else {
 
            println!("False: {}", linestring); 
        }
    }

    println!("Counter: {}", counter);
}
    // let re = Regex::new(r"([a-z]+)(?:\[([a-z]+)\]([a-z]+))*").unwrap();