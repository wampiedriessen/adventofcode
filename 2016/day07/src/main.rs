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

fn line_has_tls(line: String) -> bool {
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

fn contains_any_babs(text: &str, vec: &Vec<String>) -> bool {
    for s in vec {
        if text.contains(s) {
            return true;
        }
    }
    false
}

fn find_optional_babs(text: &str, vec: &mut Vec<String>) -> bool {

    for i in 0..(text.len()-2) {

        let slice = text.to_string().into_bytes();
        if  slice[i+0] == slice[i+2] &&
            slice[i+0] != slice[i+1] {
            let mut bab: String = "".to_string();
            bab.push(slice[i+1] as char);
            bab.push(slice[i+0] as char);
            bab.push(slice[i+1] as char);
            vec.push(bab);
        }
    }
    false
}

fn line_has_ssl(line: String) -> bool {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"\[([a-z]+)\]").unwrap();
    }

    let linestring = line.clone();
    let supernet_seqences: Vec<&str> = RE.split(&linestring).collect();
    let mut babs_to_find: Vec<String> = Vec::new();

    for sequence in supernet_seqences {
        find_optional_babs(sequence, &mut babs_to_find);
    }

    let mut correct_bab_in_hypernet = false;
    for caps in RE.captures_iter(&linestring) {
        if contains_any_babs(caps.at(1).unwrap(), &babs_to_find) {
            correct_bab_in_hypernet = true;
        }
    }

    correct_bab_in_hypernet
}

fn main() {
    let f = File::open("input.txt").expect("no file?");
    let reader = BufReader::new(f);

    let mut SSLcounter = 0;
    let mut TLScounter = 0;
    for line in reader.lines() {

        let linestring = line.unwrap();
        if line_has_tls(linestring.clone()) {
            TLScounter += 1;
        }
        if line_has_ssl(linestring.clone()){
            SSLcounter += 1;
        }
    }

    println!("TLSCounter: {}", TLScounter);
    println!("SSLcounter: {}", SSLcounter);
}
