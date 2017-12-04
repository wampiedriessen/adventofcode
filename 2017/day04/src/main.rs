use std::collections::HashMap;
use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;

fn is_anagram(a:&str, b:&str) -> bool {

    if a.len() != b.len() {
        return false;
    }

    let mut d: HashMap<char, i32> = HashMap::new();
    for c in a.trim().chars() {
        d.entry(c).or_insert(0);
        *d.get_mut(&c).unwrap() += 1;
    }
    for c in b.trim().chars() {
        if !d.contains_key(&c) {
            return false;
        }
        *d.get_mut(&c).unwrap() -= 1;
        if *d.get(&c).unwrap() == -1 {
            return false;
        }
    }
    return true;
}

fn contains_anagram(v: &Vec<String>, s:&str) -> bool {
    for word in v {
        if is_anagram(word, s) {
            println!("ANAGRAMS: {} - {}", word, s);
            return true;
        }
    }
    return false;
}

fn main() {
    let f = File::open("input.txt").expect("no file?");
    let reader = BufReader::new(f);

    let mut valid1 = 0;
    let mut valid2 = 0;

    for line in reader.lines() {
        let mut v: Vec<String> = Vec::new();
        let mut correct1:bool = true;
        let mut correct2:bool = true;
        for word in line.unwrap().trim().split(" ") {
            if v.contains(&word.to_string()) { correct1 = false; }
            if contains_anagram(&v, &word.to_string()) { correct2 = false; }
            v.push(word.to_string());
        }
        if correct1 {
            valid1 += 1;
        }
        if correct2 {
            valid2 += 1;
        }
    }

    println!("1: {:?}", valid1);
    println!("2: {:?}", valid2);
}