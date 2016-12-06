use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;

use std::vec::Vec;
use std::collections::HashMap;

fn main() {
    let f = File::open("input.txt").expect("no file?");
    let reader = BufReader::new(f);

    let mut position: Vec<HashMap<char, i32>> = Vec::with_capacity(8);
    
    for _ in 0..9 {
        position.push(HashMap::new());
    }

    for line in reader.lines() {
        let linestring = line.unwrap().clone();

        let mut i = 0;
        for c in linestring.chars() {
            let counter = position[i].entry(c).or_insert(0);
            *counter += 1;
            i += 1;
        }

    }

    let mut vmax: Vec<char> = Vec::with_capacity(8);
    let mut vmin: Vec<char> = Vec::with_capacity(8);

    let mut max: i32 = 0;
    let mut min: i32 = 0;
    let mut maxchar: char;
    let mut minchar: char;
    for i in 0..9 {
        max = 0;
        min = 999999;
        maxchar = '\t';
        minchar = '\t';
        for (ch, count) in &position[i] {
            if max < count.clone() {
                maxchar = ch.clone();
                max = count.clone();
            }
            if min > count.clone() {
                minchar = ch.clone();
                min = count.clone();
            }
        }
        vmax.push(maxchar);
        vmin.push(minchar);
    }

    let smax: String = vmax.into_iter().collect();
    let smin: String = vmin.into_iter().collect();

    println!("max: {}", smax);
    println!("max: {}", smin);

}