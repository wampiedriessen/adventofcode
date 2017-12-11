use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;

fn divisive_numbers(v: &mut std::vec::Vec<i32>) -> i32 {
    for i in 0..v.len()-1 {
        for j in i+1..v.len()-1 {
            if v[i] % v[j] == 0 {
                return v[i] / v[j] as i32;
            }
            if v[j] % v[i] == 0 {
                return v[j] / v[i] as i32;
            }
        }
    }
    return 0;
}

fn main() {
    let f = File::open("input.txt").expect("no file?");
    let reader = BufReader::new(f);

    let mut sum: i32 = 0;
    let mut sum2: i32 = 0;

    let mut v = std::vec::Vec::with_capacity(15);

    for line in reader.lines() {
        for num in line.unwrap().trim().split("\t") {
            if num == "" {
                continue;
            }
            let x: i32 = num.parse().expect("No INT! :(");
            v.push(x);
        }

        v.sort();

        sum2 += divisive_numbers(&mut v);
        sum += v[v.len()-1] - v[0];
        v.clear();
    }


    println!("Part 1: {}", sum);
    println!("Part 2: {}", sum2);
}
