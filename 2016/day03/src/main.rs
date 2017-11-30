use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;

fn main_part1() {
    
    let f = File::open("input.txt").expect("no file?");
    let mut reader = BufReader::new(f);

    let mut incorrect: i32 = 0;

    let mut v = std::vec::Vec::with_capacity(3);

    for line in reader.lines() {

        for num in line.unwrap().trim().split(" ") {
            if num == "" {
                continue;
            }
            println!("{}", num);
            let x: i32 = num.parse().expect("No INT! :(");
            v.push(x);

            // println!("Current: {}", num);
        }

        v.sort();

        if v[2] < (v[1] + v[0]) {
            incorrect += 1;
        }

        v.clear();

        println!("Incorrect: {}", incorrect);
    }
}

fn main() {
    
    let f = File::open("input.txt").expect("no file?");
    let reader = BufReader::new(f);

    let mut incorrect: i32 = 0;

    let mut v = std::vec::Vec::with_capacity(3);

    let mut v1 = std::vec::Vec::with_capacity(3);
    let mut v2 = std::vec::Vec::with_capacity(3);
    let mut v3 = std::vec::Vec::with_capacity(3);

    v.push(v1);
    v.push(v2);
    v.push(v3);

    let mut i: usize = 0;

    for line in reader.lines() {

        for num in line.unwrap().trim().split(" ") {
            if num == "" {
                continue;
            }
            println!("yay");
            let x: i32 = num.parse().expect("No INT! :(");

            (v[i%3]).push(x);

            i += 1;

            // println!("Current: {}", num);
        }

        if i == 9 {
            println!("9!");
            let vector = v.clone();

            for mut vec in vector {

                vec.sort();

                println!("num {}", vec[0]);
                println!("num {}", vec[1]);
                println!("num {}", vec[2]);

                if vec[2] < (vec[1] + vec[0]) {
                    incorrect += 1;
                }
            }
            i = 0;

            v.clear();

            let mut v1 = std::vec::Vec::with_capacity(3);
            let mut v2 = std::vec::Vec::with_capacity(3);
            let mut v3 = std::vec::Vec::with_capacity(3);

            v.push(v1);
            v.push(v2);
            v.push(v3);
            
            println!("Incorrect: {}", incorrect);
        }

    }

}
