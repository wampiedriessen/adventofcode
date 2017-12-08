use std::collections::HashMap;
use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;

fn comparison(op: &str, reg: i32, val: i32) -> bool {
    match op {
        "==" => return reg == val,
        "!=" => return reg != val,
        "<=" => return reg <= val,
        ">=" => return reg >= val,
        "<" => return reg < val,
        ">" => return reg > val,
        _ => panic!("NO OP? {:?}", op)
    }
}

fn main() {
    let f = File::open("input.txt").expect("no file?");
    let reader = BufReader::new(f);
    let mut registers:HashMap<String, i32> = HashMap::new();

    let mut maxvalever = 0;

    for line in reader.lines() {
        let line = line.unwrap();
        let args:Vec<&str> = line.split(" ").collect();
        let reg1 = *registers.entry(args[0].to_string()).or_insert(0);
        let reg2 = *registers.entry(args[4].to_string()).or_insert(0);
        if comparison(args[5], reg2, args[6].parse::<i32>().unwrap()) {
            let val = args[2].parse::<i32>().unwrap();
            let newval;
            if args[1] == "inc" {
                *registers.get_mut(args[0]).unwrap() += val;
                newval = reg1+val;
            } else {
                *registers.get_mut(args[0]).unwrap() -= val;
                newval = reg1-val;
            }
            if newval > maxvalever {
                maxvalever = newval;
            }
        }
    }

    let mut max:i32 = 0;
    let mut maxname = String::new();

    for name in registers.keys() {
        let val = *registers.get(name).unwrap();
        if val > max {
            max = val;
            maxname = name.clone();
        }
    }

    println!("Max: {:?} in {:?}", max, maxname);
    println!("Max ever: {:?}", maxvalever);
}   