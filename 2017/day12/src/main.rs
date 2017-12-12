use std::collections::HashMap;
use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;

fn dfs_count(map: &HashMap<String, String>, vec: &mut Vec<String>, key: String) {
    let val:String;
    match map.get(&key) {
        Some(v) => val = v.clone(),
        None => return
    }
    let splitright:Vec<&str> = val.split(", ").collect();
    for pid in splitright {
        if !vec.contains(&pid.to_string()) {
            vec.push(pid.to_string());
            dfs_count(&map, vec, pid.to_string());
        }
    }
    return;
}

fn main() {
    let f = File::open("input.txt").expect("no file?");
    let reader = BufReader::new(f);

    let mut map:HashMap<String, String> = HashMap::new();

    for line in reader.lines() {
        let line = line.unwrap();

        let split:Vec<&str> = line.split(" <-> ").collect();

        map.insert(split[0].to_string(), split[1].to_string());
    }

    let mut network = Vec::new();
    dfs_count(&map, &mut network, "0".to_string());

    println!("Part 1: {:?}", network.len());

    let mut groupsum = 1;

    for i in 0..2000 {
        if !network.contains(&i.to_string()) {
            groupsum += 1;
            dfs_count(&map, &mut network, i.to_string());
        }
    }

    println!("Part 2: {:?}", groupsum);
}