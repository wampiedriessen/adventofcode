use std::collections::HashMap;
use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;

#[derive(Debug,Clone)]
struct Node {
    name: String,
    weight: i32,
    total_weight: i32,
    children: Vec<String>,
}

fn fill_total_weight(mut universe: &mut HashMap<String,Node>, name: String) -> i32 {
    let mut node:Node = universe.get_mut(&name).unwrap().clone();
    node.total_weight = node.weight;
    for childname in &node.children {
        node.total_weight += fill_total_weight(&mut universe, childname.clone());
    }
    universe.insert(node.name.clone(), node.clone());
    return node.total_weight.clone();
}

fn fix_wrong_weight(universe: &HashMap<String,Node>, name: String, weight: i32, diff: i32) {
    let node:&Node = universe.get(&name).unwrap();
    for childname in &node.children {
        let child:&Node = universe.get(childname).unwrap();
        if child.total_weight == weight {
            println!("Part 2: {:?}", child.weight - diff);
            return;
        }
    }
}

fn find_inbalance(universe: &HashMap<String,Node>, name: String) -> bool {
    let node:&Node = universe.get(&name).unwrap();
    let mut weights:Vec<i32> = Vec::new();
    for childname in &node.children {
        let child:&Node = universe.get(childname).unwrap();
        if find_inbalance(universe,childname.clone()) {
            return true;
        }
        weights.push(child.total_weight);
    }

    weights.sort();

    if weights.len() > 0 && weights[0] != weights[weights.len()-1] {
        let diff = weights[weights.len()-1] - weights[0];
        let wrongweight;
        if weights[0] == weights[1] {
            wrongweight = weights[weights.len()-1];
        } else {
            wrongweight = weights[0];
        }
        fix_wrong_weight(universe, name, wrongweight, diff);
        return true;
    }
    return false;
}

fn main() {
    let f = File::open("input.txt").expect("no file?");
    let reader = BufReader::new(f);
    let mut nodesleft:Vec<String> = Vec::new();
    let mut nodesright:Vec<String> = Vec::new();

    let mut allnodes:HashMap<String,Node> = HashMap::new();

    for line in reader.lines() {
        let line:String = line.unwrap().replace("(", "").replace(")", "");
        let mut children:Vec<String> = Vec::new();
        if line.contains("->") {
            let twoparts:Vec<&str> = line.split(" -> ").collect();
            let temparr: Vec<&str> = twoparts[1].split(", ").collect();
            for rightnode in temparr {
                children.push(rightnode.to_string());
                nodesright.push(rightnode.to_string())
            }
        }
        let arguments: Vec<&str> = line.split(" ").collect();
        allnodes.insert(arguments[0].to_string(), Node {
            name: arguments[0].to_string(),
            weight: arguments[1].parse::<i32>().unwrap(),
            total_weight: 0,
            children: children
        });
        nodesleft.push(arguments[0].to_string());
    }

    let mut rootnode:String = String::new();

    for node in &nodesleft {
        if !nodesright.contains(&node) {
            rootnode = node.clone();
            break;
        }
    }

    println!("Part 1: {:?}", &rootnode);

    fill_total_weight(&mut allnodes, rootnode.clone());

    find_inbalance(&allnodes, rootnode);
}