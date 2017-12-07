use std::collections::HashMap;
use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;

struct Node {
    name: String,
    weight: i32,
    children: Vec<String>,
}

fn dfs(universe: &HashMap<String,Node>, n: &Node) -> i32 {
    let mut nodes:Vec<&Node> = Vec::new();
    let mut weights:Vec<i32> = Vec::new();
    let mut total: i32 = n.weight;
    for child in &n.children {
        let node = universe.get(child).unwrap();
        let w = dfs(universe, node);
        weights.push(w);
        nodes.push(&node);
        total += w;
    }
    if weights.len() > 0 {
        let a = weights[0];
        for i in 1..weights.len() {
            if weights[i] != a {
                print!("{:?} ({:?}) Total: {:?} ", nodes[i].name, nodes[i].weight, weights[i]);
                println!("differs from {:?} ({:?}) Total: {:?}", nodes[0].name, nodes[0].weight, weights[0]);
            }
        }
    }
    return total;
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

    println!("rootnode: {:?}", &rootnode);

    dfs(&allnodes, allnodes.get(&rootnode).unwrap());
}