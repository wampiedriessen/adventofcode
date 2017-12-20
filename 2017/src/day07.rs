use std::collections::HashMap;

#[cfg(test)]
mod tests {
    use super::*;

    const SAMPLE_INPUT:&str = "pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)";

    #[test]
    fn part1_sample_test() {
        assert_eq!("tknk", run1(&parse_input(SAMPLE_INPUT)));
    }

    #[test]
    fn part2_sample_test() {
        assert_eq!(60, run2(parse_input(SAMPLE_INPUT)));
    }

    #[test]
    fn part1_test() {
        assert_eq!("qibuqqg", part1());
    }

    #[test]
    fn part2_test() {
        assert_eq!(1079, part2());
    }
}

pub fn part1() -> String {
    let input = include_str!("../inputs/day07.txt");

    return run1(&parse_input(input));
}

pub fn part2() -> i32 {
    let input = include_str!("../inputs/day07.txt");

    return run2(parse_input(input));
}

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

fn fix_wrong_weight(universe: &HashMap<String,Node>, name: String, weight: i32, diff: i32) -> i32 {
    let node:&Node = universe.get(&name).unwrap();
    for childname in &node.children {
        let child:&Node = universe.get(childname).unwrap();
        if child.total_weight == weight {
            return child.weight - diff;
        }
    }
    return -1;
}

fn find_inbalance(universe: &HashMap<String,Node>, name: String) -> (bool,i32) {
    let node:&Node = universe.get(&name).unwrap();
    let mut weights:Vec<i32> = Vec::new();
    for childname in &node.children {
        let child:&Node = universe.get(childname).unwrap();
        let (inb,weight) = find_inbalance(universe,childname.clone());
        if inb {
            return (inb, weight);
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
        return (true, fix_wrong_weight(universe, name, wrongweight, diff));
    }
    return (false, -1);
}

fn run1(allnodes:&HashMap<String,Node>) -> String {
    let mut nodesleft:Vec<String> = Vec::with_capacity(allnodes.keys().len());
    let mut nodesright:Vec<String> = Vec::with_capacity(allnodes.keys().len());

    let mut rootnode:String = String::new();

    for key in allnodes.keys() {
        nodesleft.push(key.clone());
        let node = allnodes.get(key).unwrap();
        for child in &node.children {
            nodesright.push(child.clone());
        }
    }

    for node in &nodesleft {
        if !nodesright.contains(&node) {
            rootnode = node.clone();
            break;
        }
    }

    return rootnode;
}

fn run2(mut allnodes:HashMap<String,Node>) -> i32 {
    let rootnode = run1(&allnodes);

    fill_total_weight(&mut allnodes, rootnode.clone());
    let (_, weight) = find_inbalance(&allnodes, rootnode);

    return weight;
}

fn parse_input(input:&str) -> HashMap<String,Node> {
    let mut allnodes:HashMap<String,Node> = HashMap::new();

    let lines:Vec<&str> = input.split("\n").collect();

    for line in lines {
        let line:String = line.replace("(", "").replace(")", "");
        let mut children:Vec<String> = Vec::new();
        if line.contains("->") {
            let twoparts:Vec<&str> = line.split(" -> ").collect();
            let temparr: Vec<&str> = twoparts[1].split(", ").collect();
            for rightnode in temparr {
                children.push(rightnode.to_string());
            }
        }
        let arguments: Vec<&str> = line.split(" ").collect();
        allnodes.insert(arguments[0].to_string(), Node {
            name: arguments[0].to_string(),
            weight: arguments[1].parse::<i32>().unwrap(),
            total_weight: 0,
            children: children
        });
    }

    return allnodes;
}