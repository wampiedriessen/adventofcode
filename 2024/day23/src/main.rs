use std::collections::{HashMap, HashSet};
use std::io;
use std::io::Read;

fn pairs_of_two<'a>(set: &'a HashSet<&'a str>) -> Vec<(&'a str, &'a str)> {
    let mut out = Vec::new();

    let mut allvals: Vec<&str> = set.iter().cloned().collect();

    while let Some(val) = allvals.pop() {
        for x in &allvals {
            out.push((*x, val));
        }
    }

    out
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let mut connections = HashMap::new();
    for line in input.lines() {
        let (a, b) = line.split_once('-').unwrap();

        connections.entry(a).or_insert(HashSet::new()).insert(b);
        connections.entry(b).or_insert(HashSet::new()).insert(a);
    }

    println!("Part 1: {}", part1(&connections));
    println!("Part 1: {}", part2(&connections));

    Ok(())
}

fn part1(connections: &HashMap<&str, HashSet<&str>>) -> usize {
    let pcs = connections.keys().cloned().collect::<Vec<_>>();

    let mut found_sets = HashSet::new();
    for pc in pcs {
        if !pc.starts_with('t') { continue }

        for (a, b) in pairs_of_two(&connections[pc]) {
            if connections[&a].contains(&b) {
                let mut x = vec![pc, a, b];
                x.sort();
                found_sets.insert((x[0], x[1], x[2]));
            }
        }
    }

    found_sets.len()
}

fn part2(connections: &HashMap<&str, HashSet<&str>>) -> String {
    let mut sets_by_alphabet: HashMap<String, Vec<HashSet<&str>>> = HashMap::new();

    let pcs = connections.keys().cloned().collect::<Vec<_>>();

    for pc in &pcs {
        let mut pregroups = Vec::new();
        for conn in &connections[pc] {
            pregroups.push(HashSet::from([*pc, *conn]));
        }
        sets_by_alphabet.insert(pc.to_string(), pregroups);
    }

    for pc in &pcs {
        let connections_of_pc = &connections[pc];
        for conn in connections_of_pc {
            let groups_of_conn = sets_by_alphabet.get_mut(&String::from(*conn)).unwrap();
            for group in groups_of_conn {
                if group.is_subset(connections_of_pc) {
                    group.insert(pc);
                }
            }
        }
    }

    let mut largest = HashSet::new();
    for pc in &pcs {
        for group in &sets_by_alphabet[*pc] {
            if group.len() > largest.len() {
                largest = group.clone();
            }
        }
    }

    let mut lanparty = largest.iter().cloned().collect::<Vec<_>>();
    lanparty.sort();

    lanparty.join(",")
}