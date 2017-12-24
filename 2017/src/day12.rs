use std::collections::HashMap;

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn part1_sample_test() {
    let sample_input = "0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5";

    assert_eq!(6, run1(parse_input(sample_input)));
  }

  #[test]
  fn part2_sample_test() {
    let sample_input = "0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5";

    assert_eq!(2, run2(parse_input(sample_input)));
  }

  #[test]
  fn part1_test() {
    assert_eq!(380, part1());
  }

  #[test]
  fn part2_test() {
    assert_eq!(181, part2());
  }
}

pub fn part1() -> i32 {
    let input = include_str!("../inputs/day12.txt");

    run1(parse_input(input))
}
pub fn part2() -> i32 {
    let input = include_str!("../inputs/day12.txt");

    run2(parse_input(input))
}

fn run1(map:HashMap<String, String>) -> i32 {
    let mut network = Vec::new();
    dfs_count(&map, &mut network, "0".to_string());

    network.len() as i32
}

fn run2(map:HashMap<String, String>) -> i32 {
    let mut network = Vec::new();
    dfs_count(&map, &mut network, "0".to_string());

    let num_progs = map.keys().len();

    let mut groupsum = 1;
    for i in 0..num_progs {
        if !network.contains(&i.to_string()) {
            groupsum += 1;
            dfs_count(&map, &mut network, i.to_string());
        }
    }

    groupsum
}

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

fn parse_input(input:&str) -> HashMap<String, String> {
    let mut map:HashMap<String, String> = HashMap::new();

    for line in input.split("\n") {
        let split:Vec<&str> = line.split(" <-> ").collect();

        map.insert(split[0].to_string(), split[1].to_string());
    }

    map
}