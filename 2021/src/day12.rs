use std::collections::{HashMap, HashSet};

use crate::Day;

pub struct Day12 {
    pub input: Vec<String>,
}

type Map = HashMap<String, Vec<String>>;

#[derive(Clone, Debug)]
struct Path {
    last: String,
    been: HashSet<String>,
    double_seen: bool,
}

impl Path {
    fn start() -> Path {
        Path {
            last: "start".to_string(),
            been: HashSet::from_iter(vec!["start".to_string()]),
            double_seen: false,
        }
    }

    fn has_double(&self) -> bool {
        self.double_seen
    }

    fn cave_added(&self, cave: &String) -> Path {
        let mut new_path = self.clone();

        new_path.last = cave.clone();
        new_path.been.insert(cave.clone());

        if !is_upper(cave) && self.been.contains(cave) {
            new_path.double_seen = true;
        }

        new_path
    }

    fn last(&self) -> &String {
        &self.last
    }

    fn is_new(&self, cave: &String) -> bool {
        !self.been.contains(cave)
    }
}

struct Indice {
    pub from: String,
    pub to: String,
}

impl std::str::FromStr for Indice {
    type Err = core::num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let split: Vec<&str> = s.split("-").collect();

        Ok(Indice {
            from: split[0].to_string(),
            to: split[1].to_string(),
        })
    }
}

fn is_upper(x: &String) -> bool {
    x.chars().all(|c: char| c.is_uppercase())
}

impl Day12 {
    fn create_map(&self) -> Map {
        let indices: Vec<Indice> = self
            .input
            .iter()
            .map(|l| l.parse::<Indice>().unwrap())
            .collect();

        let mut map = Map::new();
        for p in indices {
            // heen:
            let path = map.entry(p.from.clone()).or_insert(Vec::new());
            path.push(p.to.clone());

            // en weer:
            let path = map.entry(p.to).or_insert(Vec::new());
            path.push(p.from);
        }

        map
    }

    fn dfs_caves(&self, allowed_double: bool) -> usize {
        let map = self.create_map();

        let mut todo: Vec<Path> = vec![Path::start()];
        let mut paths: Vec<Path> = vec![];

        while !todo.is_empty() {
            let cur_path = todo.pop().unwrap();
            let possibilities = map.get(cur_path.last()).unwrap();

            for cave in possibilities {
                if cave == "start" {
                    continue;
                }
                if cave == "end" {
                    paths.push(cur_path.cave_added(cave));
                    continue;
                }

                if is_upper(&cave)
                    || cur_path.is_new(&cave)
                    || (allowed_double && !cur_path.has_double())
                {
                    todo.push(cur_path.cave_added(cave));
                }
            }
        }

        paths.len()
    }
}

impl Day for Day12 {
    fn part1(&self) -> String {
        self.dfs_caves(false).to_string()
    }
    fn part2(&self) -> String {
        self.dfs_caves(true).to_string()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT1: &str = r#"start-A
start-b
A-c
A-b
b-d
A-end
b-end"#;

    const INPUT2: &str = r#"dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"#;

    const INPUT3: &str = r#"fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW"#;

    fn get_day(input_num: u8) -> Day12 {
        let inp = match input_num {
            0 => include_str!("../inputs/day12.txt"),
            1 => INPUT1,
            2 => INPUT2,
            3 => INPUT3,
            _ => panic!(),
        };

        Day12 {
            input: inp.split('\n').map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example1() {
        let d = get_day(1);

        assert_eq!("10", d.part1());
    }

    #[test]
    fn part1_example2() {
        let d = get_day(2);

        assert_eq!("19", d.part1());
    }

    #[test]
    fn part1_example3() {
        let d = get_day(3);

        assert_eq!("226", d.part1());
    }

    #[test]
    fn part2_example1() {
        let d = get_day(1);

        assert_eq!("36", d.part2());
    }

    #[test]
    fn part2_example2() {
        let d = get_day(2);

        assert_eq!("103", d.part2());
    }

    #[test]
    fn part2_example3() {
        let d = get_day(3);

        assert_eq!("3509", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("4549", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("120535", d.part2());
    }
}
