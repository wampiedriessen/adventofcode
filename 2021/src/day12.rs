use std::collections::HashMap;
use std::rc::Rc;

use crate::Day;

pub struct Day12 {
    pub input: Vec<String>,
}

type Map = HashMap<String, Vec<String>>;

enum Node<'a> {
    Cave((bool, &'a String), Rc<Node<'a>>),
    End(Rc<Node<'a>>),
    Start,
}

const START: &str = "start";
const END: &str = "end";

impl Node<'_> {
    fn get_name(&self) -> &str {
        match self {
            Node::Cave((_, x), _) => x,
            Node::Start => START,
            Node::End(_) => END,
        }
    }

    fn is_new(&self, cave: &String) -> bool {
        let mut node = self;

        loop {
            match node {
                Node::Start => return true,
                Node::Cave((_, x), next) => {
                    if x == &cave {
                        return false;
                    }
                    node = next;
                }
                Node::End(_) => panic!("cannot end here"),
            }
        }
    }

    fn has_double(&self) -> bool {
        match self {
            Node::Start => false,
            Node::End(x) => x.has_double(),
            Node::Cave((x, _), _) => *x,
        }
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

    fn dfs_rc_caves(&self, allowed_double: bool) -> usize {
        let map = self.create_map();

        let mut todo: Vec<Node> = vec![Node::Start];
        let mut paths: Vec<Node> = vec![];

        while !todo.is_empty() {
            let cur_node = Rc::new(todo.pop().unwrap());
            let possibilities = map.get(&cur_node.get_name().to_string()).unwrap();

            for cave in possibilities {
                if cave == &START {
                    continue;
                }

                if cave == &END {
                    paths.push(Node::End(Rc::clone(&cur_node)));
                    continue;
                }

                if is_upper(&cave) || cur_node.is_new(&cave) {
                    let nodeval = (cur_node.has_double(), cave);
                    todo.push(Node::Cave(nodeval, Rc::clone(&cur_node)));
                    continue;
                }

                if allowed_double && !cur_node.has_double() {
                    todo.push(Node::Cave((true, cave), Rc::clone(&cur_node)));
                }
            }
        }

        paths.len()
    }
}

impl Day for Day12 {
    fn part1(&self) -> String {
        self.dfs_rc_caves(false).to_string()
    }
    fn part2(&self) -> String {
        self.dfs_rc_caves(true).to_string()
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
