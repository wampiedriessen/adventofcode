use std::collections::HashMap;

use crate::Day;

pub struct Day14 {
    pub input: Vec<String>,
}

type Occur = HashMap<char, u64>;

struct Solver {
    rules: HashMap<String, String>,
    cache: HashMap<(String, u8), Occur>,
}

fn merge(a: Occur, b: &Occur) -> Occur {
    let mut out = a;
    for (k, v) in b {
        let e = out.entry(*k).or_insert(0);

        *e += v;
    }

    out
}

impl Solver {
    fn new(input: &Vec<String>) -> Self {
        let mut rules: HashMap<String, String> = HashMap::new();

        for l in input[2..].iter() {
            let rule: Vec<&str> = l.split(" -> ").collect();
            let parts: Vec<&str> = rule[0].split("").collect();
            let start = rule[0].to_string();
            rules.insert(start, [parts[1], rule[1], parts[2]].join(""));
        }

        Solver {
            rules,
            cache: HashMap::new(),
        }
    }

    fn process(&mut self, poly: String, depth: u8) -> Occur {
        if depth == 0 {
            return Solver::occurrances(poly.chars());
        }

        let key = (poly.clone(), depth);
        if self.cache.contains_key(&key) {
            return self.cache[&key].clone();
        }

        if poly.len() == 2 {
            let next = self.rules[&poly].clone();

            let result = self.process(next, depth - 1);

            self.cache.insert((poly, depth), result.clone());
            // we "injected", so depth - 1
            return result;
        }

        let mut out = Occur::new();

        for i in 0..poly.len() - 1 {
            let window = poly[i..(i + 2)].to_string();

            // we only splitted, so no increase of depth
            let calc = self.process(window.clone(), depth);
            out = merge(out, &calc);

            if i != 0 {
                // remove double count
                let c = window.chars().nth(0).unwrap();
                *out.get_mut(&c).unwrap() -= 1;
            }
        }

        self.cache.insert(key, out.clone());

        out
    }

    fn occurrances(input: std::str::Chars) -> Occur {
        let mut out = Occur::new();

        let mut x: Vec<char> = input.collect();

        while let Some(c) = x.pop() {
            let x = out.entry(c).or_insert(0u64);
            *x += 1;
        }

        out
    }
}

fn score(occ: Occur) -> u64 {
    occ.values().max().unwrap() - occ.values().min().unwrap()
}

impl Day for Day14 {
    fn part1(&self) -> String {
        let polymer = self.input[0].clone();
        let mut solver = Solver::new(&self.input);

        let occ = solver.process(polymer, 10);

        score(occ).to_string()
    }

    fn part2(&self) -> String {
        let polymer = self.input[0].clone();
        let mut solver = Solver::new(&self.input);

        let occ = solver.process(polymer, 40);

        score(occ).to_string()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT1: &str = r#"NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"#;

    fn get_day(input_num: u8) -> Day14 {
        let inp = match input_num {
            0 => include_str!("../inputs/day14.txt"),
            1 => INPUT1,
            _ => panic!(),
        };

        Day14 {
            input: inp.split('\n').map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example1() {
        let d = get_day(1);

        assert_eq!("1588", d.part1());
    }

    #[test]
    fn part2_example1() {
        let d = get_day(1);

        assert_eq!("2188189693529", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("3259", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("3459174981021", d.part2());
    }
}
