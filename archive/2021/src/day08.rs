use std::collections::HashSet;

use crate::Day;

pub struct Day08 {
    pub input: Vec<String>,
}

fn solve(line: &String) -> u64 {
    let words = line.split_whitespace().collect::<Vec<&str>>();

    let inputs = words[..10].iter().map(|w| HashSet::from_iter(w.chars())).collect::<Vec<HashSet<char>>>();
    let outputs = words[11..].iter().map(|w| HashSet::from_iter(w.chars())).collect::<Vec<HashSet<char>>>();

    let num7 = inputs.iter().find(|n| n.len() == 3).unwrap();
    let num4 = inputs.iter().find(|n| n.len() == 4).unwrap();

    let solve = |inp: &HashSet<char>| match inp.len() {
        2 => 1,
        3 => 7,
        4 => 4,
        // 2, 3, 5
        5 => if num7.is_subset(inp) { 3 } else { if num4.intersection(inp).collect::<Vec<&char>>().len() == 2 { 2 } else { 5 } }
        // 0, 6, 9
        6 => if num4.is_subset(inp) { 9 } else { if num7.is_subset(inp) { 0 } else { 6 } }
        7 => 8,
        _ => panic!("unknown {:?}", inp)
    };

    1000*solve(&outputs[0]) +
    100*solve(&outputs[1]) +
    10*solve(&outputs[2]) +
    1*solve(&outputs[3])
}

impl Day for Day08 {
    // naive implementation.. Didn't cut it for part 2
    fn part1(&self) -> String {
        let outputsignals: Vec<&str> = self.input.iter().flat_map(|line| {
            let words: Vec<&str> = line.split_whitespace().collect();

            words[11..].to_vec()
        }).collect();
        
        // count words with len 2,3,4,7
        outputsignals.iter().filter(|x| x.len() == 2 || x.len() == 3 || x.len() == 4 || x.len() == 7).count().to_string()
    }

    fn part2(&self) -> String {
        self.input
            .iter()
            .map(|line| solve(line))
            .sum::<u64>()
            .to_string()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &str = r#"be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
    edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
    fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
    fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
    aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
    fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
    dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
    bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
    egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
    gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"#;
    const INPUT2: &str = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf";

    fn get_day(input_num: u8) -> Day08 {
        let inp = match input_num {
            0 => include_str!("../inputs/day08.txt"),
            1 => INPUT,
            2 => INPUT2,
            _ => panic!(),
        };

        Day08 {
            input: inp.split('\n').map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part2_extra_example() {
        let d = get_day(2);

        assert_eq!("5353", d.part2());
    }

    #[test]
    fn part1_example() {
        let d = get_day(1);

        assert_eq!("26", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = get_day(1);

        assert_eq!("61229", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("247", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("933305", d.part2());
    }
}
