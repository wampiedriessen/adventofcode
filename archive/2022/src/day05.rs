use std::collections::VecDeque;
use std::str::FromStr;
use crate::Day;

pub struct Day05 {
    pub input: Vec<String>,
}

struct Crate {
    letter: char
}

struct Instruction {
    num: usize,
    src: usize,
    dst: usize
}

struct CrateStacks {
    stacks: Vec<VecDeque<Crate>>,
    instructions: VecDeque<String>
}

impl FromStr for Instruction {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        assert!(s.starts_with("move "));
        let splits: Vec<&str> = s.split_whitespace().collect();

        Ok(Instruction{
            num: splits[1].parse().unwrap(),
            src: splits[3].parse().unwrap(),
            dst: splits[5].parse().unwrap(),
        })
    }
}

impl FromStr for Crate {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if !s.starts_with('[') || !s.trim().ends_with(']') {
            return Err(());
        }

        Ok(Crate {
            letter: s.chars().nth(1).unwrap()
        })
    }
}

impl CrateStacks {
    fn parse_from_lines(input: &Vec<String>) -> CrateStacks {
        let mut stacks = Vec::with_capacity(10);
        let mut instructions = VecDeque::new();

        for line in input {
            if line.starts_with(" 1") || line.trim() == "" {
                continue;
            }
            if line.starts_with("move") {
                instructions.push_back(line.clone());
                continue;
            }
            let chunks = line.as_bytes().chunks(4);

            for (i, chunk) in chunks.enumerate() {
                if stacks.len() >= i {
                    stacks.push(VecDeque::new());
                }

                if let Ok(newcrate) = String::from_utf8_lossy(chunk).parse() {
                    stacks[i].push_back(newcrate);
                }
            }
        }

        CrateStacks {
            stacks,
            instructions
        }
    }

    fn step(&mut self) {
        assert_ne!(0, self.instructions.len());
        let instruction_str = self.instructions.pop_front().unwrap();

        let instruction: Instruction = instruction_str.parse().unwrap();

        for _ in 0..instruction.num {

            let thing = self.stacks[instruction.src - 1].pop_front().unwrap();

            self.stacks[instruction.dst - 1].push_front(thing);
        }
    }

    fn step2(&mut self) {
        assert_ne!(0, self.instructions.len());
        let instruction_str = self.instructions.pop_front().unwrap();

        let instruction: Instruction = instruction_str.parse().unwrap();

        let mut movestack = VecDeque::new();

        for _ in 0..instruction.num {
            let thing = self.stacks[instruction.src - 1].pop_front().unwrap();

            movestack.push_front(thing);
        }

        for thing in movestack {
            self.stacks[instruction.dst - 1].push_front(thing);
        }
    }

    fn get_tops(&self) -> String {
        let mut out = String::new();

        for stack in &self.stacks {
            if let Some(cratez) = stack.get(0) {
                out.push(cratez.letter);
            }
        }

        out
    }
}

impl Day for Day05 {
    fn part1(&self) -> String {
        let mut stacks = CrateStacks::parse_from_lines(&self.input);

        while !stacks.instructions.is_empty() {
            stacks.step();
        }

        stacks.get_tops()
    }

    fn part2(&self) -> String {
        let mut stacks = CrateStacks::parse_from_lines(&self.input);

        while !stacks.instructions.is_empty() {
            stacks.step2();
        }

        stacks.get_tops()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &str = "    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2";

    fn get_day(input_num: u8) -> Day05 {
        let inp = match input_num {
            0 => include_str!("../inputs/day05.txt"),
            1 => INPUT,
            _ => panic!(),
        };

        Day05 {
            input: inp.lines().map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example() {
        let d = get_day(1);

        assert_eq!("CMZ", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = get_day(1);

        assert_eq!("MCD", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("TPGVQPFDH", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("DMRDFRHHH", d.part2());
    }
}
