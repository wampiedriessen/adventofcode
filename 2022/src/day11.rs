use std::collections::VecDeque;
use crate::Day;
use std::str::FromStr;

pub struct Day11 {
    pub input: String,
}

#[derive(Debug)]
struct Operation {
    op: fn(x: i64) -> i64,
}

impl FromStr for Operation {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Operation {
            op: match s {
                "new = old * 17" => |old| old * 17,
                "new = old * 19" => |old| old * 19,
                "new = old * 2" => |old| old * 2,
                "new = old * old" => |old| old * old,
                "new = old + 1" => |old| old + 1,
                "new = old + 2" => |old| old + 2,
                "new = old + 3" => |old| old + 3,
                "new = old + 6" => |old| old + 6,
                "new = old + 7" => |old| old + 7,
                "new = old + 8" => |old| old + 8,
                _ => panic!("Unknown operation!"),
            },
        })
    }
}

#[derive(Debug)]
struct Monkey {
    items: VecDeque<i64>,
    operation: Operation,
    test_divisible_by: i64,
    goto_if_true: usize,
    goto_if_false: usize,
}

impl FromStr for Monkey {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut lines: VecDeque<&str> = s.lines().collect();
        lines.pop_front();
        let itemline = lines.pop_front().unwrap();
        let operationline = lines.pop_front().unwrap();
        let testline = lines.pop_front().unwrap();
        let gototrueline = lines.pop_front().unwrap();
        let gotofalseline = lines.pop_front().unwrap();
        let spl_items: Vec<&str> = itemline.split("Starting items: ").collect();
        let items: VecDeque<i64> = spl_items[1]
            .split(", ")
            .map(|x| x.parse().unwrap())
            .collect();

        Ok(Monkey {
            items,
            operation: operationline[13..].parse().unwrap(),
            test_divisible_by: testline[21..].parse().unwrap(),
            goto_if_true: gototrueline[29..].parse().unwrap(),
            goto_if_false: gotofalseline[30..].parse().unwrap(),
        })
    }
}

impl Day11 {
    fn get_monkeys(&self) -> Vec<Monkey> {
        self.input
            .split("\n\n")
            .map(|x| x.parse().unwrap())
            .collect()
    }
}

impl Day for Day11 {
    fn part1(&self) -> String {
        let mut monkeys: Vec<Monkey> = self.get_monkeys();
        let mut count_actions = vec![0; monkeys.len()];
        let common_divider = get_common_divider(&monkeys);

        for _ in 0..20 {
            monkey_business(&mut monkeys, &mut count_actions, 3, common_divider);
        }

        score(count_actions).to_string()
    }

    fn part2(&self) -> String {
        let mut monkeys: Vec<Monkey> = self.get_monkeys();
        let mut count_actions = vec![0; monkeys.len()];
        let common_divider = get_common_divider(&monkeys);

        for _ in 0..10000 {
            monkey_business(&mut monkeys, &mut count_actions, 1, common_divider);
        }

        score(count_actions).to_string()
    }
}

fn get_common_divider(monkeys: &[Monkey]) -> i64 {
    let mut common_divider = 1;
    for divider in monkeys.iter().map(|x| x.test_divisible_by) {
        common_divider *= divider;
    }

    common_divider
}

fn score(mut count_actions: Vec<i64>) -> i64 {
    count_actions.sort();

    let index = count_actions.len() - 2;
    let last_2 = count_actions.get(index..).unwrap();

    last_2[0] * last_2[1]
}

fn monkey_business(
    monkeys: &mut Vec<Monkey>,
    count_actions: &mut [i64],
    worry_level_divider: i64,
    common_divider: i64
) {
    for i in 0..monkeys.len() {
        while let Some(curitem) = monkeys[i].items.pop_front() {
            let op = monkeys[i].operation.op;
            let newworrylevel = op(curitem) / worry_level_divider;

            let newmonkey = if newworrylevel % monkeys[i].test_divisible_by == 0 {
                monkeys[i].goto_if_true
            } else {
                monkeys[i].goto_if_false
            };

            count_actions[i] += 1;

            monkeys[newmonkey].items.push_back(newworrylevel % common_divider);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &str = "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1";

    fn get_day(input_num: u8) -> Day11 {
        let inp = match input_num {
            0 => include_str!("../inputs/day11.txt"),
            1 => INPUT,
            _ => panic!(),
        };

        Day11 {
            input: inp.to_string(),
        }
    }

    #[test]
    fn part1_example() {
        let d = get_day(1);

        assert_eq!("10605", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = get_day(1);

        assert_eq!("2713310158", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("58322", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("13937702909", d.part2());
    }
}
