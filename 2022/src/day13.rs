use std::cmp::Ordering;
use std::str::FromStr;
use crate::Day;

pub struct Day13 {
    pub input: Vec<String>,
}

#[derive(Clone)]
enum Packet {
    List(Vec<Packet>),
    Int(i32)
}

impl FromStr for Packet {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if !s.starts_with('[') { return Ok(Packet::Int(s.parse().unwrap())); }

        let chars: Vec<char> = s.chars().collect();
        let mut brackets_deep = 0;
        let mut content = Vec::new();

        let mut buffer = String::new();
        for kar in chars {
            if brackets_deep >= 1 { buffer.push(kar); }

            match kar {
                '[' => brackets_deep += 1,
                ']' => brackets_deep -= 1,
                ',' if brackets_deep == 1 => { buffer.pop(); content.push(buffer.parse().unwrap()); buffer.clear() },
                _ => ()
            }
        }

        if buffer != "]" { buffer.pop(); content.push(buffer.parse().unwrap()); }

        Ok(Packet::List(content))
    }
}

struct Pair {
    left: Packet,
    right: Packet,
}

fn parse_input(lines: &[String]) -> Vec<Pair> {
    let mut pairs = Vec::new();

    for chunk in lines.chunks(3) {
        pairs.push(Pair{
            left: chunk[0].parse().unwrap(),
            right: chunk[1].parse().unwrap(),
        })
    }

    pairs
}

fn parse_input2(lines: &[String]) -> Vec<Packet> {
    let mut pairs = Vec::new();

    for chunk in lines.chunks(3) {
        pairs.push(chunk[0].parse().unwrap());
        pairs.push( chunk[1].parse().unwrap());
    }

    pairs
}

fn in_order(left: &Packet, right: &Packet) -> Option<bool> {
    match (left, right) {
        (Packet::Int(l), Packet::Int(r)) if l < r => Some(true),
        (Packet::Int(l), Packet::Int(r)) if l > r => Some(false),
        (Packet::Int(_), Packet::Int(_)) => None,
        (Packet::List(l), Packet::List(r)) => {
            for (i, it) in l.iter().enumerate() {
                if i >= r.len() { return Some(false); }
                match in_order(it, &r[i]) {
                    None => { continue; }
                    Some(x) => { return Some(x) }
                }
            }
            if r.len() > l.len() { Some(true) } else { None }
        },
        (Packet::List(_), Packet::Int(_)) => in_order(left, &Packet::List(vec![right.clone()])),
        (Packet::Int(_), Packet::List(_)) => in_order(&Packet::List(vec![left.clone()]), right),
    }
}

impl Day for Day13 {
    fn part1(&self) -> String {
        let pairs = parse_input(&self.input);

        let mut sum = 0;
        for (i, pair) in pairs.iter().enumerate() {
            if in_order(&pair.left, &pair.right).unwrap() { sum += i + 1; }
        }

        sum.to_string()
    }

    fn part2(&self) -> String {
        let mut packets = parse_input2(&self.input);

        let pack2 = Packet::List(vec![Packet::List(vec![Packet::Int(2)])]);
        let pack6 = Packet::List(vec![Packet::List(vec![Packet::Int(6)])]);

        packets.push(pack2.clone());
        packets.push(pack6.clone());

        packets.sort_by(|x, x1| {
            match in_order(x, x1) {
                Some(false) => Ordering::Greater,
                Some(true) => Ordering::Less,
                None => Ordering::Equal,
            }
        });

        let mut ipack2 = 0;
        let mut ipack6 = 0;
        for (i, p) in packets.iter().enumerate() {
            if in_order(p, &pack2).is_none() {
                ipack2 = i + 1;
            }
            if in_order(p, &pack6).is_none() {
                ipack6 = i + 1;
            }
        }

        (ipack2 * ipack6).to_string()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &str = "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]";

    fn get_day(input_num: u8) -> Day13 {
        let inp = match input_num {
            0 => include_str!("../inputs/day13.txt"),
            1 => INPUT,
            _ => panic!(),
        };

        Day13 {
            input: inp.lines().map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example() {
        let d = get_day(1);

        assert_eq!("13", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = get_day(1);

        assert_eq!("140", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("6428", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("22464", d.part2());
    }
}
