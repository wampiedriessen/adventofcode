use std::cmp::Ordering;
use std::str::FromStr;
use crate::Day;

pub struct Day13 {
    pub input: Vec<String>,
}

#[derive(Clone, PartialEq, Eq)]
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

impl PartialOrd for Packet {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Packet::Int(l), Packet::Int(r)) if l != r => Some(l.cmp(r)),
            (Packet::Int(_l), Packet::Int(_r)) => None,
            (Packet::List(l), Packet::List(r)) => {
                for i in 0..l.len() {
                    if i >= r.len() { return Some(Ordering::Greater); }
                    if let Some(x) = l[i].partial_cmp(&r[i]) {
                        return Some(x);
                    }
                }
                if r.len() > l.len() { Some(Ordering::Less) } else { None }
            },
            (Packet::List(_), Packet::Int(r)) => self.partial_cmp(&Packet::List(vec![Packet::Int(*r)])),
            (Packet::Int(l), Packet::List(_)) => Packet::List(vec![Packet::Int(*l)]).partial_cmp( other),
        }
    }
}

impl Ord for Packet {
    fn cmp(&self, other: &Self) -> Ordering {
        if let Some(x) = self.partial_cmp(other) {
            x
        } else {
            Ordering::Equal
        }
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

impl Day for Day13 {
    fn part1(&self) -> String {
        let pairs = parse_input(&self.input);

        let mut sum = 0;
        for (i, pair) in pairs.iter().enumerate() {
            if pair.left <= pair.right { sum += i + 1; }
        }

        sum.to_string()
    }

    fn part2(&self) -> String {
        let mut packets = parse_input2(&self.input);

        let pack2 = Packet::List(vec![Packet::List(vec![Packet::Int(2)])]);
        let pack6 = Packet::List(vec![Packet::List(vec![Packet::Int(6)])]);

        packets.push(pack2.clone());
        packets.push(pack6.clone());

        packets.sort();

        let mut ipack2 = 0;
        let mut ipack6 = 0;
        for (i, p) in packets.iter().enumerate() {
            if p == &pack2 {
                ipack2 = i + 1;
            }
            if p == &pack6 {
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
