use crate::Day;

pub struct Day16 {
    pub input: Vec<String>,
}

fn parse_hex(x: char) -> &'static str {
    match x {
        '0' => "0000",
        '1' => "0001",
        '2' => "0010",
        '3' => "0011",
        '4' => "0100",
        '5' => "0101",
        '6' => "0110",
        '7' => "0111",
        '8' => "1000",
        '9' => "1001",
        'A' => "1010",
        'B' => "1011",
        'C' => "1100",
        'D' => "1101",
        'E' => "1110",
        'F' => "1111",
        _ => panic!("Unknown hex!"),
    }
}

fn str_to_bitstring(x: String) -> String {
    let mut bits = String::new();
    let chars: Vec<char> = x.chars().collect();
    for c in chars {
        bits += parse_hex(c);
    }
    bits
}

fn parse_bool(input: &str) -> u64 {
    let mut n = 0;

    for c in input.chars() {
        n <<= 1;
        n += match c {
            '1' => 1,
            '0' => 0,
            _ => panic!("Unknown bit!"),
        };
    }

    n
}

fn is_literal(x: u64) -> bool {
    x == 4
}

fn parse_literal_value(input: &str) -> (usize, u64) {
    let mut offset = 0;
    let mut bitstring = String::new();

    while &input[offset..offset + 1] != "0" {
        bitstring += &input[offset + 1..offset + 5];
        offset += 5;
    }
    bitstring += &input[offset + 1..offset + 5];
    offset += 5;

    (offset, parse_bool(bitstring.as_str()))
}

#[derive(Debug)]
enum Packet {
    Literal(u64, u64),
    Operator(u64, u64, Vec<Packet>),
}

impl Packet {
    fn parse(input: &str) -> (usize, Packet) {
        let mut read = 0;
        let version = parse_bool(&input[0..3]);
        let ptype = parse_bool(&input[3..6]);
        read += 6;

        if is_literal(ptype) {
            let (data_read, data) = parse_literal_value(&input[6..]);
            return (read + data_read, Packet::Literal(version, data));
        }

        // operator
        let lenfiel = &input[6..7];
        read += 1;
        let subpackets;

        if lenfiel == "0" {
            let len_subpackets = parse_bool(&input[7..22]) as usize;
            read += 15;
            subpackets = Self::parse_multiple_packets(&input[22..], len_subpackets);
            read += len_subpackets
        } else {
            let num_subpackets = parse_bool(&input[7..18]);
            read += 11;
            let (sub_read, packets) = Self::read_n_packets(&input[18..], num_subpackets);
            read += sub_read;
            subpackets = packets;
        }

        (read, Packet::Operator(version, ptype, subpackets))
    }

    fn parse_multiple_packets(input: &str, datalen: usize) -> Vec<Packet> {
        let mut read = 0;
        let mut out = Vec::new();

        while read < datalen {
            let (sub_read, packet) = Packet::parse(&input[read..]);
            read += sub_read;
            out.push(packet);
        }

        debug_assert_eq!(read, datalen);

        out
    }

    fn read_n_packets(input: &str, n: u64) -> (usize, Vec<Packet>) {
        let mut read = 0;
        let mut out = Vec::new();

        for _ in 0..n {
            let (sub_read, packet) = Packet::parse(&input[read..]);
            out.push(packet);
            read += sub_read;
        }

        (read, out)
    }

    fn compute(&self) -> u64 {
        match self {
            Packet::Literal(_, data) => *data,
            Packet::Operator(_, 0, children) => children.iter().map(|c| c.compute()).sum::<u64>(),
            Packet::Operator(_, 1, children) => children
                .iter()
                .fold::<u64, _>(1, |acc, c| acc * c.compute()),
            Packet::Operator(_, 2, children) => children.iter().map(|c| c.compute()).min().unwrap(),
            Packet::Operator(_, 3, children) => children.iter().map(|c| c.compute()).max().unwrap(),
            Packet::Operator(_, 5, children) => {
                if children[0].compute() > children[1].compute() {
                    1
                } else {
                    0
                }
            }
            Packet::Operator(_, 6, children) => {
                if children[0].compute() < children[1].compute() {
                    1
                } else {
                    0
                }
            }
            Packet::Operator(_, 7, children) => {
                if children[0].compute() == children[1].compute() {
                    1
                } else {
                    0
                }
            }
            _ => panic!("Unkown ptype"),
        }
    }

    fn sum_versions(&self) -> u64 {
        match self {
            Packet::Literal(v, _) => *v,
            Packet::Operator(v, _, children) => {
                *v + children.iter().map(|c| c.sum_versions()).sum::<u64>()
            }
        }
    }
}

impl Day for Day16 {
    fn part1(&self) -> String {
        let bitstring = str_to_bitstring(self.input[0].clone());

        let (_, outerpacket) = Packet::parse(bitstring.as_str());

        outerpacket.sum_versions().to_string()
    }

    fn part2(&self) -> String {
        let bitstring = str_to_bitstring(self.input[0].clone());

        let (_, outerpacket) = Packet::parse(bitstring.as_str());

        outerpacket.compute().to_string()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn d_from_input(inp: &str) -> Day16 {
        Day16 {
            input: inp.split('\n').map(|s| s.to_string()).collect(),
        }
    }

    fn get_day(input_num: u8) -> Day16 {
        let inp = match input_num {
            0 => include_str!("../inputs/day16.txt"),
            _ => panic!(),
        };

        d_from_input(inp)
    }

    #[test]
    fn part1_examples() {
        let examples = vec![
            ("8A004A801A8002F478", "16"),
            ("620080001611562C8802118E34", "12"),
            ("C0015000016115A2E0802F182340", "23"),
            ("A0016C880162017C3686B18A3D4780", "31"),
            ("D2FE28", "6"),
            ("38006F45291200", "9"),
        ];
        for (inp, expected) in examples {
            let d = d_from_input(inp);
            assert_eq!(expected, d.part1());
        }
    }

    #[test]
    fn part1() {
        let d = get_day(0);
        assert_eq!("947", d.part1());
    }

    #[test]
    fn part2_examples() {
        let examples = vec![
            ("C200B40A82", "3"),
            ("04005AC33890", "54"),
            ("880086C3E88112", "7"),
            ("CE00C43D881120", "9"),
            ("D8005AC2A8F0", "1"),
            ("F600BC2D8F", "0"),
            ("9C005AC2F8F0", "0"),
            ("9C0141080250320F1802104A08", "1"),
        ];
        for (inp, expected) in examples {
            let d = d_from_input(inp);
            assert_eq!(expected, d.part2());
        }
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("660797830937", d.part2());
    }
}
