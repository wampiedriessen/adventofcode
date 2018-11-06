#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_sample_test() {
        assert_eq!(5, run1(vec![0,2,7,0]));
    }

    #[test]
    fn part2_sample_test() {
        assert_eq!(4, run2(vec![0,2,7,0]));
    }

    #[test]
    fn part1_test() {
        assert_eq!(12841, part1());
    }

    #[test]
    fn part2_test() {
        assert_eq!(8038, part2());
    }
}

pub fn part1() -> i32 {
    let input = include_str!("../inputs/day06.txt");

    return run1(parse_input(input));
}

pub fn part2() -> i32 {
    let input = include_str!("../inputs/day06.txt");

    return run2(parse_input(input));
}

fn run1(banks:Vec<u8>) -> i32 {
    let mut seen:Vec<String> = Vec::with_capacity(10000);

    run(banks, &mut seen);

    return seen.len() as i32;
}

fn run2(banks:Vec<u8>) -> i32 {
    let mut seen:Vec<String> = Vec::with_capacity(10000);
    let index = run(banks, &mut seen);

    return seen.len() as i32 - index;
}

fn run(mut banks:Vec<u8>, mut seen: &mut Vec<String>) -> i32 {
    let mut index: i32;
    let len = banks.len();

    add_config(&mut seen, &banks);

    loop {
        let start = max_index(&banks);
        let distribute = banks[start].clone();
        for i in 0..distribute {
            banks[(start + 1 + i as usize) % len] += 1;
        }
        banks[start] -= distribute;
        index = add_config(&mut seen, &banks);
        if index != -1 {
            break;
        }
    }

    return index;
}

fn max_index(v:&Vec<u8>) -> usize {
    let mut ind: usize = 0;
    let mut max: u8 = 0;
    for i in 0..v.len() {
        if v[i] > max {
            ind = i;
            max = v[i];
        }
    }
    return ind;
}

fn add_config(confs: &mut Vec<String>, v:&Vec<u8>) -> i32 {
    let textvec: Vec<String> = v.iter().map(|x| x.to_string()).collect();
    let conf = textvec.join(",");
    if confs.contains(&conf) {
        return confs.iter().position(|x| *x == conf).unwrap() as i32;
    }
    confs.push(conf.clone());
    return -1;
}

fn parse_input(input:&str) -> Vec<u8> {
    let x:Result<Vec<u8>, _> =
        input.split_whitespace()
            .map(|token| token.parse())
            .collect();
    if let Ok(v) = x {
        return v;
    }
    panic!("Could not parse input");
}