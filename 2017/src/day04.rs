#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_sample_test() {
        assert_eq!(1, run1(vec!["aa bb cc dd ee"]));
        assert_eq!(0, run1(vec!["aa bb cc dd aa"]));
        assert_eq!(1, run1(vec!["aa bb cc dd aaa"]));
    }

    #[test]
    fn part2_sample_test() {
        assert_eq!(1, run2(vec!["abcde fghij"]));
        assert_eq!(0, run2(vec!["abcde xyz ecdab"]));
        assert_eq!(1, run2(vec!["a ab abc abd abf abj"]));
        assert_eq!(1, run2(vec!["iiii oiii ooii oooi oooo"]));
        assert_eq!(0, run2(vec!["oiii ioii iioi iiio"]));
    }

    #[test]
    fn part1_test() {
        assert_eq!(451, part1());
    }

    #[test]
    fn part2_test() {
        assert_eq!(223, part2());
    }
}

pub fn part1() -> u32{
    let input = include_str!("../inputs/day04.txt").split("\n").collect();
    return run1(input);
}

pub fn part2() -> u32 {
    let input = include_str!("../inputs/day04.txt").split("\n").collect();
    return run2(input);
}

fn run1(input:Vec<&str>) -> u32 {
    let mut valid = 0;
    let mut v: Vec<String> = Vec::new();
    for line in input {
        v.clear();
        let mut correct:bool = true;
        for word in line.trim().split(" ") {
            if v.contains(&word.to_string()) { correct = false; break; }
            v.push(word.to_string());
        }
        if correct {
            valid += 1;
        }
    }
    return valid;
}

fn run2(input:Vec<&str>) -> u32 {
    let mut valid = 0;

    let mut v: Vec<String> = Vec::new();
    for line in input {
        v.clear();
        let mut correct:bool = true;
        for word in line.trim().split(" ") {
            if contains_anagram(&v, &word.to_string()) { correct = false; break; }
            v.push(word.to_string());
        }
        if correct {
            valid += 1;
        }
    }
    return valid;
}

fn is_anagram(a:&str, b:&str) -> bool {
    if a.len() != b.len() {
        return false;
    }

    let mut av:Vec<char> = a.chars().collect();
    let mut bv:Vec<char> = b.chars().collect();

    av.sort();
    bv.sort();

    return av == bv;
}

fn contains_anagram(v: &Vec<String>, s:&str) -> bool {
    for word in v {
        if is_anagram(word, s) {
            return true;
        }
    }
    return false;
}