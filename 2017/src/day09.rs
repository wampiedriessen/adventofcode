#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_sample_test() {
        assert_eq!(1, run1("{}"));
        assert_eq!(6, run1("{{{}}}"));
        assert_eq!(5, run1("{{},{}}"));
        assert_eq!(16, run1("{{{},{},{{}}}}"));
        assert_eq!(1, run1("{<a>,<a>,<a>,<a>}"));
        assert_eq!(9, run1("{{<ab>},{<ab>},{<ab>},{<ab>}}"));
        assert_eq!(9, run1("{{<!!>},{<!!>},{<!!>},{<!!>}}"));
        assert_eq!(3, run1("{{<a!>},{<a!>},{<a!>},{<ab>}}"));
    }

    #[test]
    fn part2_sample_test() {
        assert_eq!(0, run2("<>"));
        assert_eq!(17, run2("<random characters>"));
        assert_eq!(3, run2("<<<<>"));
        assert_eq!(2, run2("<{!>}>"));
        assert_eq!(0, run2("<!!>"));
        assert_eq!(0, run2("<!!!>>"));
        assert_eq!(10, run2("<{o\"i!a,<{i<a>"));
    }

    #[test]
    fn part1_test() {
        assert_eq!(11846, part1());
    }

    #[test]
    fn part2_test() {
        assert_eq!(6285, part2());
    }
}

pub fn part1() -> u32 {
    let input = include_str!("../inputs/day09.txt");

    return run1(input);
}

pub fn part2() -> u32 {
    let input = include_str!("../inputs/day09.txt");

    return run2(input);
}

fn run1(input:&str) -> u32 {
    let (score, _) = run(input);
    return score;
}

fn run2(input:&str) -> u32 {
    let (_, count) = run(input);
    return count;
}

fn run(line:&str) -> (u32, u32) {
    let mut depth:u32 = 0;
    let mut score:u32 = 0;
    let mut in_garbage:bool = false;
    let mut skip:bool = false;
    let mut garbage_count:u32 = 0;

    for chr in line.chars() {
        if skip {
            skip = false;
            continue;
        }
        if chr == '!' {
            if in_garbage {
                skip = true;
                continue;
            }
        }
        if in_garbage {
            if chr == '>' {
                in_garbage = false;
                continue;
            }
            garbage_count += 1;
            continue;
        }
        if chr == '{' {
            depth += 1;
        }
        if chr == '}' {
            if !in_garbage {
                score += depth;
                depth -= 1;
            }
        }
        if chr == '<' {
            in_garbage = true;
        }
    }

    return (score, garbage_count);
}