#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_sample_test() {
        assert_eq!(5, run1("0\n3\n0\n1\n-3"));
    }

    #[test]
    fn part2_sample_test() {
        assert_eq!(10, run2("0\n3\n0\n1\n-3"));
    }

    #[test]
    fn part1_test() {
        assert_eq!(373543, part1());
    }

    #[test]
    fn part2_test() {
        assert_eq!(27502966, part2());
    }
}

pub fn part1() -> u32 {
    let input = include_str!("../inputs/day05.txt");
    return run1(input);
}

pub fn part2() -> u32 {
    let input = include_str!("../inputs/day05.txt");
    return run2(input);
}

fn run1(input:&str) -> u32 {
    let mut maze:Vec<i32> = parse_input(input);
    let mut index: i32 = 0;
    let mut steps: u32 = 0;

    while index >= 0 && (index as usize) < maze.len() {
        let newindex = index + maze[index as usize];
        maze[index as usize] += 1;
        index = newindex;
        steps += 1;
    }

    return steps;
}

fn run2(input:&str) -> u32 {
    let mut maze:Vec<i32> = parse_input(input);
    let mut index: i32 = 0;
    let mut steps: u32 = 0;

    while index >= 0 && (index as usize) < maze.len() {
        let newindex = index + maze[index as usize];
        let offset = maze[index as usize];
        if offset >= 3 {
            maze[index as usize] -= 1;
        } else {
            maze[index as usize] += 1;
        }
        index = newindex;
        steps += 1;
    }

    return steps;
}

fn parse_input(input:&str) -> Vec<i32> {
    return input.split_whitespace()
        .filter_map(|e| e.parse().ok())
        .collect::<Vec<i32>>();
}