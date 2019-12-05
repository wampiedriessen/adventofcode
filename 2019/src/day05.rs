use super::intcode::Intcode;

pub fn part1() -> i32 {
    let input = include_str!("../inputs/day05.txt").trim();

    return run1(input);
}

pub fn part2() -> i32 {
    let input = include_str!("../inputs/day05.txt").trim();

    return run2(input);
}

fn run1(input:&str) -> i32 {
    return run_internal(input, 1);
}

fn run2(input:&str) -> i32 {
    return run_internal(input, 5);
}

fn run_internal(input: &str, system_id: i32) -> i32 {
    let program = Intcode::read_input(input);
    let mut t = Intcode::new(program);

    t.stdin(system_id);
    t.compute();

    // t.debug();

    let mut last_output = -1;
    while let Some(output) = t.stdout() {
        last_output = output;
    }

    return last_output;
}