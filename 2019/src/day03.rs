use std::collections::HashMap;
use std::cmp::min;

pub fn part1() -> u32 {
    let input = include_str!("../inputs/day03.txt").trim();

    return run1(input);
}

pub fn part2() -> u32 {
    let input = include_str!("../inputs/day03.txt").trim();

    return run2(input);
}

#[derive(PartialEq, Eq, Hash, Copy, Clone)]
struct Point {
    x: i32,
    y: i32,
}

fn get_steps(direction: &str) -> (char, u32) {
    let mut chars = direction.chars();
    let dir: char = chars.next().unwrap();
    let steps: u32 = chars.as_str().parse().unwrap();
    return (dir, steps);
}

fn update_direction(dir: char, steps: u32, pos: Point) -> Point {
    return match dir {
        'R' => Point { x: pos.x + (steps as i32), y: pos.y},
        'L' => Point { x: pos.x - (steps as i32), y: pos.y},
        'U' => Point { x: pos.x, y: pos.y + (steps as i32)},
        'D' => Point { x: pos.x, y: pos.y - (steps as i32)},
        _ => panic!("Unknown direction"),
    }
}

fn get_collisions(input:&str) -> HashMap<Point, (u32, u32)> {
    let mut lines = input.lines();

    let mut points_line_1: HashMap<Point, u32> = HashMap::new();
    let mut collisions: HashMap<Point, (u32, u32)> = HashMap::new();

    let mut cur_pos: Point = Point { x: 0, y: 0 };
    let mut steps_taken = 0;
    for direction in lines.next().unwrap().split(',') {
        let (dir, steps) = get_steps(direction);

        for _ in 0..steps {
            steps_taken += 1;
            cur_pos = update_direction(dir, 1, cur_pos);

            if !points_line_1.contains_key(&cur_pos) {
                points_line_1.insert(cur_pos, steps_taken);
            }
        }
    }

    steps_taken = 0;
    cur_pos.x = 0;
    cur_pos.y = 0;
    for direction in lines.next().unwrap().split(',') {
        let (dir, steps) = get_steps(direction);

        for _ in 0..steps {
            steps_taken += 1;
            cur_pos = update_direction(dir, 1, cur_pos);

            if points_line_1.contains_key(&cur_pos) {
                let steps_line_1 = points_line_1.get(&cur_pos).unwrap();
                if !collisions.contains_key(&cur_pos) {
                    collisions.insert(cur_pos, (steps_taken, *steps_line_1));
                }
            }
        }
    }
    return collisions;
}

fn run1(input:&str) -> u32 {
    let mut lowest_manhattan: u32 = 99999999;

    let collisions = get_collisions(input);

    for (pos, _)  in &collisions {
        lowest_manhattan = min(lowest_manhattan, (pos.x.abs() + pos.y.abs()) as u32);
    }
    
    return lowest_manhattan;
}

fn run2(input:&str) -> u32 {
    let mut lowest_steps: u32 = 99999999;
    let collisions = get_collisions(input);

    for (_, (a, b)) in &collisions {
        lowest_steps = min(lowest_steps, a + b);
    }
    
    return lowest_steps;
}


#[cfg(test)]
mod tests {
  use super::*;

  const EXAMPLE1: &str = "R8,U5,L5,D3
U7,R6,D4,L4
";

  const EXAMPLE2: &str = "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83
";

  const EXAMPLE3: &str = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7
";

  #[test]
  fn part1_sample_test() {
    assert_eq!(6, run1(EXAMPLE1));
    assert_eq!(159, run1(EXAMPLE2));
    assert_eq!(135, run1(EXAMPLE3));
  }

  #[test]
  fn part2_sample_test() {
    assert_eq!(30, run2(EXAMPLE1));
    assert_eq!(610, run2(EXAMPLE2));
    assert_eq!(410, run2(EXAMPLE3));
  }

  #[test]
  fn part1_test() {
    assert_eq!(1285, part1());
  }

  #[test]
  fn part2_test() {
    assert_eq!(14228, part2());
  }
}