use super::Day;
use super::util::Point;
use std::collections::HashMap;
use std::cmp::min;

pub struct Day03 {
  input_line_a: Vec<String>,
  input_line_b: Vec<String>,
}

impl Day for Day03 {
  fn new(input: &str) -> Day03 {
    let lines: Vec<_> = input.lines().collect();
    return Day03 {
        input_line_a: lines[0].split(',').map(|s| String::from(s)).collect(),
        input_line_b: lines[1].split(',').map(|s| String::from(s)).collect(),
    }
  }

  fn part1(&self) -> Box<dyn std::fmt::Display> {
    return Box::new(self.run1());
  }

  fn part2(&self) -> Box<dyn std::fmt::Display> {
    return Box::new(self.run2());
  }
}

// -- Privates
impl Day03 {
    fn get_steps(&self, direction: &String) -> (char, u32) {
        let mut chars = direction.chars();
        let dir: char = chars.next().unwrap();
        let steps: u32 = chars.as_str().parse().unwrap();
        return (dir, steps);
    }

    fn update_direction(&self, dir: char, steps: u32, pos: Point) -> Point {
        return match dir {
            'R' => Point { x: pos.x + (steps as i32), y: pos.y},
            'L' => Point { x: pos.x - (steps as i32), y: pos.y},
            'U' => Point { x: pos.x, y: pos.y + (steps as i32)},
            'D' => Point { x: pos.x, y: pos.y - (steps as i32)},
            _ => panic!("Unknown direction"),
        }
    }

    fn get_collisions(&self) -> HashMap<Point, (u32, u32)> {
        let mut points_line_1: HashMap<Point, u32> = HashMap::new();
        let mut collisions: HashMap<Point, (u32, u32)> = HashMap::new();

        let mut cur_pos: Point = Point { x: 0, y: 0 };
        let mut steps_taken = 0;
        for direction in &self.input_line_a {
            let (dir, steps) = self.get_steps(&direction);

            for _ in 0..steps {
                steps_taken += 1;
                cur_pos = self.update_direction(dir, 1, cur_pos);

                if !points_line_1.contains_key(&cur_pos) {
                    points_line_1.insert(cur_pos, steps_taken);
                }
            }
        }

        steps_taken = 0;
        cur_pos.x = 0;
        cur_pos.y = 0;
        for direction in &self.input_line_b {
            let (dir, steps) = self.get_steps(&direction);

            for _ in 0..steps {
                steps_taken += 1;
                cur_pos = self.update_direction(dir, 1, cur_pos);

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

    fn run1(&self) -> u32 {
        let mut lowest_manhattan: u32 = 99999999;

        let collisions = self.get_collisions();

        for (pos, _)  in &collisions {
            lowest_manhattan = min(lowest_manhattan, (pos.x.abs() + pos.y.abs()) as u32);
        }
        
        return lowest_manhattan;
    }

    fn run2(&self) -> u32 {
        let mut lowest_steps: u32 = 99999999;
        let collisions = self.get_collisions();

        for (_, (a, b)) in &collisions {
            lowest_steps = min(lowest_steps, a + b);
        }
        
        return lowest_steps;
    }
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
    let day1 = Day03::new(EXAMPLE1);
    assert_eq!(6, day1.run1());

    let day2 = Day03::new(EXAMPLE2);
    assert_eq!(159, day2.run1());

    let day3 = Day03::new(EXAMPLE3);
    assert_eq!(135, day3.run1());
  }

  #[test]
  fn part2_sample_test() {
    let day1 = Day03::new(EXAMPLE1);
    assert_eq!(30, day1.run2());

    let day2 = Day03::new(EXAMPLE2);
    assert_eq!(610, day2.run2());

    let day3 = Day03::new(EXAMPLE3);
    assert_eq!(410, day3.run2());
  }

  #[test]
  fn part1_test() {
    let input = include_str!("../inputs/day03.txt").trim();
    let day = Day03::new(input);

    assert_eq!("1285", format!("{}", day.part1()));
  }

  #[test]
  fn part2_test() {
    let input = include_str!("../inputs/day03.txt").trim();
    let day = Day03::new(input);

    assert_eq!("14228", format!("{}", day.part2()));
  }
}