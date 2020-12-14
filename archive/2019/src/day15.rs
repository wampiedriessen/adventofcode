use super::intcode::*;
use super::Day;
use super::headings as h;

use std::collections::HashMap;
use std::collections::VecDeque;

const WALL: i64 = 0;
const FLOOR: i64 = 1;
const TARGET: i64 = 2;
const HASBEEN: i64 = 9;

pub struct Day15 {
  input: IntcodeProg
}

impl Day for Day15 {
  fn new(input: &str) -> Day15 {
    Day15 {
      input: Intcode::read_input(input),
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
impl Day15 {
  fn recursive_compute(&self, map: &mut HashMap<(i32, i32), i64>, p: &mut Intcode, x: i32, y: i32) {
    for dir in 1..5 {
      let (new_x, new_y) = h::update_location(dir, x, y);
      if map.contains_key(&(new_x, new_y)) { continue; }

      p.stdin(dir);
      p.compute();
      let tile = p.stdout().unwrap();
      map.insert((new_x, new_y), tile);

      if tile != WALL {
        self.recursive_compute(map, p, new_x, new_y);

        p.stdin(h::reverse_heading(dir).unwrap());
        p.compute();
        let _ = p.stdout();
      }
    }
  }

  fn backtrack(&self, map: &mut HashMap<(i32, i32), i64>, x: i32, y: i32) -> Option<i32> {
    let cur_loc = map.get_mut(&(x, y)).unwrap();
    *cur_loc = HASBEEN;

    for dir in 1..5 {
      let (new_x, new_y) = h::update_location(dir, x, y);

      let next = match map.get(&(new_x, new_y)) {
        Some(&HASBEEN) => continue,
        Some(&TARGET) => Some(0),
        Some(&FLOOR) => self.backtrack(map, new_x, new_y),
        Some(&WALL) => continue,
        _ => panic!("Map should be filled before backtrack"),
      };

      match next {
        None => continue,
        Some(v) => return Some(v+1),
      }
    }
    None
  }

  fn run1(&self) -> i32 {
    let mut p = Intcode::new(&self.input);
    let mut map: HashMap<(i32, i32), i64> = HashMap::new();
    map.insert((0, 0), FLOOR);

    self.recursive_compute(&mut map, &mut p, 0, 0);

    self.backtrack(&mut map, 0, 0).unwrap()
  }

  fn diffuse(&self, map: &mut HashMap<(i32, i32), i64>, stack: &mut VecDeque<(i32, i32, i32)>, seconds: i32, x: i32, y: i32) -> i32 {
    let cur_loc = map.get_mut(&(x, y)).unwrap();
    *cur_loc = HASBEEN;

    for dir in 1..5 {
      let (new_x, new_y) = h::update_location(dir, x, y);

      match map.get(&(new_x, new_y)) {
        Some(&HASBEEN) => continue,
        Some(&TARGET) => stack.push_back((seconds + 1, new_x, new_y)),
        Some(&FLOOR) => stack.push_back((seconds + 1, new_x, new_y)),
        Some(&WALL) => continue,
        _ => panic!("Map should be filled before backtrack"),
      };
    }

    if stack.is_empty() {
      return seconds;
    }
    let (new_s, new_x, new_y) = stack.pop_front().unwrap();
    return self.diffuse(map, stack, new_s, new_x, new_y);
  }

  fn run2(&self) -> i32 {
    let mut p = Intcode::new(&self.input);
    let mut map: HashMap<(i32, i32), i64> = HashMap::new();
    map.insert((0, 0), FLOOR);

    self.recursive_compute(&mut map, &mut p, 0, 0);

    let (&(x, y), _) = map.iter().find(|&(_, v)| *v == TARGET).unwrap();

    self.diffuse(&mut map, &mut VecDeque::new(), 0, x, y)
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn part1() {
    let day = Day15::new(include_str!("../inputs/day15.txt"));
    assert_eq!(day.run1(), 216);
  }

  #[test]
  fn part2() {
    let day = Day15::new(include_str!("../inputs/day15.txt"));
    assert_eq!(day.run2(), 326);
  }
}