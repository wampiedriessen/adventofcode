use intcode::{Intcode, IntcodeProg};
use std::collections::HashMap;
use super::Day;
use super::headings as h;

// ASCII codes
const NEWLINE: i64 = 10;
const COMMA: i64 = 44;
const LEFT: i64 = 76;
const RIGHT: i64 = 82;

pub struct Day17 {
  input: IntcodeProg
}

impl Day for Day17 {
  fn new(input: &str) -> Day17 {
    Day17 {
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

#[derive(PartialEq)]
enum MapItem {
  Space,
  Walkway,
  Intersection
}

struct Robot {
  x: i32,
  y: i32,
  heading: h::Heading,
}

type Map = HashMap<(i32, i32), MapItem>;

// -- Privates
impl Day17 {

  fn update_walkway_status(&self, x: i32, y: i32, map: &mut Map) {
    if x <= 1 || y <= 1 { return; }

    let ways = [map.get(&(x, y-2)), map.get(&(x+1, y-1)), map.get(&(x, y-1)), map.get(&(x-1, y-1)) ];

    if ways.iter().all(|&x| x.map_or(false, |v| *v == MapItem::Walkway) ) {
      map.insert((x, y-1), MapItem::Intersection);
    }
  }

  fn read_map(&self, p: &mut Intcode) -> (Map, Robot) {
    let mut map = HashMap::new();

    p.compute();
    let mut x = 0;
    let mut y = 0;
    let mut robot = Robot {x: 0, y: 0, heading: h::NORTH};
    while p.has_output() {
      match p.stdout() {
        Some(NEWLINE) =>  { x = 0; y += 1; continue; },
        Some(35) =>  { map.insert((x, y), MapItem::Walkway); self.update_walkway_status(x, y, &mut map); },
        Some(46) =>  { map.insert((x, y), MapItem::Space); },
        Some(60) =>  { map.insert((x, y), MapItem::Walkway); robot = Robot {x: x, y: y, heading: h::WEST}; }
        Some(62) =>  { map.insert((x, y), MapItem::Walkway); robot = Robot {x: x, y: y, heading: h::EAST}; }
        Some(118) => { map.insert((x, y), MapItem::Walkway); robot = Robot {x: x, y: y, heading: h::SOUTH}; }
        Some(94) =>  { map.insert((x, y), MapItem::Walkway); robot = Robot {x: x, y: y, heading: h::NORTH}; }
        _ => {},
      };
      x += 1;
    }

    (map, robot)
  }

  fn find_xy_range(&self, map: &Map) -> (i32, i32) {
    let mut x = 0;
    let mut y = 0;
    while map.contains_key(&(x, 0)) {
      x += 1;
    }
    while map.contains_key(&(0, y)) {
      y += 1;
    }

    (x, y)
  }

  fn print_map(&self, map: &Map) {
    let (max_x, max_y) = self.find_xy_range(&map);

    for y in 0..max_y {
      for x in 0..max_x {
        match &map[&(x, y)] {
          MapItem::Space => print!("."),
          MapItem::Walkway => print!("#"),
          MapItem::Intersection => print!("O"),
        }
      }
      print!("\n");
    }
  }

  fn run1(&self) -> i32 {
    let mut p = Intcode::new(&self.input);
    let (map, _) = self.read_map(&mut p);

    self.print_map(&map);

    map.iter().map(|((x, y), v)| {
      if *v == MapItem::Intersection {
        return x * y;
      }
      0
    }).sum()
  }

  fn get_heading_of_adjacent(&self, map: &Map, heading: h::Heading, x: i32, y: i32) -> Option<h::Heading> {
    let opposite_heading = h::reverse_heading(heading).unwrap_or(50);

    let possible_headings = [
      (h::WEST, map.get(&(x-1, y))),
      (h::EAST, map.get(&(x+1, y))),
      (h::NORTH, map.get(&(x, y-1))),
      (h::SOUTH, map.get(&(x, y+1)))
    ];

    let found = possible_headings.iter().find(|&(h, v)| *h != opposite_heading && *v.unwrap_or(&MapItem::Space) != MapItem::Space);
    match found {
      Some((h, _)) => Some(*h),
      None => None,
    }
  }

  fn get_new_heading(&self, old: h::Heading, new: h::Heading) -> i64 {
    match old {
      h::NORTH => match new {
          h::EAST => RIGHT,
          h::WEST => LEFT,
          _ => panic!(),
        },
      h::SOUTH => match new {
          h::EAST => LEFT,
          h::WEST => RIGHT,
          _ => panic!(),
        },
      h::WEST => match new {
          h::NORTH => RIGHT,
          h::SOUTH => LEFT,
          _ => panic!(),
        },
      h::EAST => match new {
          h::NORTH => LEFT,
          h::SOUTH => RIGHT,
          _ => panic!(),
        },
      _ => panic!()
    }
  }

  fn prepare_command(&self, map: &Map, robot: Robot) -> Vec<i64> {
    let mut x = robot.x;
    let mut y = robot.y;
    let mut heading = robot.heading;
    let mut new_heading = self.get_heading_of_adjacent(map, 60, x, y);

    let mut commands = vec![];

    while new_heading.is_some() {
      if heading != new_heading.unwrap() {
        commands.push(self.get_new_heading(heading, new_heading.unwrap()));
        heading = new_heading.unwrap();
      }

      let mut walk = 0;
      loop {
        let (n_x, n_y) = h::update_location(heading, x, y);
        let t = map.get(&(n_x, n_y));
        if t.is_none() || *t.unwrap() == MapItem::Space { break; }
        walk += 1;
        x = n_x;
        y = n_y;
      }
      commands.push(walk);
      new_heading = self.get_heading_of_adjacent(map, heading, x, y);
    }

    commands
  }

  fn zip_commands(&self, commands: Vec<i64>) -> (Vec<i64>, Vec<i64>, Vec<i64>, Vec<i64>) {

    (vec![], vec![], vec![], vec![])
  }

  fn to_ascii_code(&self, command: &mut Vec<i64>) -> Vec<i64> {
    let mut out: Vec<i64> = vec![];
    out.push(command.pop().unwrap());

    while !command.is_empty() {
      out.push(COMMA);
      out.push(command.pop().unwrap());
    }
    out.push(NEWLINE);
    out
  }

  fn run2(&self) -> i64 {
    let mut mapper = Intcode::new(&self.input);
    let (map, robot_details) = self.read_map(&mut mapper);

    let command_string = self.prepare_command(&map, robot_details);

    let mut command_mode = self.input.clone();
    command_mode[0] = 2;
    let mut robot = Intcode::new(&command_mode);

    println!("{:?}", command_string);

    let (mut main, mut def_a, mut def_b, mut def_c) = self.zip_commands(command_string);

    for x in self.to_ascii_code(&mut main) { robot.stdin(x); }
    for x in self.to_ascii_code(&mut def_a) { robot.stdin(x); }
    for x in self.to_ascii_code(&mut def_b) { robot.stdin(x); }
    for x in self.to_ascii_code(&mut def_c) { robot.stdin(x); }

    robot.compute();

    return robot.stdout().unwrap();
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn part1() {
    let day = Day17::new(include_str!("../inputs/day17.txt"));
    assert_eq!(day.run1(), 6680);
  }

  #[test]
  fn part2() {
    let day = Day17::new(include_str!("../inputs/day17.txt"));
    assert_eq!(day.run2(), 0);
  }
}