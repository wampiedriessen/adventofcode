use super::headings as h;
use super::Day;
use intcode::{Intcode, IntcodeProg};
use std::collections::HashMap;

// ASCII codes
const NEWLINE: i64 = 10;
const COMMA: i64 = 44;
const LEFT: i64 = 76;
const RIGHT: i64 = 82;

const ASCII_A: i64 = 65;
const ASCII_0: i64 = 48;

const ASCII_PADDING: i64 = 200;

pub struct Day17 {
  input: IntcodeProg,
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
  Intersection,
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
    if x <= 1 || y <= 1 {
      return;
    }

    let ways = [
      map.get(&(x, y - 2)),
      map.get(&(x + 1, y - 1)),
      map.get(&(x, y - 1)),
      map.get(&(x - 1, y - 1)),
    ];

    if ways
      .iter()
      .all(|&x| x.map_or(false, |v| *v == MapItem::Walkway))
    {
      map.insert((x, y - 1), MapItem::Intersection);
    }
  }

  fn read_map(&self, p: &mut Intcode) -> (Map, Robot) {
    let mut map = HashMap::new();

    let mut x = 0;
    let mut y = 0;
    let mut robot = Robot {
      x: 0,
      y: 0,
      heading: h::NORTH,
    };
    while p.has_output() {
      match p.stdout() {
        Some(NEWLINE) => {
          x = 0;
          y += 1;
          continue;
        }
        Some(35) => {
          map.insert((x, y), MapItem::Walkway);
          self.update_walkway_status(x, y, &mut map);
        }
        Some(46) => {
          map.insert((x, y), MapItem::Space);
        }
        Some(60) => {
          map.insert((x, y), MapItem::Walkway);
          robot = Robot {
            x: x,
            y: y,
            heading: h::WEST,
          };
        }
        Some(62) => {
          map.insert((x, y), MapItem::Walkway);
          robot = Robot {
            x: x,
            y: y,
            heading: h::EAST,
          };
        }
        Some(118) => {
          map.insert((x, y), MapItem::Walkway);
          robot = Robot {
            x: x,
            y: y,
            heading: h::SOUTH,
          };
        }
        Some(94) => {
          map.insert((x, y), MapItem::Walkway);
          robot = Robot {
            x: x,
            y: y,
            heading: h::NORTH,
          };
        }
        _ => {}
      };
      x += 1;
    }

    (map, robot)
  }

  // fn find_xy_range(&self, map: &Map) -> (i32, i32) {
  //   let mut x = 0;
  //   let mut y = 0;
  //   while map.contains_key(&(x, 0)) {
  //     x += 1;
  //   }
  //   while map.contains_key(&(0, y)) {
  //     y += 1;
  //   }

  //   (x, y)
  // }

  // fn print_map(&self, map: &Map) {
  //   let (max_x, max_y) = self.find_xy_range(&map);

  //   for y in 0..max_y {
  //     for x in 0..max_x {
  //       match &map[&(x, y)] {
  //         MapItem::Space => print!("."),
  //         MapItem::Walkway => print!("#"),
  //         MapItem::Intersection => print!("O"),
  //       }
  //     }
  //     print!("\n");
  //   }
  // }

  fn run1(&self) -> i32 {
    let mut p = Intcode::new(&self.input);
    p.compute();
    let (map, _) = self.read_map(&mut p);

    map
      .iter()
      .map(|((x, y), v)| {
        if *v == MapItem::Intersection {
          return x * y;
        }
        0
      })
      .sum()
  }

  fn get_heading_of_adjacent(
    &self,
    map: &Map,
    heading: h::Heading,
    x: i32,
    y: i32,
  ) -> Option<h::Heading> {
    let opposite_heading = h::reverse_heading(heading).unwrap_or(50);

    let possible_headings = [
      (h::WEST, map.get(&(x - 1, y))),
      (h::EAST, map.get(&(x + 1, y))),
      (h::NORTH, map.get(&(x, y - 1))),
      (h::SOUTH, map.get(&(x, y + 1))),
    ];

    let found = possible_headings
      .iter()
      .find(|&(h, v)| *h != opposite_heading && *v.unwrap_or(&MapItem::Space) != MapItem::Space);
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
      _ => panic!(),
    }
  }

  fn get_route_commands(&self, map: &Map, robot: Robot) -> Vec<i64> {
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
        if t.is_none() || *t.unwrap() == MapItem::Space {
          break;
        }
        walk += 1;
        x = n_x;
        y = n_y;
      }
      commands.push(walk + ASCII_PADDING);
      new_heading = self.get_heading_of_adjacent(map, heading, x, y);
    }

    commands
  }

  fn find_subsequence(&self, haystack: &[i64], needle: &[i64]) -> Option<usize> {
    haystack
      .windows(needle.len())
      .position(|window| window == needle)
  }

  fn remove_word_from_parts<'a>(&self, word: &[i64], commands: &Vec<&'a [i64]>) -> Vec<&'a [i64]> {
    let mut newcommands: Vec<&[i64]> = vec![];

    for cmd in commands {
      let mut seen = 0;
      while let Some(i) = self.find_subsequence(&cmd[seen..cmd.len()], word) {
        if i != 0 {
          newcommands.push(&cmd[seen..(seen + i)]);
          seen += i;
        }
        seen += word.len();
      }
      if seen != cmd.len() {
        newcommands.push(&cmd[seen..cmd.len()]);
      }
    }

    newcommands
  }

  fn find_movement_functions<'a>(
    &self,
    commands: &Vec<&'a [i64]>,
    to_find: u8,
  ) -> Option<Vec<&'a [i64]>> {
    if to_find == 0 {
      if commands.iter().any(|v| !v.is_empty()) {
        return None;
      }
      return Some(vec![]);
    }
    for wlen in (2..12).step_by(2).rev() {
      if wlen > commands[0].len() {
        continue;
      }
      let word: &[i64] = &commands[0][(0..wlen)];

      let newparts = self.remove_word_from_parts(&word[..], commands);
      match self.find_movement_functions(&newparts, to_find - 1) {
        None => continue,
        Some(mut ans) => {
          ans.push(word);
          return Some(ans);
        }
      }
    }

    None
  }

  fn zip_commands(&self, commands: &[i64]) -> (Vec<i64>, Vec<i64>, Vec<i64>, Vec<i64>) {
    let movement_functions = self.find_movement_functions(&vec![commands], 3).unwrap();

    let mut main = vec![];
    let mut skip = 0;
    while skip != commands.len() {
      for (i, &fun) in movement_functions.iter().enumerate() {
        if skip + fun.len() > commands.len() {
          continue;
        }
        if &commands[(skip..(skip + fun.len()))] == fun {
          skip += fun.len();
          main.push(ASCII_A + i as i64);
          break;
        }
      }
    }

    let a = movement_functions[0].iter().map(|v| *v).collect();
    let b = movement_functions[1].iter().map(|v| *v).collect();
    let c = movement_functions[2].iter().map(|v| *v).collect();
    (main, a, b, c)
  }

  fn de_pad_ascii(&self, val: i64, out: &mut Vec<i64>) {
    if val < ASCII_PADDING {
      out.push(val);
      return;
    }

    let actual = val - ASCII_PADDING;
    if actual >= 10 {
      out.push(ASCII_0 + (actual / 10));
    }
    out.push(ASCII_0 + (actual % 10));
  }

  fn to_ascii_code(&self, command: &Vec<i64>) -> Vec<i64> {
    let mut out: Vec<i64> = vec![];
    let mut code = command.clone();
    code.reverse();

    self.de_pad_ascii(code.pop().unwrap(), &mut out);

    while !code.is_empty() {
      out.push(COMMA);
      self.de_pad_ascii(code.pop().unwrap(), &mut out);
    }
    out.push(NEWLINE);
    out
  }

  // fn show_video_feed(&self, robot: &mut Intcode, lines: i32) {
  //   while robot.has_output() {
  //     let mut y = 0;
  //     while robot.has_output() && y < lines {
  //       let x = robot.stdout().unwrap();
  //       print!("{}", std::char::from_u32(x as u32).unwrap());
  //       if x == NEWLINE {
  //         y += 1;
  //       }
  //     }
  //   }
  // }

  fn run2(&self) -> i64 {
    let mut command_mode = self.input.clone();
    command_mode[0] = 2;
    let mut robot = Intcode::new(&command_mode);
    robot.compute();
    let (map, robot_details) = self.read_map(&mut robot);

    let command_string = self.get_route_commands(&map, robot_details);

    let (main, def_a, def_b, def_c) = self.zip_commands(&command_string);

    for x in self.to_ascii_code(&main) {
      robot.stdin(x);
    }
    for x in self.to_ascii_code(&def_a) {
      robot.stdin(x);
    }
    for x in self.to_ascii_code(&def_b) {
      robot.stdin(x);
    }
    for x in self.to_ascii_code(&def_c) {
      robot.stdin(x);
    }

    robot.stdin(110);
    robot.stdin(NEWLINE);
    robot.compute();

    let mut lastval = 0;
    while robot.has_output() {
      lastval = robot.stdout().unwrap();
    }

    lastval
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
    assert_eq!(day.run2(), 1103905);
  }
}
