use super::intcode::{IntcodeProg, Intcode};
use super::Day;

use std::collections::HashMap;
use std::cmp::{min, max};

pub struct Day11 {
  program: IntcodeProg
}

impl Day for Day11 {
  fn new(program: &str) -> Day11 {
    Day11 {
      program: Intcode::read_input(program),
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
impl Day11 {
  fn run1(&self) -> i32 {
    let mut panels: HashMap<(i32,i32), bool> = HashMap::new();

    let mut bot = Intcode::new(&self.program);

    bot.stdin(0);
    bot.compute();

    let mut x = 0;
    let mut y = 0;
    let mut dir = 0;

    let mut painted_panels = 0;

    while let Some(paint) = bot.stdout() {
      let turn = bot.stdout().unwrap();

      if !panels.contains_key(&(x, y)) {
        painted_panels += 1;
      }

      panels.insert((x, y), paint == 1);

      if turn == 0 {
        dir = (dir + 1) % 4;
      } else {
        dir = (dir + 3) % 4;
      }

      match dir {
        0 => y -= 1,
        1 => x -= 1,
        2 => y += 1,
        3 => x += 1,
        _ => panic!("Unknown dir"),
      }

      let newpanel = match panels.get(&(x, y)) {
        Some(x) => if *x { 1 } else { 0 },
        None => 0,
      };

      bot.stdin(newpanel);
      bot.compute();
    }

    return painted_panels;
  }

  fn run2(&self) -> String {
    let mut panels: HashMap<(i32,i32), bool> = HashMap::new();

    let mut bot = Intcode::new(&self.program);

    bot.stdin(1);
    bot.compute();

    let mut min_x = 0;
    let mut min_y = 0;
    let mut max_x = 0;
    let mut max_y = 0;

    let mut x = 0;
    let mut y = 0;
    let mut dir = 0;

    while let Some(paint) = bot.stdout() {
      let turn = bot.stdout().unwrap();

      panels.insert((x, y), paint == 1);

      if turn == 0 {
        dir = (dir + 1) % 4;
      } else {
        dir = (dir + 3) % 4;
      }

      match dir {
        0 => y -= 1,
        1 => x -= 1,
        2 => y += 1,
        3 => x += 1,
        _ => panic!("Unknown dir"),
      }

      min_x = min(x, min_x);
      min_y = min(y, min_y);
      max_x = max(x, max_x);
      max_y = max(y, max_y);

      let newpanel = match panels.get(&(x, y)) {
        Some(x) => if *x { 1 } else { 0 },
        None => 0,
      };

      bot.stdin(newpanel);
      bot.compute();
    }

    let mut identifier: String = String::new();

    for b in min_y..(max_y+1) {
      for a in min_x..(max_x+1) {
        identifier.push(match panels.get(&(a, b)) {
          Some(p) => if *p { '#' } else { ' ' },
          None => ' ',
        });
      }
      identifier.push('\n');
    }

    return identifier;
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn part1() {
    let input = include_str!("../inputs/day11.txt");
    let day = Day11::new(input);
    assert_eq!("2415", format!("{}", day.part1()));
  }
  
  #[test]
  fn part2() {
    let input = include_str!("../inputs/day11.txt");
    let day = Day11::new(input);
    assert_eq!(" ###  #### ###  #  # #### #  # ###   ##    
 #  # #    #  # #  #    # #  # #  # #  #   
 ###  ###  #  # #  #   #  #  # #  # #      
 #  # #    ###  #  #  #   #  # ###  #      
 #  # #    #    #  # #    #  # #    #  #   
 ###  #    #     ##  ####  ##  #     ##    
", format!("{}", day.part2()));
  }
}