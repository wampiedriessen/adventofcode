use super::intcode::*;
use super::Day;

pub struct Day13 {
  input: IntcodeProg,
}

impl Day for Day13 {
  fn new(input: &str) -> Day13 {
    Day13 {
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

#[derive(Clone)]
enum Tile {
  Empty,
  Wall,
  Block,
  Paddle,
  Ball
}

// -- Privates
impl Day13 {
  fn run1(&self) -> i32 {
    let mut p = Intcode::new(&self.input);
    let mut grid: Vec<Vec<Tile>> = Vec::new();

    p.compute();

    while let Some(x1) = p.stdout() {
      let x = x1 as usize;
      let y = p.stdout().unwrap() as usize;
      let newtype = p.stdout().unwrap();

      if y >= grid.len() {
        grid.resize(y+1, Vec::new());
      }
      if x >= grid[y].len() {
        grid[y].resize(x+1, Tile::Empty);
      }
      grid[y][x] = match newtype {
        0 => Tile::Empty,
        1 => Tile::Wall,
        2 => Tile::Block,
        3 => Tile::Paddle,
        4 => Tile::Ball,
        _ => panic!("Unknown tile type!")
      }
    }

    return grid.iter().fold(0, |acc, l|
      return acc + l.iter().fold(0, |acc2, b|
        return match b {
          Tile::Block => acc2+1,
          _ => acc2
        }));
  }

  fn run2(&self) -> i64 {
    let mut hacked_input = self.input.clone();
    hacked_input[0] = 2;
    let mut p = Intcode::new(&hacked_input);

    p.stdin(0);
    p.compute();

    let mut score = 0;

    let mut paddle_x = 99;
    let mut ball_x = 99;

    while !p.is_finished() || p.has_output() {

      while p.has_output() {
        let x = p.stdout().unwrap();
        let y = p.stdout().unwrap();
        if x == -1 && y == 0 {
          score = p.stdout().unwrap();
        } else {
          match p.stdout().unwrap() {
            0 => continue,
            1 => continue,
            2 => continue,
            3 => {
              paddle_x = x;
            },
            4 => {
              ball_x = x;
            },
            _ => panic!("Unknown tile type!")
          }
        }
      }
      
      if p.is_waiting() {
        match paddle_x.cmp(&ball_x) {
          std::cmp::Ordering::Equal => p.stdin(0),
          std::cmp::Ordering::Less => p.stdin(1),
          std::cmp::Ordering::Greater => p.stdin(-1),
        }
        p.compute();
      }
    }

    return score;
  }
}