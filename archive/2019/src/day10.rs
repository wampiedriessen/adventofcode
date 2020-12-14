use super::util::Point;
use super::Day;

use std::collections::VecDeque;

pub struct Day10 {
  grid: Vec<Vec<bool>>,
  asteroids: Vec<Point>,
  height: usize,
  width: usize,
}

impl Day for Day10 {
  fn new(input: &str) -> Day10 {
    let lines: Vec<&str> = input.lines().collect();

    let linelen = lines[0].len();
    let mut asteroids: Vec<Point> = Vec::new();

    for y in 0..lines.len() {
      let chars: Vec<char> = lines[y].chars().collect();
      for x in 0..linelen {
        if chars[x] == '#' {
          asteroids.push(Point { y: y as i32, x: x as i32 });
        }
      }
    }
    
    Day10 {
      grid: lines.iter().map(|l| l.chars().map(|c| c == '#').collect::<Vec<bool>>()).collect::<Vec<Vec<bool>>>(),
      asteroids,
      height: lines.len(),
      width: linelen,
    }
  }

  fn part1(&self) -> Box<dyn std::fmt::Display> {
    return Box::new(self.run1());
  }

  fn part2(&self) -> Box<dyn std::fmt::Display> {
    return Box::new(self.run2());
  }
}

fn gcd(a: u8, b: u8) -> u8 {
  if b == 0 { panic!("Division by zero!"); }
  if a % b == 0 {
    return b;
  }
  return gcd(b, a % b);
}

fn base_vector(x: i32, y: i32) -> Point {
  if x == 0 {
    return Point {x, y: y / y.abs()};
  }
  if y == 0 {
    return Point {x: x / x.abs(), y};
  }
  let cd = gcd(x.abs() as u8, y.abs() as u8) as i32;
  return Point {x: x / cd, y: y / cd};
}

// -- Privates
impl Day10 {
  fn is_visible(&self, a: &Point, b: &Point) -> bool {
    if a.x == b.x && a.y == b.y { return false; }

    let v = base_vector(b.x - a.x, b.y - a.y);

    let mut i = 1;

    while ! (a.x + (i*v.x) == b.x && a.y + (i*v.y) == b.y) {
      let x = a.x + (i*v.x);
      let y = a.y + (i*v.y);

      if x < 0 || y < 0 || x >= self.width as i32 || y >= self.height as i32 {
        break;
      }
      if self.grid[y as usize][x as usize] {
        return false;
      }
      i += 1;
    }

    return true;
  }
  
  fn get_best_asteroid(&self) -> (usize, u32) {
    let mut most_seen: u32 = 0;
    let mut most_index: usize = 0;
    let mut i = 0;

    let mut seen:Vec<u32> = vec![0; self.asteroids.len()];

    while i < self.asteroids.len() {
      let mut j = i + 1;
      for other in &self.asteroids[j .. ] {
        if self.is_visible(&self.asteroids[i], other) {
          seen[i] += 1;
          seen[j] += 1;
        } else {
        }
        j += 1;
      }
      if most_seen < seen[i] {
        most_seen = seen[i];
        most_index = i;
      }
      i += 1;
    }

    return (most_index, most_seen);
  }

  fn run1(&self) -> u32 {
    let (_, seen) = self.get_best_asteroid();

    return seen;
  }

  fn sort_asteroids_in_destroy_order(&self, center: &Point) -> Vec<(Point, f64, i32)> {
    let mut r_poss:Vec<(Point, f64, i32)> = self.asteroids.iter().map(|p| {
      let x = p.x - center.x;
      let y = p.y - center.y;
      let angle = -1.0 * (x as f64).atan2(y as f64);
      (*p, angle, x.abs() + y.abs())
    }).collect();

    r_poss.sort_by(|(_, _, a), (_, _, b)| a.partial_cmp(b).unwrap() );
    r_poss.sort_by(|(_, a, _), (_, b, _)| a.partial_cmp(b).unwrap() );

    return r_poss;
  }

  fn run2(&self) -> i32 {
    let (index, _) = self.get_best_asteroid();
    let center = self.asteroids[index];

    let mut todo_destroy_asteroids = VecDeque::from(self.sort_asteroids_in_destroy_order(&center));

    let mut last_angle: f64 = -4.0; // minder dan -1*Pi
    let mut i = 1;
    while let Some((asteroid,angle,d)) = todo_destroy_asteroids.pop_front() {
      if angle == last_angle {
        todo_destroy_asteroids.push_back((asteroid,angle,d));
        continue;
      }
      
      if i == 200 {
        return (100 * asteroid.x) + asteroid.y;
      }

      last_angle = angle;
      i += 1;
    }

    panic!("Less than 200 Asteroids found!");
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn base_vector_test()
  {
    let p1 = base_vector((1_i32-4).abs(), (0_i32-3).abs());
    assert_eq!(1, p1.x);
    assert_eq!(1, p1.y);

    let p2 = base_vector(3, 0);
    assert_eq!(1, p2.x);
    assert_eq!(0, p2.y);

    let p2 = base_vector(0, 5);
    assert_eq!(0, p2.x);
    assert_eq!(1, p2.y);

    let p2 = base_vector(-5, 0);
    assert_eq!(-1, p2.x);
    assert_eq!(0, p2.y);

    let p2 = base_vector(0, -3);
    assert_eq!(0, p2.x);
    assert_eq!(-1, p2.y);
  }

  #[test]
  fn is_visible_horizontal_test()
  {
    let d = Day10::new("###");

    let a = Point {x: 0, y: 0};
    let b = Point {x: 1, y: 0};
    let c = Point {x: 2, y: 0};
    assert_eq!(false, d.is_visible(&a, &c));
    assert_eq!(false, d.is_visible(&c, &a));

    assert_eq!(true, d.is_visible(&a, &b));
    assert_eq!(true, d.is_visible(&b, &a));
    assert_eq!(true, d.is_visible(&b, &c));
    assert_eq!(true, d.is_visible(&c, &b));
  }

  #[test]
  fn is_visible_vertical_test()
  {
    let d = Day10::new("#\n#\n#");

    let a = Point {x: 0, y: 0};
    let b = Point {x: 0, y: 1};
    let c = Point {x: 0, y: 2};
    assert_eq!(false, d.is_visible(&a, &c));
    assert_eq!(false, d.is_visible(&c, &a));

    assert_eq!(true, d.is_visible(&a, &b));
    assert_eq!(true, d.is_visible(&b, &a));
    assert_eq!(true, d.is_visible(&b, &c));
    assert_eq!(true, d.is_visible(&c, &b));
  }

  #[test]
  fn is_visible_cross_test()
  {
    let day = Day10::new("# #\n # \n###");

    let a = Point {x: 0, y: 0};
    let b = Point {x: 2, y: 0};
    // let c = Point {x: 1, y: 1};
    let d = Point {x: 0, y: 2};
    let e = Point {x: 1, y: 2};
    let f = Point {x: 2, y: 2};
    assert_eq!(false, day.is_visible(&a, &f));
    assert_eq!(false, day.is_visible(&f, &a));

    assert_eq!(false, day.is_visible(&b, &d));
    assert_eq!(false, day.is_visible(&d, &b));

    assert_eq!(true, day.is_visible(&a, &e));
    assert_eq!(true, day.is_visible(&e, &a));

    assert_eq!(true, day.is_visible(&b, &e));
    assert_eq!(true, day.is_visible(&e, &b));
  }

  fn best_asteroid_location(day: &Day10) -> String {
    let (index, _) = day.get_best_asteroid();
    let best_asteroid = day.asteroids[index];
    return format!("{},{}", best_asteroid.x, best_asteroid.y);
  }

  const EXAMPLE1: &str = ".#..#
.....
#####
....#
...##";

  #[test]
  fn part1_example1_location()
  {
    let day = Day10::new(EXAMPLE1);

    assert_eq!("3,4", best_asteroid_location(&day));
    assert_eq!(8, day.run1());
  }

  const EXAMPLE2: &str = "......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####";

  #[test]
  fn part1_example2_location()
  {
    let day = Day10::new(EXAMPLE2);

    assert_eq!("5,8", best_asteroid_location(&day));
    assert_eq!(33, day.run1());
  }

  const EXAMPLE3: &str = "#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.";

  #[test]
  fn part1_example3_location()
  {
    let day = Day10::new(EXAMPLE3);

    assert_eq!("1,2", best_asteroid_location(&day));
    assert_eq!(35, day.run1());
  }

  const EXAMPLE4: &str = ".#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..";

  #[test]
  fn part1_example4_location()
  {
    let day = Day10::new(EXAMPLE4);

    assert_eq!("6,3", best_asteroid_location(&day));
    assert_eq!(41, day.run1());
  }

  const EXAMPLE5: &str = ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##";

  #[test]
  fn part1_example5_location()
  {
    let day = Day10::new(EXAMPLE5);

    assert_eq!("11,13", best_asteroid_location(&day));
    assert_eq!(210, day.run1());
  }

  #[test]
  fn part2_example5()
  {
    let day = Day10::new(EXAMPLE5);

    assert_eq!(802, day.run2());
  }

  #[test]
  fn part1()
  {
    let day = Day10::new(include_str!("../inputs/day10.txt"));

    assert_eq!("278", format!("{}", day.part1()));
  }
  
  #[test]
  fn part2()
  {
    let day = Day10::new(include_str!("../inputs/day10.txt"));

    assert_eq!("1417", format!("{}", day.part2()));
  }
}