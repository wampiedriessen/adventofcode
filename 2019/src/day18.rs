use super::Day;
use std::collections::{HashMap, HashSet, VecDeque};

type Location = (u32, u32);
type Map = HashMap<Location, char>;

pub struct Day18 {
  map: Map,
}

fn read_map(input: &str) -> Map {
  let mut x;
  let mut y = 0;
  let mut map = Map::new();

  for line in input.lines() {
    x = 0;
    for car in line.chars() {
      match car {
        v => {
          map.insert((x, y), v);
        }
      }
      x += 1;
    }

    y += 1;
  }

  map
}

impl Day for Day18 {
  fn new(input: &str) -> Day18 {
    let map = read_map(input);
    Day18 { map: map }
  }

  fn part1(&self) -> Box<dyn std::fmt::Display> {
    return Box::new(self.run1());
  }

  fn part2(&self) -> Box<dyn std::fmt::Display> {
    return Box::new(self.run2());
  }
}

// -- Privates
impl Day18 {
  fn find_neighbours(&self, location: Location) -> HashMap<char, u32> {
    let mut subgraph = HashMap::new();
    let mut todo = VecDeque::new();
    let mut visited = HashSet::new();
    todo.push_back((0, location));

    while let Some((steps, (x, y))) = todo.pop_front() {
      visited.insert((x, y));
      for neighbour in &[(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)] {
        if visited.contains(neighbour) {
          continue;
        }
        if let Some(&item) = self.map.get(neighbour) {
          if item == '#' {
            continue;
          }
          if item == '.' {
            todo.push_back((steps + 1, *neighbour))
          } else {
            subgraph.insert(item, steps + 1);
          }
        }
      }
    }
    subgraph
  }

  fn create_graph(&self) -> HashMap<char, HashMap<char, u32>> {
    let mut graph = HashMap::new();
    for (&loc, &item) in self.map.iter() {
      if item != '#' && item != '.' {
        graph.insert(item, self.find_neighbours(loc));
      }
    }
    graph
  }

  fn run1(&self) -> u32 {
    let graph = self.create_graph();
    println!("{:?}", graph);

    // TODO:
    0
  }

  fn run2(&self) -> i32 {
    return 0;
  }
}

#[cfg(test)]
mod test {
  use super::*;

  const EXAMPLE1: &str = "#########
#b.A.@.a#
#########
";
  const EXAMPLE2: &str = "########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################
";
  const EXAMPLE3: &str = "########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################
";
  const EXAMPLE4: &str = "#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################
";
  const EXAMPLE5: &str = "########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################
";

  #[test]
  fn part1_example1() {
    let day = Day18::new(EXAMPLE1);
    assert_eq!(day.run1(), 8);
  }

  #[test]
  fn part1_example2() {
    let day = Day18::new(EXAMPLE2);
    assert_eq!(day.run1(), 86);
  }

  #[test]
  fn part1_example3() {
    let day = Day18::new(EXAMPLE3);
    assert_eq!(day.run1(), 132);
  }

  #[test]
  fn part1_example4() {
    let day = Day18::new(EXAMPLE4);
    assert_eq!(day.run1(), 136);
  }

  #[test]
  fn part1_example5() {
    let day = Day18::new(EXAMPLE5);
    assert_eq!(day.run1(), 81);
  }

  #[test]
  fn part1() {
    let day = Day18::new(include_str!("../inputs/day18.txt"));
    assert_eq!(day.run1(), 0);
  }

  #[test]
  fn part2() {
    let day = Day18::new(include_str!("../inputs/day18.txt"));
    assert_eq!(day.run2(), 0);
  }
}
