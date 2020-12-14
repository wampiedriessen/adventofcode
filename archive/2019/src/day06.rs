use std::collections::HashSet;
use std::collections::HashMap;
use super::Day;

pub struct Day06 {
  map: HashMap<String, Vec<String>>
}

impl Day for Day06 {
  fn new(input: &str) -> Day06 {
    let mut day = Day06 { map: HashMap::new() };

    day.parse_map(input);

    return day;
  }

  fn part1(&self) -> Box<dyn std::fmt::Display> {
    return Box::new(self.run1());
  }

  fn part2(&self) -> Box<dyn std::fmt::Display> {
    return Box::new(self.run2());
  }
}

// -- Privates
impl Day06 {
  fn parse_map(&mut self, input: &str) {
    for line in input.lines() {
      let s = line.split(")").map(|x| String::from(x)).collect::<Vec<String>>();
      if self.map.contains_key(&s[0]) {
        self.map.get_mut(&s[0]).unwrap().push(s[1].clone());
      } else {
        self.map.insert(s[0].clone(), vec![s[1].clone()]);
      }
    }
  }

  fn count_orbits(&self, object: &String, depth: u32) -> u32 {
    if !self.map.contains_key(object) {
      return 0;
    }

    let mut sum_orbits = 0;
    for orbit in self.map.get(object).unwrap() {
      sum_orbits += depth;
      sum_orbits += self.count_orbits(orbit, depth+1);
    }

    return sum_orbits;
  }

  fn run1(&self) -> u32 {
    return self.count_orbits(&String::from("COM"), 1);
  }

  fn flatten_route(&self, start: &String, location: &String) -> Option<HashSet<String>> {
    if start == location {
      return Some(HashSet::new());
    }

    if !self.map.contains_key(start) {
      return None;
    }

    for orbit in self.map.get(start).unwrap() {
      if let Some(mut route) = self.flatten_route(orbit, location) {
        route.insert(start.clone());
        return Some(route);
      }
    }

    return None;
  }

  fn run2(&self) -> u32 {
    let start = String::from("COM");
    let route_to_santa = self.flatten_route(&start, &String::from("SAN")).unwrap();
    let route_to_me = self.flatten_route(&start, &String::from("YOU")).unwrap();

    let objects_not_in_common: Vec<&String> = route_to_me
      .symmetric_difference(&route_to_santa)
      .into_iter()
      .collect();

    return objects_not_in_common.len() as u32;
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  const EXAMPLE1: &str = "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L";

  const EXAMPLE2: &str = "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN";

  #[test]
  fn part1_sample_test()
  {
    let d = Day06::new(EXAMPLE1);

    assert_eq!(42, d.run1())
  }

  #[test]
  fn part2_sample_test()
  {
    let d = Day06::new(EXAMPLE2);

    assert_eq!(4, d.run2())
  }

  #[test]
  fn part1_answer()
  {
    let input = include_str!("../inputs/day06.txt").trim();
    let day = Day06::new(input);

    assert_eq!("270768", format!("{}", day.part1()));
  }

  #[test]
  fn part2_answer()
  {
    let input = include_str!("../inputs/day06.txt").trim();
    let day = Day06::new(input);

    assert_eq!("451", format!("{}", day.part2()));
  }
  
}