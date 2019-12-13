extern crate regex;

use self::regex::{Regex, Captures};
use super::Day;

pub struct Day12 {
  moons: [[i32; 3]; 4],
}

fn input_captures(caps: Captures) -> [i32; 3] {
  return [
    caps["x"].parse().unwrap(),
    caps["y"].parse().unwrap(),
    caps["z"].parse().unwrap(),
  ];
}

impl Day for Day12 {
  fn new(input: &str) -> Day12 {
    let re = Regex::new(r"<x=(?P<x>.+), y=(?P<y>.+), z=(?P<z>.+)>").unwrap();

    let lines = input.lines().collect::<Vec<_>>();
    Day12 {
      moons: [
        input_captures(re.captures(lines[0]).unwrap()),
        input_captures(re.captures(lines[1]).unwrap()),
        input_captures(re.captures(lines[2]).unwrap()),
        input_captures(re.captures(lines[3]).unwrap()),
      ]
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
impl Day12 {
  fn cmp(&self, a: i32, b: i32) -> i32 {
    return match a.cmp(&b) {
      std::cmp::Ordering::Equal => 0,
      std::cmp::Ordering::Less => 1,
      std::cmp::Ordering::Greater => -1,
    };
  }

  fn apply_gravity(&self, pos: &[[i32; 3]; 4], vel: &mut [[i32; 3]; 4]) -> () {
    for i in 0..4 {
      for j in 0..4 {
        vel[i][0] += self.cmp(pos[i][0], pos[j][0]);
        vel[i][1] += self.cmp(pos[i][1], pos[j][1]);
        vel[i][2] += self.cmp(pos[i][2], pos[j][2]);
      }
    }
  }

  fn apply_velocity(&self, pos: &mut [[i32; 3]; 4], vel: &[[i32; 3]; 4]) -> () {
    for i in 0..4 {
      pos[i][0] += vel[i][0];
      pos[i][1] += vel[i][1];
      pos[i][2] += vel[i][2];
    }
  }

  fn moons_after_steps(&self, steps: u32) -> ([[i32; 3]; 4], [[i32; 3]; 4]) {
    let mut positions = self.moons.clone();
    let mut velocities = [[0; 3]; 4];

    for _ in 0..steps {
      self.apply_gravity(&positions, &mut velocities);
      self.apply_velocity(&mut positions, &velocities);
    }

    return (positions, velocities);
  }

  fn get_energy(&self, pos: &[[i32; 3]; 4], vel: &[[i32; 3]; 4]) -> i32 {
    let mut energy = 0;
    for i in 0..4 {
      let pot = pos[i][0].abs() + pos[i][1].abs() + pos[i][2].abs();
      let kin = vel[i][0].abs() + vel[i][1].abs() + vel[i][2].abs();

      energy += pot * kin;
    }

    return energy;
  }

  fn steps_to_repeat_state(&self) -> u64{
    let mut positions = self.moons.clone();
    let mut velocities = [[0; 3]; 4];

    self.apply_gravity(&positions, &mut velocities);
    self.apply_velocity(&mut positions, &velocities);
    let mut i = 1;

    while velocities != [[0; 3]; 4] {
      self.apply_gravity(&positions, &mut velocities);
      self.apply_velocity(&mut positions, &velocities);
      i +=1;
    }

    return i;
  }

  fn run1(&self) -> i32 {
    let (pos, vel) = self.moons_after_steps(1000);
    return self.get_energy(&pos, &vel);
  }

  fn run2(&self) -> u64 {
    return self.steps_to_repeat_state();
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn part1_example1()
  {
    let day = Day12::new("<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>");

    let (pos, vec) = day.moons_after_steps(10);
    let energy = day.get_energy(&pos, &vec);

    assert_eq!(179, energy);
  }

  const EXAMPLE2: &str = "<x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>";

  #[test]
  fn part1_example2()
  {
    let day = Day12::new(EXAMPLE2);

    let (pos, vec) = day.moons_after_steps(100);
    let energy = day.get_energy(&pos, &vec);

    assert_eq!(1940, energy);
  }
  
  // #[test]
  // fn part2_example2()
  // {
  //   let day = Day12::new(EXAMPLE2);

  //   let steps = day.steps_to_repeat_state();

  //   assert_eq!(4686774924, steps);
  // }
}