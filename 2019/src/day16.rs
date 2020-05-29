extern crate rayon;

use self::rayon::prelude::*;
use super::Day;

pub struct Day16 {
  input: Vec<u32>
}

impl Day for Day16 {
  fn new(input: &str) -> Day16 {
    Day16 {
      input: input.chars().map(|c| c.to_digit(10).unwrap() as u32).collect(),
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
struct TakeSchema {
  take: usize,
  start: usize,
}

// -- Privates
impl Day16 {
  fn generate_take_schemes(&self, repeat: usize) -> Vec<TakeSchema> {
    (0..self.input.len()*repeat).map(|v| TakeSchema {
      take: 1 + v,
      start: v,
    }).collect()
  }

  fn get_last(&self, val: i32) -> u32 {
    val.abs() as u32 % 10
  }

  fn fft_100_times(&self, signal_repetition: u32) -> [Vec<u32>; 2] {
    let schemes = self.generate_take_schemes(signal_repetition as usize);

    let mut lists: [Vec<u32>; 2] = [self.input.clone().repeat(signal_repetition as usize), vec![]];

    for i in 0..100 {
      lists[(i+1)%2] = schemes
        .par_iter()
        .map(|schema| {
          let mut negate: i32 = 1;
          let mut sum: i32 = 0;
  
          let (_, relevant) = lists[i%2].split_at(schema.start);
          let chunks = relevant.chunks(schema.take*2);
  
          for chunk in chunks {
            sum += negate * chunk.iter().take(schema.take).sum::<u32>() as i32;
            negate *= -1;
          }
  
          self.get_last(sum)
      }).collect();
    }

    return lists;
  }

  fn get_answer(&self, vec: &Vec<u32>, take: u32, skip: usize) -> u32 {
    vec.iter().skip(skip).take(take as usize).enumerate().map(|(ind, v)| *v * (10_u32.pow((take - 1) - ind as u32))).sum()
  }

  fn run1(&self) -> u32 {
    let ans = &self.fft_100_times(1)[0];
    self.get_answer(&ans, 8, 0)
  }

  fn run2(&self) -> u32 {
    let message_padding = self.get_answer(&self.input, 7, 0) as usize;

    let ans = &self.fft_100_times(1)[0];
    println!("{:?}", message_padding);
    println!("{:?}", ans);
    self.get_answer(&ans, 8, message_padding % self.input.len())
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn part1() {
    let day = Day16::new(include_str!("../inputs/day16.txt"));
    assert_eq!(day.run1(), 85726502);
  }

  #[test]
  fn part1_example1() {
    let day = Day16::new("80871224585914546619083218645595");
    assert_eq!(day.run1(), 24176176);
  }

  #[test]
  fn part2() {
    let day = Day16::new(include_str!("../inputs/day16.txt"));
    assert_eq!(day.run2(), 0);
  }

  #[test]
  fn part2_example1() {
    let day = Day16::new("03036732577212944063491565474664");
    assert_eq!(day.run2(), 84462026);
  }

}